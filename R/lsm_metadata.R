#'Extract the metadata from Zeiss LSM file using loci bioformats as text vector
#'
#'Specifically, this relies on the showinf tool of loci Details
#'@param f Path to lsm file
#'@param cachefile Whether to save a copy of metadata to disk (TRUE)
#'@param ReturnMetaData Whether to return metadata rather than success (default:
#'  \code{TRUE})
#'@param Force whether to re-parse metadata even if a cached version exists
#'@param UseLock whether to use a lock file while parsing for simple parallelism
#'@return character vector of metadata OR TRUE/FALSE for success
#'@export
#'@importFrom nat.utils RunCmdForNewerInput makelock
#'@seealso \code{\link{parse_key_lsm_metadata}}
#' @examples \dontrun{
#' lsmdir=file.path(fcconfig$regroot,"lsms")
#' for(f in dir(lsmdir,patt="lsm$",full=T)) lsm_metadata(f,UseLock=TRUE)
#'}
lsm_metadata<-function(f,cachefile=TRUE,
                       ReturnMetaData=TRUE,Force=FALSE,UseLock=FALSE){
  
  if(!file.exists(f)) {
    warning("File: ",f," is missing")
    return(FALSE)
  }
  
  if(tools::file_ext(f)=="bz2"){
    # we need to uncompress first
    f=bunzip2(f)
  }
  
  if(is.logical(cachefile)) {
    if(cachefile) {
      cachefile=sub("\\.lsm$",".txt",f)
      stdout=cachefile
    } else stdout=TRUE
  } else if(is.function(cachefile)){
    stdout <- cachefile <- cachefile(f)
  } else stdout=cachefile
  
  if(is.character(cachefile) && file.exists(cachefile)){
    if(!Force && !RunCmdForNewerInput(NULL,f,cachefile)){
      if(ReturnMetaData) return(readLines(cachefile))
      else return(TRUE)
    }
  }
  
  if(is.character(stdout) && UseLock){
    lockfile=paste(stdout,sep=".","lock")
    if(!makelock(lockfile)) return (FALSE)
    on.exit(unlink(lockfile))
  }
  # cached file is older, or missing, or we used force
  rval=showinf(f, outfile=ifelse(is.character(cachefile), cachefile, FALSE))
  
  if(is.numeric(rval) && rval>0) {
    warning("showinf error for file: ",f)
    if(is.character(stdout)) unlink(stdout)
    return(FALSE)
  }
  if(ReturnMetaData) {
    if (is.character(stdout)) invisible(readLines(stdout))
    else invisible(rval)
  }
  else return(TRUE)
}

LoadObjsFromRda<-function(f){
  objnames=load(f,envir=environment())
  return(lapply(objnames,get,envir=environment()))
}

#' Make summary dataframe for lsm files and their key metadata
#'
#' @details Will calculate md5 sums for all input files and then use this to
#'   update a dataframe.
#'
#'   If an extrafields function is supplied, it should take one parameter, the
#'   list output of \code{\link{parse_key_lsm_metadata}}, which will include as
#'   an attribute the full contents of the metadata file dumped by the loci
#'   showinf tool. The function should take care to return values for all fields
#'   that it \emph{could} return each time it is called even if these are NA and
#'   to ensure that values have a consistent data type per field.
#' @param lsmdir Path to directory containing lsm files
#' @param oldlsmdf Dataframe or path to rda file containing one
#' @param extrafields Function to parse extra fields from metadata for each
#'   image
#' @param Verbose Logical: Show messages about progress/updating of cached data
#' @return dataframe
#' @export
#' @seealso \code{\link{lsm_metadata}}, \code{\link{parse_key_lsm_metadata}}
make_lsm_df<-function(lsmdir,oldlsmdf=NULL,extrafields=NULL,Verbose=TRUE){
  lsmdf=data.frame(txtfile=dir(lsmdir,pattern = "txt$"),stringsAsFactors=FALSE)
  lsmdf$lsmfile=sub("txt$","lsm",lsmdf$txtfile)
  lsmdf$gene_name=sub(".txt","",lsmdf$txtfile,fixed=TRUE)
  rownames(lsmdf)=lsmdf$gene_name
  lsmdf$txtmtime=file.info(file.path(lsmdir,lsmdf$txtfile))$mtime
  lsmfi=file.info(file.path(lsmdir,lsmdf$lsmfile))
  lsmdf$lsmsize=lsmfi$size
  lsmdf$lsmmtime=lsmfi$mtime
  
  if(Verbose) message("Computing md5 sums for ",nrow(lsmdf),' metadata files.\n')
  lsmdf$txtmd5=tools::md5sum(file.path(lsmdir,lsmdf$txtfile))
  
  if(nrow(lsmdf)==0) return(lsmdf)
  
  fieldsToCompute=c('lsmqhash',"Ch1","Ch2","ChannelName0","swapchannels","DimensionX", "DimensionY",
                    "DimensionZ", "VoxelSizeX", "VoxelSizeY","VoxelSizeZ")
  lsmdf[,fieldsToCompute]=NA
  
  # can accept a path to cached dataframe, if so read it in
  if(is.character(oldlsmdf)){
    if(!file.exists(oldlsmdf)){
      warning("Unable to read cached dataframe from: ", oldlsmdf)
      oldlsmdf=NULL
    } else {
      if(Verbose) message("Loading cached dataframe")
      oldlsmdf=LoadObjsFromRda(oldlsmdf)[[1]]
    }
  }
  
  if(!is.null(oldlsmdf)){
    # we're going to use a cached version of the lsmdf table
    # first drop anything that's not unique by md5
    
    oldlsmdf=oldlsmdf[!duplicated(oldlsmdf$txtmd5) & !is.na(oldlsmdf$txtmd5), , drop=FALSE]
    # then use the md5 for rownames
    rownames(oldlsmdf)=oldlsmdf$txtmd5
    
    # figure out for which rows we have cached data
    gene_names_withmd5match=lsmdf[lsmdf$txtmd5%in%oldlsmdf$txtmd5,"gene_name"]
    # and the one we will need to parse from scratch
    gene_names_toparse=setdiff(lsmdf$gene_name,gene_names_withmd5match)
    
    # now add matching data
    fieldstocopy=intersect(fieldsToCompute,colnames(oldlsmdf))
    # using md5s to look up data from cached dataframe
    lsmdf[gene_names_withmd5match,fieldstocopy]=
      oldlsmdf[lsmdf[gene_names_withmd5match,'txtmd5'],fieldstocopy]
  } else {
    # parse all rows from scratch
    gene_names_toparse=lsmdf$gene_name
  }
  if(Verbose) message("Parsing ",length(gene_names_toparse)," metadata files from scratch")
  for (g in gene_names_toparse){
    p=try(parse_key_lsm_metadata(file.path(lsmdir,lsmdf[g,"txtfile"]),
                              ReturnRawMetaData=TRUE))
    if(inherits(p,"try-error")) {
      warning("Error parsing metadata for gene_name: ",g)
      next
    }
    lsmdf[g,c("DimensionX", "DimensionY", "DimensionZ", "VoxelSizeX", "VoxelSizeY",
              "VoxelSizeZ")]=p[[1]]
    lsmdf[g,"Ch1"]=p[[2]][1]
    lsmdf[g,"Ch2"]=p[[2]][2]
    lsmdf[g,"ChannelName0"]=p[[2]][3]
    lsmdf[g,"lsmqhash"]=quickhash.lsm(file.path(lsmdir,lsmdf[g,"lsmfile"]))
    # will need to swap channels if brain is not channel 1
    # TODO - figure out a way to provide this - pass in a user function?
    # lsmdf[g,"swapchannels"] = FCLSMBrainChannel(p)!=1
    if(!is.null(extrafields)){
      xi=extrafields(attr(p,'rawmd'))
      missing_cols=setdiff(names(xi),colnames(lsmdf))
      lsmdf[,missing_cols]=NA
      lsmdf[g,names(xi)]=xi
    }
  }
  return(lsmdf)
}

#' Parse key Zeiss LSM metadata into an R list
#'
#' @param f Path to file containing lsm metadata
#' @param text Text version of lsm metadata (optional, otherwise read from file)
#' @param ReturnRawMetaData Whether to return the raw metadata from the file.
#'
#' @return a list containing parsed metadata
#' @export
#' @importFrom tools file_ext
#' @seealso \code{\link{lsm_metadata}}
parse_key_lsm_metadata<-function(f,text=NULL,ReturnRawMetaData=FALSE){
  ext=file_ext(f)
  if(ext=="lsm") {
    text=lsm_metadata(f = f)
  }
  ll <- if(!is.null(text)) text else readLines(f)
  
  chans=vector("character",length=2)
  ch1=sub(".*Name: (.*)","\\1",
          grep("DataChannel Name #1",ll,fixed=T,value = T))
  chans[1]=ifelse(length(ch1)>0,ch1, NA)
  
  ch2=sub(".*Name: (.*)","\\1",
          grep("DataChannel Name #2",ll,fixed=T,value=T))
  chans[2]=ifelse(length(ch2)>0,ch2, NA)
  chnm0=grep("ChannelName0",ll,fixed=T,value=T)
  if(length(chnm0))
    chans[3]=sub("ChannelName0: ","",chans[3])
  else chans[3]=NA_character_
  names(chans)=c("Chan1Name","Chan2Name","ChannelName0")
  
  # TODO: add Pixel type = uint16
  
  selected_lines=grep("Dimension([XYZ]|Channels)",ll,value=T)
  selected_lines=c(selected_lines,grep("VoxelSize[XYZ]",ll,value=T))
  
  parse_values <- function(lines){
    valueslist=strsplit(lines, ": ")
    values_str=sapply(valueslist,"[[",2)
    values=suppressWarnings(as.numeric(values_str))
    if(any(is.na(values)))
      values=values_str
    names(values)=sapply(valueslist,"[[",1)
    values
  }
  dimvalues=parse_values(selected_lines)
  if(length(dimvalues)!=7) stop("Error retrieving Dimension metadata for file:",f)
  
  lens_lines=grep("(Recording Objective|Zoom X)", ll, value=T)
  lensvalues=parse_values(lens_lines)
  
  timestamp=parse_values(grep("Sample 0Time", ll, value = T))
  timestamp=ISOdatetime(1899,12,30,0,0,0)+timestamp[[1]]*60*60*24
  
  bits=parse_values(grep("Bits Per Sample", ll, value = T))
  
  structure(list(dim=dimvalues,chan=chans, lens=lensvalues, timestamp=timestamp, bits=bits),file=f,
            rawmd=if(ReturnRawMetaData) ll else NULL)
}
