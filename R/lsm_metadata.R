#'Extract the metadata from Zeiss LSM file using loci bioformats
#'
#'Specifically, this relies on the showinf tool of loci Details
#'@param f Path to lsm file
#'@param cachefile Whether to save a copy of metdata to disk (TRUE)
#'@param ReturnMetaData Whether to return metadata rather than success (default:
#'  \code{TRUE})
#'@param Force whether to re-parse metdata even if a cached version exists
#'@param UseLock whether to use a lock file while parsing for simple parallelism
#'@return character vector of metadata OR TRUE/FALSE for success
#'@export
#'@importFrom nat.utils RunCmdForNewerInput makelock
#'@seealso \code{\link{parseKeyLSMMetadata}}
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
