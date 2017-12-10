#' Split Zeiss LSMs and save each channel to a nrrd
#'
#' image.lsm -> image_01.nrrd image_02.nrrd image_03.nrrd
#'
#' ChannelOrder specifies the output stem of the channels in the input image.
#' Thus \code{c(2,3,1)} would imply:
#'
#' image.lsm[1:3] -> image_02.nrrd image_03.nrrd image_01.nrrd
#'
#' \code{lsmstoconvert} uses Fiji and lsmtonrrd macro, which in turn relies on
#' LOCI Bio-Formats which cannot currently be run headless.
#'
#' Uses RunCmdForNewerInput to compare in/outputs to see if action required.
#' Uses file locking so can be run in parallel, although IO intensive so running
#' too many jobs in parallel would be counter-productive. RandomOrder=TRUE
#' reduces file locking pressure when several processes compete to lock the same
#' file.
#' @param lsmstoconvert Paths to one or more LSMs or a single directory
#'   containing LSMs
#' @param rawnrrdsdir Output directory in which to save nrrds
#' @param ChannelOrder The order in which channels of each individual image are
#'   saved (defaults to swapping the order of first 2 channels - see details).
#' @param RandomOrder Scramble processing order of files (default TRUE)
#' @param DryRun Show what would happen but don't run (default TRUE)
#' @param Verbose Show filename (default !DryRun)
#' @param ... Additional arguments passed to \code{\link{runFijiMacro}}
#' @return Named logical vector indicating whether action was required for each
#'   file
#' @export
#' @seealso \code{\link{runFijiMacro}}, \code{\link{RunCmdForNewerInput}}
#' @examples
#' \dontrun{
#' convertlsmstonrrd("my.lsm", ChannelOrder=1:3, DryRun=FALSE)
#' }
#'
#' # Path to the Fiji macro distributed with the package
#' system.file('ijm','lsmtonrrd.txt', package = 'jimpipeline')
convertlsmstonrrd<-function(lsmstoconvert, rawnrrdsdir, ChannelOrder=c(2,1,3,4),
  RandomOrder=TRUE,DryRun=TRUE,Verbose=!DryRun, ...){
  
  if(!file.exists(rawnrrdsdir)) dir.create(rawnrrdsdir)
  
  if(length(lsmstoconvert)==1 && file.info(lsmstoconvert)$isdir){
    lsmstoconvert=dir(lsmstoconvert,pattern = "lsm$",full.names = T)  
  }
  if(RandomOrder) lsmstoconvert <- sample(lsmstoconvert)
  runsummary=logical(length(lsmstoconvert))
  names(runsummary)=basename(lsmstoconvert)
  
  missingChannels=setdiff(seq_len(max(ChannelOrder)), ChannelOrder)
  if(length(missingChannels)) {
    stop("You have not specified the channel order for channels: ", 
         paste(missingChannels, collapse = ', '))
  }
  # c(2,1,3,4) -> "02010304" for ImageJ
  ChannelOrder=paste0("0", ChannelOrder, collapse = "")
  
  for(i in seq(along=lsmstoconvert)){
    f=lsmstoconvert[i]
    cmd=runFijiMacro(
      macro=system.file('ijm','lsmtonrrd.txt', package = 'jimpipeline'),
      macroArg=paste(f, rawnrrdsdir, 'nrrd', ChannelOrder, sep=","),
      # javaArgs="-noverify",
      headless=TRUE, DryRun=TRUE, ...)
    gene_name=sub("\\.lsm","",basename(f))
    nrrds=file.path(rawnrrdsdir,paste(gene_name,"_0",1:2,".nrrd",sep=""))
    # either run cmd, or say what would have happened
    runsummary[i]=RunCmdForNewerInput(ifelse(DryRun,NA,cmd),infiles=f,
                                      outfiles=nrrds,UseLock=TRUE)
    if(Verbose)
      cat(ifelse(runsummary[i],"Processed","Skipped"),f,'\n')
    else {
      cat(ifelse(runsummary[i],"+","."))
      if((i%%100)==0) cat(" i = ",i,"\n",sep="")
    }
  }
  if(!Verbose && (i%%100)!=0) cat("\n") # Just to finish cleanly
  runsummary
}
