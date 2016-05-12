
#' Run a Fiji macro
#' 
#' @param macro Path to macro to run
#' @param macroArg Arguments for macro
#' @param headless Whether to have ImageJ/Fiji running headless
#' @param batch Use batch mode
#' @param MinMem,MaxMem Memory limits
#' @param IncrementalGC Whether to use incremental garbage collection
#' @param Threads Number of threads
#' @param fijiArgs Arguments for ImageJ/Fiji (as opposed to the macro itself)
#' @param javaArgs Arguments for java
#' @param ijArgs Arguments for ImageJ
#' @param fijiPath Path to fiji executable (can be set by
#'   \code{options(jimpipeline.fiji="/some/path")})
#' @param DryRun Whether to return the command to be run rather than actually 
#'   executing it.
#' @export
runFijiMacro <- function(macro="",macroArg="",headless=FALSE,batch=TRUE,
                         MinMem=MaxMem,MaxMem="2500m",IncrementalGC=TRUE,Threads=NULL,
                         fijiArgs=NULL,javaArgs=NULL,ijArgs=NULL,
                         fijiPath=fiji(),DryRun=FALSE){
  
  if(headless) fijiArgs = c(fijiArgs,"--headless")
  fijiArgs=paste(fijiArgs,collapse=" ")
  
  javaArgs=c(paste("-Xms",MinMem,sep=""),paste("-Xmx",MaxMem,sep=""),javaArgs)
  if(IncrementalGC) javaArgs=c(javaArgs,"-Xincgc")
  javaArgs=paste(javaArgs,collapse=" ")
  
  threadAdjust=ifelse(is.null(Threads),"",paste("run(\"Memory & Threads...\", \"parallel=",Threads,"\");",sep=""))
  
  macroCall=paste(" -eval '",threadAdjust,"runMacro(\"", macro,"\",\"",macroArg,"\");' ",sep="")
  
  ijArgs=paste(c(ijArgs,ifelse(batch,"-batch","")),collapse=" ")
  
  cmd<-paste(fijiPath,javaArgs,fijiArgs,"--",macroCall,ijArgs)
  if(DryRun) return(cmd)
  system(cmd)
}

 
# Private function to eturn path to preferred fiji
fiji <- function(fijipath=NULL) {
  if(!is.null(fijipath)) {
    if(!file.exists(fijipath)) stop("fiji is not at: ", fijipath)
    options(jimpipeline.fiji=fijipath)
  } else {
    # do we have an option set?
    fijipath=getOption('jimpipeline.fiji')
    if(!is.null(fijipath)) {
      if(!file.exists(fijipath))
        stop("fiji is not at: ", fijipath, " as specified by options('jimpipeline.fiji')!")
      else {
        options(jimpipeline.fiji=NULL)
      }
    } else {
      # look for it in sensible places
      if(nzchar(fijipath <- Sys.which('fiji'))){
        options(jimpipeline.fiji=fijipath)
      }
      else stop("Unable to find fiji!")
    }
  }
  fijipath
}
