
#' Call external Fiji/ImageJ executable
#' 
#' @description \code{runFijiMacro} runs a macro using preferred Fiji executable
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
#' @return logical indicating if the command completed successfully
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
  return(0==system(cmd))
}

 
#' @description \code{fiji} returns path to preferred Fiji executable
#' @rdname runFijiMacro
#' @export
#' @examples 
#' # Path to current Fiji executable
#' \donttest{
#' fiji()
#' }
#' 
#' \dontrun{
#' # Set path to preferred Fiji executable (this will be remembered)
#' # you can also set options(jimpipeline.fiji="/some/path")
#' fiji("/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx")
#' }
fiji <- function(fijiPath=NULL) {
  if(!is.null(fijiPath)) {
    if(!file.exists(fijiPath)) 
      stop("fiji is not at: ", fijiPath)
  } else {
    # do we have an option set?
    fijiPath=getOption('jimpipeline.fiji')
    if(!is.null(fijiPath)) {
      if(!file.exists(fijiPath))
        stop("fiji is not at: ", fijiPath, " as specified by options('jimpipeline.fiji')!")
    } else {
      # look for it in sensible places
      if(!nzchar(fijiPath <- Sys.which('fiji'))) {
        macapp="/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx"
        if(file.exists(macapp))
          fijiPath=macapp
        else 
          stop("Unable to find fiji!",
               "Set options('jimpipeline.fiji') to point to the fiji command line executable!")
      }
    }
  }
  options(jimpipeline.fiji=fijiPath)
  normalizePath(fijiPath)
}
