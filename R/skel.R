#' Skeletonise NRRD images (e.g. to make them suitable for NBLAST)
#'
#' @param input Path to single input file or an input directory
#' @param outdir Path to output directory
#' @param threshold Integer specifying an absolute threshold or string
#'   specifying ImageJ authothreshold method.
#' @param fileregex Optional regex matching the \bold{whole} of the filename (NB
#'   just filename without parent directory)
#' @inheritParams convertlsmstonrrd
#'
#' @return Logical indicating if the command completed successfully
#' @export
#'
#' @examples
#' \dontrun{
#' # basic version
#' skeletonise_nrrds('/path/to/indir', '/path/to/outdir')
#' # convert those to dotrops for NBLAST
#' dps=dotprops(dir('/path/to/outdir', pattern='nrrd$', full.names=TRUE))
#' 
#' # additional arguments
#' skeletonise_nrrds('/path/to/indir', '/path/to/outdir', threshold='Iso_Data')
#' skeletonise_nrrds('/path/to/indir', '/path/to/outdir', fileregex='^seg_whole.*c0\\.nrrd')
#' }
skeletonise_nrrds <- function(input, outdir, threshold=0, fileregex=NULL, DryRun=TRUE, ...) {
  skel=system.file('ijm','skeletonize_nrrds.txt', package = 'jimpipeline')

  outdir <- normalizePath(outdir, mustWork = FALSE)
  if(!file.exists(outdir)) {
    message('Creating output dir: ', outdir)
    dir.create(outdir, recursive = TRUE)
  }
  
  ma=paste(normalizePath(input, mustWork = TRUE), 
           outdir,
           threshold, sep=",")
  if(!is.null(fileregex))
    ma=paste(ma, fileregex, sep=",")
  
  runFijiMacro(
    macro=skel,
    macroArg=ma,
    # javaArgs="-noverify",
    DryRun=DryRun, ...)  
}

