#' Uncompress a file with bunzip2
#' @param f The file to uncompress
#' @param keep Whether to keep the compressed file
#' @return Path to the uncompressed file
#' @export
bunzip2 <- function(f, keep=TRUE) {
  args=shQuote(path.expand(f))
  if (keep) args=c("--keep", args)
  bunzip2cmd=Sys.which('bunzip2')
  if(!nzchar(bunzip2cmd)) stop("Unable to find bunzip2!")
  system2(bunzip2cmd, args = args)
  invisible(tools::file_path_sans_ext(f))
}
