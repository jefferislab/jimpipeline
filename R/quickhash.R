#' Compute a quick hash for head and tail of a Zeiss LSM confocal file
#' 
#' The first ~ 150,000 bytes includes a number of fields such as dimension and
#' acquisition timestamp that are typically enough to make for a unique hash. In
#' particular the first 150K (but not 100K) are unique across the 16226 v2
#' flycircuit LSMs. The hope is the last 100K should include some image data
#' (though in practice for flycrircuit LSMs) this is often in the 8 bit
#' segmented neurons which nearly all have 0 background
#' @param f Path to lsm files
#' @param offsets Default c(0,-1e5)
#' @param chunksizes Default c(1e5,1e5)
#' @param ... Additional arguments passed to default \code{\link{quickhash}} function
#' @return character vector containing hash values for lsm(s)
#' @export
#' @seealso \code{\link{quickhash}} and \code{\link{digest}}
quickhash.lsm<-function(f,offsets=c(0,-1e5),chunksizes=c(1.5e5,1e5),...){
  quickhash(f,offsets=offsets,chunksizes=chunksizes,...)
}

#' Compute a (quick) hash for some part of the contents of a set of files
#' 
#' Default is to compute md5 hash for whole of each file. negative offsets => 
#' seek to offset position wrt end of file \code{chunksize=Inf} => whole file. 
#' Uses digest() which defaults to algo="md5"
#' @param f Path to files to hash
#' @param offsets integer offsets into file at which to read data
#' @param chunksizes amount of data to read at each offset
#' @param CheckFileSize see if the file has enough data to match the 
#'   requirements defined by \code{offsets} and \code{chunksizes} and compute a 
#'   digest of the entire file when there is insufficient data. The default 
#'   value of \code{TRUE} is strongly recommended.
#' @param ... Additional arguments passed to the \code{\link{digest}} function.
#' @return character vector containing hash values for file(s)
#' @export
#' @seealso \code{\link{digest}}, \code{\link{quickhash.lsm}}
#' @import digest
#' @importFrom utils file_test
quickhash<-function(f,offsets=0,chunksizes=Inf,CheckFileSize=TRUE,...){
	if(length(f)>1)
		return(sapply(f,quickhash,offsets=offsets,chunksizes=chunksizes,CheckFileSize=CheckFileSize,...))
	if(!file_test('-f',f)){
		warning("Cannot read file: ",f)
		return (NA_character_)
	}
	if(length(offsets)!=length(chunksizes))
		stop("Must specify the same number of offsets (",length(offsets),
			") and chunk sizes (",length(chunksizes),")")
	# hash whole file, if smaller than total chunk sizes requested 
  # FIXME also need to consider offsets as well
	mindatatohash=sum(chunksizes)
	if(!is.finite(mindatatohash) || 
		(CheckFileSize && file.info(f)$size<=mindatatohash))
		return(digest(f,file=T,...))
	
	con=file(f,open='rb')
	on.exit(close(con))
	d=raw()
	for(i in seq(along=offsets)){
		seek(con,where=offsets[i],origin=ifelse(offsets[i]>=0,"start","end"))
		d=c(d,readBin(con,what=raw(),n=chunksizes[i]))
	}
	digest(d,serialize=FALSE,...)
}
