#' Package to wrap Jefferis lab image processing pipeline
#' 
#' This contains functions that we use during our large scale image processing 
#' and registration work. We will try to make these as generic as possible, but 
#' for the moment use by third parties is not a major design consideration.
#' @section Package Options: The following options can be set to specify default
#'   behaviour.
#'   
#'   \itemize{
#'   
#'   \item{\code{jimpipeline.fiji}}{ Location of fiji to use for running 
#'   scripts}
#'   
#'   If this is not set, at the moment the package will check the system path
#'   for an executable called \code{fiji}.
#'   
#'   }
#' @name jimpipeline-package
#' @aliases jimpipeline
#' @docType package
#' @keywords package
#' @import bftools
NULL
