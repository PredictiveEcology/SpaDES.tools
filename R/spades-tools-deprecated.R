#' \code{SpaDES.tools} deprecated functions
#'
#' @param name A character string giving the name/path of a file.
#'
smallNamify <- function(name) {
 .Deprecated(".prefix")
  .prefix(name, prefix = "Small")
}
