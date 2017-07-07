#' @section 7 Package options:
#'
#' \code{SpaDES} packages use the following \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{spades.lowMemory}: If true, some functions will use more memory
#'     efficient (but slower) algorithms. Default \code{FALSE}.
#' }
#'
#' @rdname SpaDES.tools-package
#'
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  tmpdir <- file.path(tempdir(), "SpaDES")
  ## set options using the approach used by devtools
  opts <- options()
  opts.spades <- list( # nolint
    spades.lowMemory = FALSE
  )
  toset <- !(names(opts.spades) %in% names(opts))
  if (any(toset)) options(opts.spades[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Using SpaDES.tools version ", packageVersion("SpaDES.tools"), ".")
  }
}
