## -------------------------------------------------------------------------- ##
## edit the package options help documentation in spades-tools-package.R      ##
## -------------------------------------------------------------------------- ##

.onLoad <- function(libname, pkgname) {

  elapsedRatio <- Sys.getenv("_R_CHECK_EXAMPLE_TIMING_CPU_TO_ELAPSED_THRESHOLD_")
  if (nzchar(elapsedRatio)) {
    # CRAN allows use of up to 2 CPUs when OMP_NUM_THREADS is not
    # set, but for some reason some examples finish too fast as if
    # we're using more than 1 CPUs:
    #
    # "Examples with CPU time > 2.5 times elapsed time"
    #
    # To work around this issue, never use more than 1 CPU when
    # OMP_NUM_THREADS is not set && _R_CHECK_EXAMPLE_TIMING_CPU_TO_ELAPSED_THRESHOLD_ is set.
    options("spades.OMP_NUM_THREADS" = Sys.getenv("OMP_NUM_THREADS"))
    options("spades.OMP_THREAD_LIMIT" = Sys.getenv("OMP_THREAD_LIMIT"))
    Sys.setenv("OMP_NUM_THREADS"=1L)
    Sys.setenv("OMP_THREAD_LIMIT"=1L)
  }

  ## set options using the approach used by devtools
  opts <- options()
  opts.spades <- list( # nolint
    spades.lowMemory = FALSE
  )
  toset <- !(names(opts.spades) %in% names(opts))
  if (any(toset)) options(opts.spades[toset])

  ## import functions using backports:
  backports::import(pkgname, "isFALSE")

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Using SpaDES.tools version ", packageVersion("SpaDES.tools"), ".")
  }
}

.onDetach <- function(libname, pkgname) {
  if (!is.null(getOption("spades.OMP_THREAD_LIMIT"))) {
    Sys.setenv("OMP_THREAD_LIMIT" = getOption("spades.OMP_THREAD_LIMIT"))
    Sys.setenv("OMP_NUM_THREADS" = getOption("spades.OMP_NUM_THREADS"))
  }
}
