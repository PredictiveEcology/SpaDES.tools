rastDF <- data.frame(pkg = c("raster", "terra"),
                     class = c("Raster", "SpatRaster"),
                     read = c("raster::raster", "terra::rast"),
                     stack = c("raster::stack", "terra::rast"),
                     stackClass = c("RasterStack", "SpatRaster"),
                     extent = c("raster::extent", "terra::ext"))

needTerraAndRaster <- function(envir = parent.frame()) {
  if (!requireNamespace("raster", quietly = TRUE)) {
    rastDF <- rastDF[rastDF$pkg == "terra", ]
  }
  return(rastDF)
}

# puts tmpdir, tmpCache, opts, optsDebug in this environment,
# loads and libraries indicated plus testthat,
# puts tmpdir, tmpCache, tmpfile (can be vectorized with length >1 tmpFileExt),
#   optsAsk in this environment,
# loads and libraries indicated plus testthat,
# sets options("reproducible.ask" = FALSE) if ask = FALSE
testInit <- function(libraries = character(), ask = FALSE, verbose,
                     tmpFileExt = "",
                     opts = NULL, needGoogleDriveAuth = FALSE
                     ) {

  reproducible::set.randomseed()

  pf <- parent.frame()

  if (isTRUE(needGoogleDriveAuth))
    libraries <- c(libraries)
  if (length(libraries)) {
    libraries <- unique(libraries)
    loadedAlready <- vapply(libraries, function(pkg)
      any(grepl(paste0("package:", pkg), search())), FUN.VALUE = logical(1))
    libraries <- libraries[!loadedAlready]

    if (length(libraries)) {
      pkgsLoaded <- unlist(lapply(libraries, requireNamespace, quietly = TRUE))
      if (!all(pkgsLoaded)) {
        lapply(libraries[!pkgsLoaded], skip_if_not_installed)
      }
      suppressWarnings(lapply(libraries, withr::local_package, .local_envir = pf))
    }
  }


  # skip_gauth <- identical(Sys.getenv("SKIP_GAUTH"), "true") # only set in setup.R for covr
  # if (isTRUE(needGoogleDriveAuth) ) {
  #   if (!skip_gauth) {
  #     if (interactive()) {
  #       if (!googledrive::drive_has_token()) {
  #         getAuth <- FALSE
  #         if (is.null(getOption("gargle_oauth_email"))) {
  #           possLocalCache <- "c:/Eliot/.secret"
  #           cache <- if (file.exists(possLocalCache))
  #             possLocalCache else TRUE
  #           switch(Sys.info()["user"],
  #                  emcintir = {options(gargle_oauth_email = "eliotmcintire@gmail.com",
  #                                      gargle_oauth_cache = cache)},
  #                  NULL)
  #         }
  #         if (is.null(getOption("gargle_oauth_email"))) {
  #           if (.isRstudioServer()) {
  #             .requireNamespace("httr", stopOnFALSE = TRUE)
  #             options(httr_oob_default = TRUE)
  #           }
  #         }
  #         getAuth <- TRUE
  #         if (isTRUE(getAuth))
  #           googledrive::drive_auth()
  #       }
  #     }
  #   }
  #   skip_if_no_token()
  # }

  out <- list()
  withr::local_options("reproducible.ask" = ask, .local_envir = pf)
  # withr::local_options("spades.debug" = debug, .local_envir = pf)
  # withr::local_options("spades.moduleCodeChecks" = smcc, .local_envir = pf)
  withr::local_options("spades.recoveryMode" = FALSE, .local_envir = pf)
  withr::local_options("reproducible.verbose" = FALSE, .local_envir = pf)
  withr::local_options("spades.useRequire" = FALSE, .local_envir = pf)
  withr::local_options("spades.sessionInfo" = FALSE, .local_envir = pf)

  if (!missing(verbose))
    withr::local_options("reproducible.verbose" = verbose, .local_envir = pf)
  if (!is.null(opts))
    withr::local_options(opts, .local_envir = pf)
  tmpdir <- reproducible::normPath(withr::local_tempdir(tmpdir = reproducible::tempdir2(), .local_envir = pf))
  tmpCache <- reproducible::normPath(withr::local_tempdir(tmpdir = tmpdir, .local_envir = pf))
  if (isTRUE(any(nzchar(tmpFileExt)))) {
    dotStart <- startsWith(tmpFileExt, ".")
    if (any(!dotStart))
      tmpFileExt[!dotStart] <- paste0(".", tmpFileExt)
    out$tmpfile <- reproducible::normPath(withr::local_tempfile(tmpdir = tmpdir, fileext = tmpFileExt))
  }
  withr::local_dir(tmpdir, .local_envir = pf)

  out <- append(out, list(tmpdir = tmpdir, tmpCache = tmpCache))
  list2env(out, envir = pf)
  return(invisible(out))
}

