# puts tmpdir, tmpCache, tmpfile (can be vectorized with length >1 tmpFileExt),
#   optsAsk in this environment,
# loads and libraries indicated plus testthat,
# sets options("reproducible.ask" = FALSE) if ask = FALSE
testInit <- function(libraries, ask = FALSE, verbose = FALSE, tmpFileExt = "",
                     opts = NULL, needGoogle = FALSE) {

  optsAsk <- if (!ask)
    options("reproducible.ask" = ask)
  else
    list()

  optsVerbose <- if (verbose)
    options(reproducible.verbose = verbose)
  else
    list()

  if (missing(libraries)) libraries <- list()
  unlist(lapply(libraries, require, character.only = TRUE))
  require("testthat")
  tmpdir <- reproducible:::tempdir2(reproducible:::rndstr(1, 6))

  if (isTRUE(needGoogle)) {
    if (utils::packageVersion("googledrive") >= "1.0.0")
      googledrive::drive_deauth()
    else
      googledrive::drive_auth_config(active = TRUE)

    if (quickPlot::isRstudioServer()) {
      options(httr_oob_default = TRUE)
    }

    ## #119 changed use of .httr-oauth (i.e., no longer used)
    ## instead, uses ~/.R/gargle/gargle-oauth/long_random_token_name_with_email
    if (interactive()) {
      if (utils::packageVersion("googledrive") >= "1.0.0") {
        googledrive::drive_deauth()
      } else {
        if (file.exists("~/.httr-oauth")) {
          linkOrCopy("~/.httr-oauth", to = file.path(tmpdir, ".httr-oauth"))
        } else {
          googledrive::drive_auth()
          print("copying .httr-oauth to ~/.httr-oauth")
          file.copy(".httr-oauth", "~/.httr-oauth", overwrite = TRUE)
        }

        if (!file.exists("~/.httr-oauth"))
          message("Please put an .httr-oauth file in your ~ directory")
      }
    }
  }
  checkPath(tmpdir, create = TRUE)
  origDir <- setwd(tmpdir)
  tmpCache <- reproducible::normPath(file.path(tmpdir, "testCache"))
  checkPath(tmpCache, create = TRUE)

  defaultOpts <- list(reproducible.showSimilar = FALSE,
                      reproducible.overwrite = TRUE,
                      reproducible.useNewDigestAlgorithm = TRUE,
                      reproducible.cachePath = tmpCache,
                      reproducible.tempPath = tmpdir)
  if (length(opts) > 0)
    defaultOpts[names(opts)] <- opts
  opts <- defaultOpts

  if (!is.null(opts)) {
    if (needGoogle) {
      optsGoogle <- if (utils::packageVersion("googledrive") >= "1.0.0") {
        # list(httr_oob_default = quickPlot::isRstudioServer(),
        #      httr_oauth_cache = "~/.httr-oauth")
      } else {
        list(httr_oob_default = quickPlot::isRstudioServer())
      }
      opts <- append(opts, optsGoogle)
    }
    opts <- options(opts)
  }

  if (!is.null(tmpFileExt)) {
    ranfiles <- unlist(lapply(tmpFileExt, function(x) paste0(reproducible:::rndstr(1, 7), ".", x)))
    tmpfile <- file.path(tmpdir, ranfiles)
    tmpfile <- gsub(pattern = "\\.\\.", tmpfile, replacement = "\\.")
    file.create(tmpfile)
    tmpfile <- reproducible::normPath(tmpfile)
  }

  try(clearCache(tmpdir, ask = FALSE), silent = TRUE)
  try(clearCache(tmpCache, ask = FALSE), silent = TRUE)

  outList <- list(tmpdir = tmpdir, origDir = origDir, libs = libraries,
                  tmpCache = tmpCache, optsAsk = optsAsk,
                  optsVerbose = optsVerbose, tmpfile = tmpfile,
                  opts = opts, needGoogle = needGoogle)
  list2env(outList, envir = parent.frame())
  return(outList)
}

testOnExit <- function(testInitOut) {
  if (length(testInitOut$optsVerbose))
    options("reproducible.verbose" = testInitOut$optsVerbose[[1]])
  if (length(testInitOut$optsAsk))
    options("reproducible.ask" = testInitOut$optsAsk[[1]])
  if (length(testInitOut$opts))
    options(testInitOut$opts)
  setwd(testInitOut$origDir)
  unlink(testInitOut$tmpdir, recursive = TRUE)
  if (isTRUE(testInitOut$needGoogle)) {
    if (utils::packageVersion("googledrive") < "1.0.0")
      googledrive::drive_auth_config(active = FALSE)
  }
  unlink(testInitOut$tmpCache, recursive = TRUE, force = TRUE)
  unlink(testInitOut$tmpdir, recursive = TRUE, force = TRUE)

  if (grepl("Pq", class(getOption("reproducible.conn", NULL)))) {
    tabs <- DBI::dbListTables(conn = getOption("reproducible.conn", NULL))
    tab1 <- grep(value = TRUE, tabs, pattern =
                   paste(collapse = "_", c(basename2(dirname(testInitOut$tmpCache)),
                                           basename2(testInitOut$tmpCache))))
    tab2 <- grep(value = TRUE, tabs, pattern =
                   paste(collapse = "_", c(basename2(dirname(testInitOut$tmpdir)),
                                           basename2(testInitOut$tmpdir))))
    if (length(tab1))
      try(DBI::dbRemoveTable(conn = getOption("reproducible.conn", NULL), tab1))
    if (length(tab2))
      try(DBI::dbRemoveTable(conn = getOption("reproducible.conn", NULL), tab2))
  }

  lapply(testInitOut$libs, function(lib) {
    try(detach(paste0("package:", lib), character.only = TRUE), silent = TRUE)}
  )
}
