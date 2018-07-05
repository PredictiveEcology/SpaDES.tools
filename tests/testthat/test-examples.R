test_that("all exported functions have examples", {
  debug <- grepl("VIC-", Sys.info()["nodename"]) & grepl("emcintir", Sys.info()["user"])

  if (isTRUE(debug))  {
    tmpExFile <- "C:/Eliot/tmp/examplesSpaDES.tools.txt"
    cat("#START##############\n", file = tmpExFile, append = FALSE)
  }

  exFiles <- normalizePath(dir("../../man", full.names = TRUE))
  # use for loop as it keeps control at top level
  owd <- getwd()
  tmpdir <- file.path(tempdir(), "test_Examples") %>% checkPath(create = TRUE)
  setwd(tmpdir)
  on.exit({
    unlink(tmpdir, recursive = TRUE)
    setwd(owd)
  } , add = TRUE)

  if (isTRUE(debug)) {
    cat(paste("All files exist: ", isTRUE(all(file.exists(exFiles))), "\n"),
        file = tmpExFile, append = TRUE)
  }

  for (file in exFiles) {
    if (isTRUE(debug)) {
      cat(paste(file, " -- ", "\n"), file = tmpExFile, append = TRUE)
    }

    print(file)
    test_example(file)
  }
})
