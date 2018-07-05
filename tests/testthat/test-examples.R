test_that("all exported functions have examples", {
  tmpExFile <- "C:/Eliot/tmp/examplesSpaDES.tools.txt"
  if (grepl("VIC-", Sys.info()["nodename"]))  {
    cat("#START##############\n", file = tmpExFile, append = FALSE) # for debugging only #nolint
  }

  exFiles <- normalizePath(dir("../../man", full.names = TRUE))
  # use for loop as it keeps control at top level
  owd <- getwd()
  tmpdir <- file.path(tempdir(), "test_Examples") %>% checkPath(create = TRUE)
  setwd(tmpdir)
  on.exit({
    unlink(tmpdir, recursive = TRUE)
    setwd(owd)
    }
    , add = TRUE)
  if (grepl("VIC-", Sys.info()["nodename"])) { # for debugging only
    cat(paste("All files exist: ", isTRUE(all(file.exists(exFiles))), "\n"),
        file = tmpExFile, append = TRUE)

  }

  for (file in exFiles) {
    if (grepl("VIC-", Sys.info()["nodename"])) {
      cat(paste(file, " -- ", "\n"), file = tmpExFile, append = TRUE) # for debugging only
    }
    # for debugging only
    print(file)
    test_example(file)
  }

})
