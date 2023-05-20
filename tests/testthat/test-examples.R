test_that("all exported functions have examples", {

  exFiles <- normalizePath(dir("../../man", full.names = TRUE))
  exFiles <- exFiles[!dir.exists(exFiles)]
  # use for loop as it keeps control at top level
  owd <- getwd()
  tmpdir <- file.path(tempdir(), "test_Examples") %>% checkPath(create = TRUE)
  setwd(tmpdir)
  on.exit({
    unlink(tmpdir, recursive = TRUE)
    setwd(owd)
  } , add = TRUE)


  for (file in exFiles) {

    print(file)
    test_example(file)
  }
})
