test_that("all exported functions have examples", {

  exFiles <- normalizePath(dir("../../man", full.names = TRUE))
  testInit()
  exFiles <- exFiles[!dir.exists(exFiles)]
  # use for loop as it keeps control at top level
  for (file in exFiles) {
    print(file)
    test_example(file)
  }
})
