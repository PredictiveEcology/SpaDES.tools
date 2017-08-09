# test_that("all exported functions have examples", {
#   fns <- ls("package:SpaDES.tools")
#   sapply(fns, function(x) {
#     expect_warning(example(x, package = "SpaDES.tools", character.only = TRUE,
#                            echo = FALSE), NA)
#   })
# })

test_that("check all examples", {
  test_examples(path = "../../man")
})
