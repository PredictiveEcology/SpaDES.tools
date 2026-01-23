test_that("rastFromDF", {
  testInit(c("terra", "data.table"))

  # Template raster (3x3 grid)
  tmpl <- rast(nrows = 3, ncols = 3, xmin = 0, xmax = 3, ymin = 0, ymax = 3,
               crs = "EPSG:4326")

  # Sparse table: provide values for a subset of cells
  df <- data.table(
    cell = c(1L, 3L, 5L, 9L),
    v1   = c(10, 20, 30, 40),
    v2   = c(100, 200, 300, 400)
  )

  r <- rastFromDF(df, tmpl)
  testthat::expect_s4_class(r, "SpatRaster")
  testthat::expect_equal(ignore_attr = TRUE, terra::nlyr(r), 2L)
  for (i in 1:NROW(df)) {
    for (j in names(df)[-1])
    testthat::expect_equal(terra::values(mat = FALSE, r[[j]])[df$cell[i]], df[[j]][i] )
  }
  # terra::plot(r)  # remaining cells are NA
})
