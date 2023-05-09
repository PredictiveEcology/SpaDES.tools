test_that("mapReduce: file does not work correctly 1", {
  library(data.table)
  library(raster)
  library(terra)

  on.exit({
    detach("package:data.table")
    detach("package:raster")
  }, add = TRUE)

  ras <- terra::rast(terra::ext(0, 15, 0, 15), res = 1)
  set.seed(123)
  fullRas <- randomPolygons(ras, numTypes = 2)
  names(fullRas) <- "mapcodeAll"
  uniqueComms <- as.vector(unique(fullRas[]))
  reducedDT <- data.table(
    mapcodeAll = as.integer(uniqueComms),
    communities = sample(1:1000, length(uniqueComms)),
    biomass = as.integer(rnbinom(length(uniqueComms), mu = 4000, 0.4))
  )

  biomass <- rasterizeReduced(reducedDT, fullRas, "biomass")
  expect_equal(sort(as.vector(unique(biomass[]))), sort(reducedDT$biomass))

  communities <- rasterizeReduced(reducedDT, fullRas, "communities")
  expect_equal(sort(as.vector(unique(communities[]))), sort(reducedDT$communities))

  expect_true(sum(table(sort(fullRas[])) * reducedDT$communities) == sum(communities[]))

  ## with SpatRaster
  fullSpatRas <- rast(fullRas)
  uniqueComms <- as.vector(unique(fullSpatRas[]))

  biomass <- rasterizeReduced(reducedDT, fullSpatRas, "biomass")
  expect_equal(sort(as.vector(unique(biomass[]))), sort(reducedDT$biomass))

  communities <- rasterizeReduced(reducedDT, fullSpatRas, "communities")
  expect_equal(sort(as.vector(unique(communities[]))), sort(reducedDT$communities))

  expect_true(sum(table(sort(fullSpatRas[])) * reducedDT$communities) == sum(communities[]))
})

# test_that("mapReduce: file does not work correctly 2", {
#   library(data.table)
#   library(raster)
#
#   on.exit({
#    detach("package:data.table")
#    detach("package:raster"))
#   }, add = TRUE)
#
#   ras <- terra::rast(terra::ext(0,15,0,15), res=1)
#   fullRas <- randomPolygons(ras, numTypes=5, speedup=1, p=0.3)
#   names(fullRas) <- "mapcodeAll"
#   uniqueComms <- raster::unique(fullRas)
#   reducedDT <- data.table(
#     mapcodeAll=uniqueComms,
#     communities=sample(1:1000, length(uniqueComms)),
#     biomass=rnbinom(length(uniqueComms), mu=4000, 0.4)
#   )
#   biomass <- rasterizeReduced(reducedDT, fullRas, "biomass")
#
#   expect_equal(sort(unique(getValues(biomass))), sort(reducedDT$biomass))
#   expect_equal(length(unique(getValues(biomass))), length(unique(getValues(fullRas))))
#
#   setkey(reducedDT, biomass)
#   communities <- rasterizeReduced(reducedDT, fullRas, "communities")
#   expect_equal(sort(unique(getValues(communities))), sort(reducedDT$communities))
#   expect_equal(length(unique(getValues(communities))), length(unique(getValues(fullRas))))
# })
#
# test_that("mapReduce: file does not work correctly 3", {
#   library(data.table)
#   library(raster)
#
#   on.exit({
#    detach("package:data.table")
#    detach("package:raster"))
#   }, add = TRUE)
#
#   ras <- terra::rast(terra::ext(0, 15, 0, 15), res = 1)
#   fullRas <- randomPolygons(ras, numTypes = 5, speedup = 1, p = 0.3)
#   names(fullRas) <- "mapcodeAll""'
#   uniqueComms <- raster::unique(fullRas)
#   reducedDT <- data.table(
#     mapcodeAll=uniqueComms,
#     communities=sample(1:1000, length(uniqueComms)),
#     biomass=rnbinom(length(uniqueComms), mu = 4000, 0.4)
#   )
#   biomass <- rasterizeReduced(reducedDT, fullRas, "biomass")
#
#   setkey(reducedDT, biomass)
#   communities <- rasterizeReduced(reducedDT, fullRas, "communities")
#   expect_equal(sort(unique(getValues(communities))), sort(reducedDT$communities))
# })
#
# test_that("mapReduce: file does not work correctly 4", {
#   library(data.table)
#   library(raster)
#
#   on.exit({
#    detach("package:data.table")
#    detach("package:raster"))
#   }, add = TRUE)
#
#   ras <- terra::rast(terra::ext(0,15,0,15), res=1)
#   fullRas <- randomPolygons(ras, numTypes=5, speedup=1, p=0.3)
#   names(fullRas) <- "mapcodeAll"
#   uniqueComms <- raster::unique(fullRas)
#   reducedDT <- data.table(
#     mapcodeAll=uniqueComms,
#     communities=sample(1:1000, length(uniqueComms)),
#     biomass=rnbinom(length(uniqueComms), mu=4000, 0.4)
#   )
#   biomass <- rasterizeReduced(reducedDT, fullRas, "biomass")
#
#   setkey(reducedDT, biomass)
#   communities <- rasterizeReduced(reducedDT, fullRas, "communities")
#   expect_equal(length(unique(getValues(communities))), length(unique(getValues(fullRas))))
# })
