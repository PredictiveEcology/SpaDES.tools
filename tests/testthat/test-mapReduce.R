test_that("mapReduce: file does not work correctly 1", {
  withr::local_package("data.table")

  df <- data.frame(pkg = c("raster", "terra"),
                   cls = c("RasterLayer", "SpatRaster"),
                   read = c("raster::raster", "terra::rast"),
                   ext = c("raster::extent", "terra::ext"))

  for (i in seq(NROW(df))) {
    pkg <- df$pkg[i]
    cls <- df$cls[i]
    read <- df$read[i]

    withr::local_package(pkg)
    withr::local_options(reproducible.rasterRead = read)

    extFun <- eval(parse(text = df$ext[i]))

    ras <- reproducible::rasterRead(extFun(0, 15, 0, 15), res = 1)
    ras[] <- NA

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

    ## test factor raster
    cls <- data.frame(id = sort(unique(as.vector(fullRas[]))))
    cls$Bclass <- LETTERS[cls$id]
    clsDT <- as.data.table(cls)

    if (is(fullRas, "SpatRaster")) {
      levels(fullRas) <- cls
    } else {
      fullRas <- raster::as.factor(fullRas)
      levs <- levels(fullRas)[[1]]
      levs$Bclass <- clsDT[id %in% unique(fullRas[]), "Bclass"]

      levels(fullRas) <- levs
    }
    reducedDT <- reducedDT[clsDT, on = "mapcodeAll==id"]
    reducedDT[, mapcodeAll := Bclass]

    biomass2 <- rasterizeReduced(reducedDT, fullRas, "biomass")
    expect_equal(biomass, biomass2)
  }
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
