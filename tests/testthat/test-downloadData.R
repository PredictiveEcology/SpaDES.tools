test_that("downloadData downloads and unzips module data", {
  if (identical(Sys.getenv("TRAVIS"), "true") &&
      tolower(Sys.info()[["sysname"]]) == "darwin") skip("On Travis OSX")
  # skip_on_cran()

  if (Sys.info()["sysname"] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl", download.file.extra = "-L")
  }

  m <- "test"
  tmpdir <- file.path(tempdir(), "modules")
  datadir <- file.path(tmpdir, m, "data")
  reproducible::checkPath(datadir, create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  filenames <- c("DEM.tif", "habitatQuality.tif")
  if (paste0(R.version$major, ".", R.version$minor) > "3.4.2") {
    chksums <- structure(list(
      file = structure(1:2, .Label = c("DEM.tif", "habitatQuality.tif"),
                       class = "factor"),
    checksum = structure(1:2, .Label = c("77c56d42fecac5b1", "f21251dcdf23dde0"),
                         class = "factor"),
    filesize = structure(1:2, .Label = c("6045", "43558"), class = "factor"),
    algorithm = structure(1:2, .Label = c("xxhash64", "xxhash64"), class = "factor")),
    .Names = c("file", "checksum", "filesize", "algorithm"),
    class = "data.frame", row.names = c(NA, -2L))

    moduleDir <- file.path(tmpdir, "test")
    dataDir <- file.path(moduleDir, "data")
    write.table(chksums, file = file.path(dataDir, "CHECKSUMS.txt"), row.names = FALSE)
    expectsInputs <- data.frame(objectName = c("DEM", "habitatQuality"),
                             objectClass = "RasterLayer",
                             sourceURL = c("https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/DEM.tif",
                                           "https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/habitatQuality.tif"),
                             stringsAsFactors = FALSE
                    )

    reproducible::checkPath(dataDir, create = TRUE)

    #f <- downloadModule(m, tmpdir, quiet = TRUE)
    t1 <- system.time(downloadData(m, tmpdir, quiet = FALSE, urls = expectsInputs$sourceURL))
    result <- checksums(m, tmpdir)$result
    expect_true(all(file.exists(file.path(datadir, filenames))))
    expect_true(all(result == "OK"))

    # shouldn't need a redownload because file exists
    t2 <- system.time(downloadData(m, tmpdir, quiet = TRUE, urls = expectsInputs$sourceURL))
    expect_true(t1[3] > t2[3]) # compare elapsed times

    # if one file is missing, will fill in correctly
    unlink(file.path(datadir, filenames)[1])
    downloadData(m, tmpdir, quiet = TRUE, urls = expectsInputs$sourceURL)
    expect_true(all(file.exists(file.path(datadir, filenames))))

    # if files are there, but one is incorrectly named
    file.rename(from = file.path(datadir, filenames[1]),
                to = file.path(datadir, "test.tif"))
    downloadData(m, tmpdir, quiet = TRUE, urls = expectsInputs$sourceURL) # renames the file back to expected
    expect_true(all(file.exists(file.path(datadir, filenames))))

    # if files are there with correct names, but wrong content
    library(raster); on.exit(detach("package:raster"), add = TRUE)
    if (require(rgdal, quietly = TRUE)) {
      on.exit(detach("package:rgdal"), add = TRUE)
      ras <- raster(file.path(datadir, filenames[2]))
      ras[4] <- maxValue(ras) + 1
      writeRaster(ras, filename = file.path(datadir, filenames[2]), overwrite = TRUE)
      downloadData(m, tmpdir, quiet = TRUE, urls = expectsInputs$sourceURL)
      expect_true(all(file.exists(file.path(datadir, filenames))))
    }

    # downloads data using urls described in module metadata
    m <- "LccToBeaconsReclassify"
    tmpdir <- file.path(tempdir(), "modules") %>% reproducible::checkPath(create = TRUE)
    dataDir <- file.path(tmpdir, m, "data") %>% reproducible::checkPath(create = TRUE)

    ## write the defineModude function into an .Rscript to circumvent using downloadmodule
    writeLines(con = file.path(tmpdir, m, paste0(m, ".R")),
               text = "defineModule(sim, list(
                 name = \"LccToBeaconsReclassify\",
                 description = \"Takes the LCC05 classification of 39 land cover classes, and reclassifies it to the 11 classes of the Beacons succession model.\",
                 keywords = c(\"forest succession\", \"LCC05\", \"land cover classification 2005\", \"Beacons\"),
                 childModules = character(),
                 authors = c(
                   person(c(\"Eliot\", \"J\", \"B\"), \"McIntire\", email = \"eliot.mcintire@canada.ca\", role = c(\"aut\", \"cre\")),
                   person(c(\"Alex\", \"M\"), \"Chubaty\", email = \"alexander.chubaty@canada.ca\", role = c(\"aut\")),
                   person(\"Steve\", \"Cumming\", email = \"Steve.Cumming@sbf.ulaval.ca\", role = c(\"aut\"))
                 ),
                 version = numeric_version(\"1.1.2\"),
                 spatialExtent = raster::extent(rep(NA_real_, 4)),
                 timeframe = as.POSIXlt(c(\"2005-01-01\", NA)),
                 timeunit = \"year\",
                 citation = list(\"citation.bib\"),
                 documentation = list(\"README.txt\", \"LccToBeaconsReclassify.Rmd\"),
                 reqdPkgs = list(\"raster\", \"RColorBrewer\", \"fastmatch\", \"dplyr\"),
                 parameters = rbind(
                   defineParameter(\".plotInitialTime\", \"numeric\", NA_real_, NA, NA, desc = \"Initial time for plotting\"),
                   defineParameter(\".plotInterval\", \"numeric\", NA_real_, NA, NA, desc = \"Interval between plotting\"),
                   defineParameter(\".saveInitialTime\", \"numeric\", NA_real_, NA, NA, desc = \"Initial time for saving\"),
                   defineParameter(\".saveInterval\", \"numeric\", NA_real_, NA, NA, desc = \"Interval between save events\")
                 ),
                 inputObjects = data.frame(
                   objectName = \"vegMapLcc\", objectClass = \"RasterLayer\",
                   sourceURL = \"ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip\",
                   other = NA_character_, stringsAsFactors = FALSE),
                 outputObjects = data.frame(
                   objectName = c(\"trajMapBeacons\", \"vegMapBeacons\", \"trajObj\"),
                   objectClass = c(\"RasterLayer\", \"RasterLayer\", \"matrix\"),
                   other = rep(NA_character_, 3L), stringsAsFactors = FALSE)
               ))")

    ## write associated CHECKSUMS.txt
    chksums <- structure(list(
      file = structure(1:2, .Label = c("LCC2005_V1_4a.tif", "LandCoverOfCanada2005_V1_4.zip"),
                       class = "factor"),
      checksum = structure(1:2, .Label = c("b50567fc83bcc5de" , "e100c037a257e377"),
                           class = "factor"),
      filesize = structure(1:2, .Label = c("437916202", "47185238"), class = "factor"),
      algorithm = structure(1:2, .Label = c("xxhash64", "xxhash64"), class = "factor")),
      .Names = c("file", "checksum", "filesize", "algorithm"),
      class = "data.frame", row.names = c(NA, -2L))

    write.table(chksums, file = file.path(dataDir, "CHECKSUMS.txt"), row.names = FALSE)

    filenames <- c("LandCoverOfCanada2005_V1_4.zip")

    on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
    suppressWarnings(downloadData(module = m, path = tmpdir, quiet = TRUE))

    expect_true(all(file.exists(file.path(dataDir, filenames))))
  }
})

