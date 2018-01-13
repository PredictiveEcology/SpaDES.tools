downloadFromWebDB <- function(filename, filepath, dataset = NULL)
{
  urls <- webDatabases::urls

  if (!is.null(dataset))
    urls <- urls[dataset == dataset]

  for (i in 1:nrow(urls))
  {
    if (any(filename == urls$files[[i]]))
    {
      authenticate <-
        if (!is.na(urls$password[[i]]))
        {
          split <- strsplit(urls$password[[i]], split = "[:]")[[1]]
          httr::authenticate(split[1L], split[2L])
        }

      GET(
        url = paste0(urls$url[[i]], filename),
        authenticate,
        write_disk(filepath, overwrite = TRUE)
      )
      break
    }
  }
}

extractFromArchive <- function(archivePath, dataPath = dirname(archivePath), needed, extractedArchives = NULL)
{
  ext <- tolower(tools::file_ext(archivePath))
  args <- list(archivePath, exdir = dataPath)

  if (ext == "zip")
  {
    fun <- "unzip"
    filesInArchive <- unzip(archivePath, list = TRUE)$Name
    args <- c(args, list(junkpaths = TRUE))
  }
  else if (ext == "tar")
  {
    fun <- "untar"
    filesInArchive <- Cache(untar, archivePath, list = TRUE)
  }

  if (any(needed %in% filesInArchive))
  {
    message(paste("  Extracting from archive:", basename(archivePath)))
    do.call(fun, c(args, list(files = needed[needed %in% filesInArchive])))
  }

  isArchive <- grepl(tools::file_ext(filesInArchive), pattern = "(zip|tar)", ignore.case = TRUE)

  if (any(isArchive))
  {
    arch <- filesInArchive[isArchive]
    do.call(fun, c(list(files = arch), args))
    extractedArchives <- c(
      extractedArchives,
      unlist(
        lapply(file.path(dataPath, arch), extractFromArchive, needed = needed, extractedArchives = extractedArchives)
      )
    )
  }

  c(extractedArchives, archivePath)
}


smallNamify <- function(name)
{
  file.path(dirname(name), paste0("Small", basename(name)))
}

prepInputs <- function(targetFile,
                       archive = NULL,
                       modulePath,
                       moduleName,
                       loadFun = "raster",
                       loadPackage = "raster",
                       studyArea = NULL,
                       writeCropped = FALSE,
                       rasterToMatch = NULL,
                       rasterInterpMethod = "bilinear",
                       rasterDatatype = "INT2U",
                       addTagsByObject = NULL,
                       cacheTags = "stable")
{
  dataPath <- file.path(modulePath, moduleName, "data")

  targetFile <- basename(targetFile)
  targetFilePath <- file.path(dataPath, targetFile)

  # Here we assume that if dataPath has not been updated checksums don't need to
  # be rerun. This is useful for WEB apps.
  capturedOutput <- capture.output(
    Cache(file.info, asPath(dir(dataPath, full.names = TRUE)), userTags = cacheTags),
    type = "message"
  )

  notOlderThan <- if (length(capturedOutput) == 0) Sys.time()

  checkSums <- data.table(
    Cache(checksums,
          module = moduleName,
          path = modulePath,
          digestPathContent = TRUE,
          checksumFile = asPath(file.path(modulePath, moduleName, "data", "CHECKSUMS.txt")),
          write = FALSE,
          notOlderThan = notOlderThan,
          userTags = cacheTags
    )
  )

  # Check if the checkSums match, otherwise download or extract the file
  checksums <- checkSums[expectedFile == targetFile,]
  mismatch <- !compareNA(checksums[["result"]], "OK")

  if (mismatch)
  {
    if (is.null(archive))
    {
      downloadFromWebDB(targetFile, targetFilePath)

      if (.quickCheck)
      {
        fileSize <- file.size(asPath(targetFilePath))

        if (checksums[["filesize"]] != fileSize)
          warning("The version downloaded of ", targetFile, " does not match the checksums")
      }
      else
      {
        checkSum <- digest::digest(file = asPath(targetFilePath), algo = checksums[["algorithm"]])

        if (checksums[["checksum"]] != checkSum)
          warning("The version downloaded of ", targetFile, " does not match the checksums")
      }
    }
    else
    {
      archive <- basename(archive)
      archivePath <- file.path(dataPath, archive)

      checksums <- checkSums[expectedFile == archive,]
      mismatch <- !compareNA(checksums[["result"]], "OK")

      if (mismatch)
      {
        downloadFromWebDB(archive, archivePath)

        if (.quickCheck)
        {
          fileSize <- file.size(asPath(archivePath))

          if (checksums[["filesize"]] != fileSize)
            warning("The version downloaded of ", archive, " does not match the checksums")
        }
        else
        {
          checkSum <- digest::digest(file = asPath(archivePath), algo = checksums[["algorithm"]])

          if (checksums[["checksum"]] != checkSum)
            warning("The version downloaded of ", archive, " does not match the checksums")
        }
      }

      unlink(extractFromArchive(archive = archivePath, needed = targetFile))
    }
  }

  fun <- getFromNamespace(loadFun, loadPackage)

  if (loadFun == "raster" && loadPackage == "raster")
  {
    x <- fun(targetFilePath)
  }
  else
  {
    x <- Cache(fun(targetFilePath), userTags = cacheTags)
  }

  objClass <- is(x)

  if (!is.null(studyArea) || !is.null(rasterToMatch))
  {
    targetCRS <-
      if (!is.null(rasterToMatch))
      {
        raster::crs(rasterToMatch)
      }
      else if (!is.null(studyArea))
      {
        raster::crs(studyArea)
      }
      else raster::crs(targetFile)

    smallFN <- smallNamify(targetFilePath)

    if (!is.null(studyArea))
    {
      if (!identical(targetCRS, raster::crs(studyArea)))
        studyArea <- Cache(sp::spTransform, x = studyArea, CRSobj = targetCRS, userTags = cacheTags)
    }

    message("  Cropping, reprojecting")

    if ("RasterLayer" %in% objClass || "RasterStack" %in% objClass)
    {
      if (!is.null(studyArea))
      {
        if (!identical(raster::crs(studyArea), raster::crs(x)))
        {
          x <- Cache(
            raster::crop,
            x = x,
            y = Cache(sp::spTransform, x = studyArea, CRSobj = raster::crs(x), userTags = cacheTags),
            userTags = cacheTags
          )
        }
      }

      if (!identical(raster::crs(x), targetCRS))
      {
        x <- Cache(raster::projectRaster, from = x, to = rasterToMatch, method = rasterInterpMethod, userTags = cacheTags)
      }

      message("  Masking")
      x <- Cache(amc::fastMask, x = x, mask = studyArea, userTags = cacheTags)

      if (writeCropped)
      {
        Cache(
          raster::writeRaster,
          x = x,
          overwrite = TRUE,
          format = "GTiff",
          datatype = rasterDatatype,
          filename = smallFN,
          userTags = cacheTags,
          notOlderThan = if (!file.exists(asPath(smallFN))) Sys.time()
        )
      }
    }
    else if ("spatialObjects" %in% objClass)
    {
      if (!suppressWarnings(rgeos::gIsValid(x)))
      {
        xValid <- Cache(raster::buffer, x, dissolve = FALSE, width = 0, userTags = cacheTags)
        x <- Cache(sp::SpatialPolygonsDataFrame, Sr = xValid, data = as.data.frame(x), userTags = cacheTags)
      }

      if (!identical(targetCRS, raster::crs(x)))
        x <- Cache(sp::spTransform, x = x, CRSobj = targetCRS, userTags = cacheTags)

      if (!is.null(studyArea))
      {
        x <- Cache(raster::crop, x, studyArea, userTags = cacheTags)
      }

      if (!is.null(rasterToMatch))
      {
        x <- Cache(raster::crop, x, rasterToMatch, userTags = cacheTags)
      }

      if (writeCropped)
      {
        Cache(
          raster::shapefile,
          x = x,
          overwrite = TRUE,
          filename = smallFN,
          userTags = cacheTags,
          notOlderThan = if (!file.exists(asPath(smallFN))) Sys.time()
        )
      }
    }
    else if ("sf" %in% objClass)
    {
      if (!suppressWarnings(sf::st_is_valid(x)))
      {
        x <- Cache(sf::st_buffer, x, dist = 0, userTags = cacheTags)
      }

      if (!identical(targetCRS, raster::crs(x)))
        x <- Cache(sf::st_transform, x = x, crs = targetCRS, userTags = cacheTags)

      if (!is.null(studyArea))
      {
        x <- Cache(raster::crop, x, studyArea, userTags = cacheTags)
      }

      if (!is.null(rasterToMatch))
      {
        x <- Cache(raster::crop, x, rasterToMatch, userTags = cacheTags)
      }
      # x <- Cache(sf::st_collection_extract, x = x, type = "POLYGON")

      if (writeCropped)
      {
        Cache(
          sf::st_write,
          obj = x,
          delete_dsn = TRUE,
          dsn = smallFN,
          userTags = cacheTags,
          notOlderThan = if (!file.exists(asPath(smallFN))) Sys.time()
        )
      }
    }
  }

  x
}

