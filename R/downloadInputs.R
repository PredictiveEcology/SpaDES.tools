if (getRversion() >= "3.1.0") {
  utils::globalVariables("expectedFile")
}

#' Download file from web databases
#'
#' This function can be used to download a file from a web database listed in
#'\link[webDatabases]{urls}.
#'
#' @param filename Character string naming the file to be downloaded.
#'
#' @param filepath Character string giving the path where the file will be
#' written.
#'
#' @param dataset Optional character string representing the dataset of interest
#' for download. Allows for restricting the lookup for the url to a dataset,
#' thus avoiding filename collision.
#'
#' @author Jean Marchal
#' @importFrom webDatabases urls
#' @rdname downloadFromWebDB
#'
downloadFromWebDB <- function(filename, filepath, dataset = NULL) {
  urls <- webDatabases::urls

  if (!is.null(set <- dataset))
    urls <- urls[grepl(dataset, pattern = set, fixed = TRUE)]

  for (i in 1:nrow(urls)) {
    if (any(filename == urls$files[[i]])) {
      authenticate <-
        if (!is.na(urls$password[[i]])) {
          split <- strsplit(urls$password[[i]], split = "[:]")[[1]]
          httr::authenticate(split[1L], split[2L])
        }

      httr::GET(
        url = paste0(urls$url[[i]], filename),
        authenticate,
        httr::write_disk(filepath, overwrite = TRUE)
      )
      break
    }
  }
}

#' Extract files from archive.
#'
#' Extract zip or tar archive files, possibly nested in other zip or tar
#' archives.
#'
#' @param archivePath Character string giving the path of the archive
#' containing the \code{file} to be extracted.
#'
#' @param dataPath Character string giving the path where \code{needed} will be
#' extracted. Defaults to the archive directory.
#'
#' @param needed Character string giving the name of the file(s) to be extracted.
#'
#' @param extractedArchives Used internally.
#'
#' @return A character vector listing the paths of the extracted archives.
#'
#' @author Jean Marchal
#' @importFrom reproducible Cache
#' @importFrom tools file_ext
#' @rdname extractFromArchive
#'
extractFromArchive <- function(archivePath, dataPath = dirname(archivePath),
                               needed, extractedArchives = NULL) {
  ext <- tolower(tools::file_ext(archivePath))
  args <- list(archivePath, exdir = dataPath)

  if (ext == "zip") {
    fun <- "unzip"
    filesInArchive <- unzip(archivePath, list = TRUE)$Name
    args <- c(args, list(junkpaths = TRUE))
  } else if (ext == "tar") {
    fun <- "untar"
    filesInArchive <- Cache(untar, archivePath, list = TRUE)
  }

  if (any(needed %in% filesInArchive)) {
    message(paste("  Extracting from archive:", basename(archivePath)))
    do.call(fun, c(args, list(files = needed[needed %in% filesInArchive])))
  }

  isArchive <- grepl(tools::file_ext(filesInArchive), pattern = "(zip|tar)", ignore.case = TRUE)

  if (any(isArchive)) {
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

#' @keywords internal
smallNamify <- function(name) {
  file.path(dirname(name), paste0("Small", basename(name)))
}

#' Download and optionally reproject, crop, mask raw data and output module
#' inputs
#'
#' This function can be used to prepare module inputs from raw data. It
#' runs several other functions, conditionally and sequentially:
#' \code{downloadFromWebDB}, \code{extractFromArchive}.
#'
#' @param targetFile Character string giving the path of the raw data.
#'
#' @param archive Optional character string giving the path of an archive
#' containing \code{targetFile}.
#'
#' @param dataset Optional character string representing the dataset of interest
#' for download. Allows for restricting the lookup for the url to a dataset,
#' thus avoiding filename collision.
#'
#' @param moduleName Character string giving the name of the module.
#'
#' @param modulePath Character string giving the path to the module directory.
#'
#' @param fun Character string indicating the function to use to load
#' \code{targetFile}.
#'
#' @param pkg Character string indicating the package in which to find \code{fun}.
#'
#' @param studyArea spatial* or sf object used for cropping and masking.
#'
#' @param rasterToMatch Template Raster* object used for reprojecting and
#' cropping.
#'
#' @param rasterInterpMethod Method used to compute values for the new
#' RasterLayer. See \code{\link[raster]{projectRaster}}. Defaults to bilinear.
#'
#' @param rasterDatatype Output data type. Passed to \code{\link[raster]{writeRaster}}.
#'
#' @param writeCropped Write the output on disk ?
#'
#' @param addTagsByObject Pass any object in there for which there is a
#' .tagsByClass function
#'
#' @inheritParams reproducible::Cache
#'
#' @param cacheTags Character vector with Tags. These Tags will be added to the
#' repository along with the artifact.
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom data.table data.table
#' @importFrom methods is
#' @importFrom reproducible Cache compareNA asPath
#' @importFrom sf st_is_valid st_buffer st_transform st_write
#' @importFrom amc fastMask
#' @importFrom digest digest
#' @rdname prepInputs
#'
prepInputs <- function(targetFile,
                       archive = NULL,
                       dataset = NULL,
                       modulePath,
                       moduleName,
                       fun = "raster",
                       pkg = "raster",
                       studyArea = NULL,
                       rasterToMatch = NULL,
                       rasterInterpMethod = "bilinear",
                       rasterDatatype = "INT2U",
                       writeCropped = TRUE,
                       addTagsByObject = NULL,
                       quick = FALSE,
                       cacheTags = "stable") {
  message("Preparing: ", targetFile)
  dataPath <- file.path(modulePath, moduleName, "data")

  targetFile <- basename(targetFile)
  targetFilePath <- file.path(dataPath, targetFile)

  # Here we assume that if dataPath has not been updated checksums don't need to
  # be rerun. This is useful for WEB apps.
  capturedOutput <- capture.output(
    tmp <- Cache(file.info, asPath(dir(dataPath, full.names = TRUE)), userTags = cacheTags),
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

  if (mismatch) {
    if (is.null(archive)) {
      downloadFromWebDB(targetFile, targetFilePath, dataset)

      if (quick) {
        fileSize <- file.size(asPath(targetFilePath))

        if (checksums[["filesize"]] != fileSize)
          warning("The version downloaded of ", targetFile, " does not match the checksums")
      } else {
        checkSum <- digest::digest(file = asPath(targetFilePath), algo = checksums[["algorithm"]])

        if (checksums[["checksum"]] != checkSum)
          warning("The version downloaded of ", targetFile, " does not match the checksums")
      }
    } else {
      archive <- basename(archive)
      archivePath <- file.path(dataPath, archive)

      checksums <- checkSums[expectedFile == archive,]
      mismatch <- !compareNA(checksums[["result"]], "OK")

      if (mismatch) {
        downloadFromWebDB(archive, archivePath, dataset)

        if (quick) {
          fileSize <- file.size(asPath(archivePath))

          if (checksums[["filesize"]] != fileSize)
            warning("The version downloaded of ", archive, " does not match the checksums")
        } else {
          checkSum <- digest::digest(file = asPath(archivePath), algo = checksums[["algorithm"]])

          if (checksums[["checksum"]] != checkSum)
            warning("The version downloaded of ", archive, " does not match the checksums")
        }
      }

      unlink(extractFromArchive(archivePath = archivePath, needed = targetFile))
    }
  }

  f <- getFromNamespace(fun, pkg)

  if (fun == "raster" && pkg == "raster") {
    x <- f(targetFilePath)
  } else {
    x <- Cache(f, targetFilePath, userTags = cacheTags)
  }

  objClass <- is(x)

  if (!is.null(studyArea) || !is.null(rasterToMatch)) {
    targetCRS <-
      if (!is.null(rasterToMatch)) {
        raster::crs(rasterToMatch)
      } else if (!is.null(studyArea)) {
        raster::crs(studyArea)
      } else {
        raster::crs(targetFile)
      }

    smallFN <- smallNamify(targetFilePath)

    if (!is.null(studyArea)) {
      if (!identical(targetCRS, raster::crs(studyArea)))
        studyArea <- Cache(sp::spTransform, x = studyArea, CRSobj = targetCRS, userTags = cacheTags)
    }

    message("  Cropping, reprojecting")

    if ("RasterLayer" %in% objClass || "RasterStack" %in% objClass || "RasterBrick" %in% objClass) {
      if (!is.null(studyArea)) {
        # if (!identical(raster::crs(studyArea), raster::crs(x)))
        # {
        #   studyArea <- Cache(sp::spTransform, x = studyArea, CRSobj = raster::crs(x), userTags = cacheTags)
        # }
        x <- Cache(
          raster::crop,
          x = x,
          y = studyArea,
          userTags = cacheTags
        )
      }

      if (!is.null(rasterToMatch)) {
        if (!identical(raster::crs(x), targetCRS) |
            !identical(raster::res(x), raster::res(rasterToMatch)) |
            !identical(raster::extent(x), raster::extent(rasterToMatch))) {
          x <- Cache(raster::projectRaster, from = x, to = rasterToMatch,
                     method = rasterInterpMethod, userTags = cacheTags)
        }
      } else {
        if (!identical(raster::crs(x), targetCRS)) {
          x <- Cache(raster::projectRaster, from = x, crs = targetCRS,
                     method = rasterInterpMethod, userTags = cacheTags)
        }
      }

      if (!is.null(studyArea)) {
        message("  Masking")
        x <- Cache(amc::fastMask, stack = x, polygon = studyArea, userTags = cacheTags)
      }

      if (writeCropped) {
        x <- Cache(
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
    } else if ("spatialObjects" %in% objClass) {
      if (!suppressWarnings(rgeos::gIsValid(x))) {
        xValid <- Cache(raster::buffer, x, dissolve = FALSE, width = 0, userTags = cacheTags)
        x <- Cache(sp::SpatialPolygonsDataFrame, Sr = xValid, data = as.data.frame(x),
                   userTags = cacheTags)
      }

      if (!identical(targetCRS, raster::crs(x)))
        x <- Cache(sp::spTransform, x = x, CRSobj = targetCRS, userTags = cacheTags)

      if (!is.null(studyArea)) {
        x <- Cache(raster::crop, x, studyArea, userTags = cacheTags)
      }

      if (!is.null(rasterToMatch)) {
        x <- Cache(raster::crop, x, rasterToMatch, userTags = cacheTags)
      }

      if (writeCropped) {
        Cache(
          raster::shapefile,
          x = x,
          overwrite = TRUE,
          filename = smallFN,
          userTags = cacheTags,
          notOlderThan = if (!file.exists(asPath(smallFN))) Sys.time()
        )
      }
    } else if ("sf" %in% objClass) {
      if (!suppressWarnings(sf::st_is_valid(x))) {
        x <- Cache(sf::st_buffer, x, dist = 0, userTags = cacheTags)
      }

      if (!identical(targetCRS, raster::crs(x)))
        x <- Cache(sf::st_transform, x = x, crs = targetCRS, userTags = cacheTags)

      if (!is.null(studyArea)) {
        x <- Cache(raster::crop, x, studyArea, userTags = cacheTags)
      }

      if (!is.null(rasterToMatch)) {
        x <- Cache(raster::crop, x, rasterToMatch, userTags = cacheTags)
      }
      # x <- Cache(sf::st_collection_extract, x = x, type = "POLYGON")

      if (writeCropped) {
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

