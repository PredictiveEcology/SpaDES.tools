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

      message("  Downloading ", filename)

      httr::GET(
        url = paste0(urls$url[[i]], filename),
        authenticate,
        httr::progress(),
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
  ext <- tolower(file_ext(archivePath))
  args <- list(archivePath, exdir = dataPath)

  if (ext == "zip") {
    fun <- "unzip"
    filesInArchive <- unzip(archivePath, list = TRUE)$Name
    args <- c(args, list(junkpaths = TRUE))
  } else if (ext == "tar") {
    fun <- "untar"
    filesInArchive <- Cache(untar, archivePath, list = TRUE)
  }

  if (any(needed %in% basename(filesInArchive))) {
    message(paste("  Extracting from archive:", basename(archivePath)))
    do.call(fun, c(args, list(files = filesInArchive[basename(filesInArchive) %in% needed])))
  }

  isArchive <- grepl(file_ext(filesInArchive), pattern = "(zip|tar)", ignore.case = TRUE)

  if (any(isArchive)) {
    arch <- filesInArchive[isArchive]
    do.call(fun, c(list(files = arch), args))
    extractedArchives <- c(
      extractedArchives,
      unlist(
        lapply(file.path(dataPath, arch), extractFromArchive, needed = needed,
               extractedArchives = extractedArchives)
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
#' @param alsoExtract Optional character string naming files other than
#' \code{targetFile} that must be extracted from the \code{archive}.
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
#' @inheritParams cropReprojInputs
#'
#' @inheritParams writeInputsOnDisk
#'
#' @param writeCropped Write the output on disk ?
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
#' @importFrom digest digest
#' @rdname prepInputs
#'
prepInputs <- function(targetFile,
                       archive = NULL,
                       alsoExtract = NULL,
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
    Cache(
      read.table,
      asPath(file.path(modulePath, moduleName, "data", "CHECKSUMS.txt")),
      header = TRUE,
      stringsAsFactors = FALSE,
      digestPathContent = TRUE,
      notOlderThan = notOlderThan,
      userTags = cacheTags,
      quick = quick
    )
  )

  # checkSums <- data.table(
  #   Cache(checksums,
  #         module = moduleName,
  #         path = modulePath,
  #         digestPathContent = TRUE,
  #         checksumFile = asPath(file.path(modulePath, moduleName, "data", "CHECKSUMS.txt")),
  #         write = FALSE,
  #         notOlderThan = notOlderThan,
  #         userTags = cacheTags,
  #         quickCheck = quick
  #   )
  # )

  # Check if the checkSums match, otherwise download or extract the file
  checksums <- checkSums[file == targetFile, ]

  result <- if (quick) {
    file.size(asPath(targetFilePath)) == checksums[["filesize"]]
  } else {
    digest(asPath(targetFilePath), checksums[["algorithm"]], file = TRUE) == checksums[["checksum"]]
  }

  mismatch <- !isTRUE(result)

  if (mismatch) {
    if (is.null(archive)) {
      downloadFromWebDB(targetFile, targetFilePath, dataset)

      if (quick) {
        fileSize <- file.size(asPath(targetFilePath))

        if (checksums[["filesize"]] != fileSize)
          warning("The version downloaded of ", targetFile, " does not match the checksums.")

      } else {

        checkSum <- digest(file = asPath(targetFilePath), algo = checksums[["algorithm"]], file = TRUE)

        if (checksums[["checksum"]] != checkSum)
          warning("The version downloaded of ", targetFile, " does not match the checksums.")
      }
    } else {
      archive <- basename(archive)
      archivePath <- file.path(dataPath, archive)

      checksums <- checkSums[file == targetFile, ]

      result <- if (quick) {
        file.size(asPath(archivePath)) == checksums[["filesize"]]
      } else {
        digest(asPath(archivePath), checksums[["algorithm"]]) == checksums[["checksum"]]
      }

      mismatch <- !compareNA(result, "OK")

      if (mismatch) {
        downloadFromWebDB(archive, archivePath, dataset)

        if (quick) {
          fileSize <- file.size(asPath(archivePath))

          if (checksums[["filesize"]] != fileSize)
            warning("The version downloaded of ", archive, " does not match the checksums.")

        } else {

          checkSum <- digest(file = asPath(archivePath), algo = checksums[["algorithm"]])

          if (checksums[["checksum"]] != checkSum)
            warning("The version downloaded of ", archive, " does not match the checksums.")
        }
      }

      unlink(extractFromArchive(archivePath = archivePath, needed = c(targetFile, alsoExtract)))
    }
  }

  f <- getFromNamespace(fun, pkg)

  if (fun == "raster" && pkg == "raster") {
    x <- f(targetFilePath)
  } else {
    x <- Cache(f, targetFilePath, userTags = cacheTags)
  }

  # objClass <- is(x)

  if (!is.null(studyArea) || !is.null(rasterToMatch)) {
    targetCRS <-
      if (!is.null(rasterToMatch)) {
        crs(rasterToMatch)
      } else if (!is.null(studyArea)) {
        crs(studyArea)
      } else {
        crs(targetFile)
      }

    if (!is.null(studyArea)) {
      if (!identical(targetCRS, crs(studyArea))) {
        if (is(studyArea, "sf"))
          studyArea <- Cache(st_transform, x = studyArea, crs = targetCRS, quick = quick, userTags = cacheTags)
        else
          studyArea <- Cache(spTransform, x = studyArea, CRSobj = targetCRS, quick = quick, userTags = cacheTags)
      }
    }

    x <- Cache(
      cropReprojInputs,
      x = x,
      studyArea = studyArea,
      rasterToMatch = rasterToMatch,
      rasterInterpMethod = rasterInterpMethod,
      quick = quick,
      cacheTags = cacheTags,
      userTags = cacheTags
    )

    if (!is.null(studyArea)) {
      x <- Cache(
        maskInputs,
        x = x,
        studyArea = studyArea,
        userTags = cacheTags,
        quick = quick
      )
    }

    if (writeCropped) {
      smallFN <- smallNamify(targetFilePath)

      Cache(
        writeInputsOnDisk,
        filename = smallFN,
        rasterDatatype = rasterDatatype,
        quick = quick,
        userTags = cacheTags,
        notOlderThan = if (!file.exists(asPath(smallFN))) Sys.time()
      )
    }
  }
  x
}

#' Reproject, crop module inputs
#'
#' This function can be used to crop or reproject module inputs from raw data.
#'
#' @param x a Spatial*, sf or Raster* object
#'
#' @param studyArea SpatialPolygonsDataFrame or sf object used for cropping and masking.
#'
#' @param rasterToMatch Template Raster* object used for reprojecting and
#' cropping.
#'
#' @param rasterInterpMethod Method used to compute values for the new
#' RasterLayer. See \code{\link[raster]{projectRaster}}. Defaults to bilinear.
#'
#' @param addTagsByObject Pass any object in there for which there is a
#' .tagsByClass function
#'
#' @param cacheTags Character vector with Tags. These Tags will be added to the
#' repository along with the artifact.
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster buffer crop crs extent projectRaster res
#' @importFrom rgeos gIsValid
#' @importFrom reproducible Cache
#' @importFrom sp SpatialPolygonsDataFrame spTransform
#' @importFrom sf st_is_valid st_buffer st_transform
#' @rdname cropReprojInputs

cropReprojInputs <- function(x, studyArea = NULL, rasterToMatch = NULL,
                             rasterInterpMethod = "bilinear",
                             addTagsByObject = NULL, cacheTags = NULL) {
  message("  Cropping, reprojecting")

  if (!is.null(studyArea) || !is.null(rasterToMatch)) {
    targetCRS <-
      if (!is.null(rasterToMatch)) {
        crs(rasterToMatch)
      } else if (!is.null(studyArea)) {
        crs(studyArea)
      } else {
        crs(x)
      }

    if (!is.null(studyArea)) {
      if (!identical(targetCRS, crs(studyArea))) {
        if (is(studyArea, "sf"))
          studyArea <- Cache(st_transform, x = studyArea, crs = targetCRS, userTags = cacheTags)
        else
          studyArea <- Cache(spTransform, x = studyArea, CRSobj = targetCRS, userTags = cacheTags)
      }
    }

    if (is(x, "RasterLayer") ||
        is(x, "RasterStack") ||
        is(x, "RasterBrick")) {
      if (!is.null(studyArea)) {
        x <- Cache(
          crop,
          x = x,
          y = studyArea,
          userTags = cacheTags
        )
      }

      if (!is.null(rasterToMatch)) {
        if (!identical(crs(x), targetCRS) |
            !identical(res(x), res(rasterToMatch)) |
            !identical(extent(x), extent(rasterToMatch))) {
          x <- Cache(projectRaster, from = x, to = rasterToMatch,
                     method = rasterInterpMethod, userTags = cacheTags)
        }
      } else {
        if (!identical(crs(x), targetCRS)) {
          x <- Cache(projectRaster, from = x, crs = targetCRS,
                     method = rasterInterpMethod, userTags = cacheTags)
        }
      }
    } else if (is(x, "SpatialPolygonsDataFrame")) {
      if (!suppressWarnings(gIsValid(x))) {
        xValid <- Cache(buffer, x, dissolve = FALSE, width = 0, userTags = cacheTags)
        x <- Cache(SpatialPolygonsDataFrame, Sr = xValid, data = as.data.frame(x),
                   userTags = cacheTags)
      }

      if (!identical(targetCRS, crs(x)))
        x <- Cache(spTransform, x = x, CRSobj = targetCRS, userTags = cacheTags)

      if (!is.null(studyArea)) {
        x <- Cache(crop, x, studyArea, userTags = cacheTags)
      }

      if (!is.null(rasterToMatch)) {
        x <- Cache(crop, x, rasterToMatch, userTags = cacheTags)
      }
    } else if (is(x, "sf")) {
      if (!suppressWarnings(st_is_valid(x))) {
        x <- Cache(st_buffer, x, dist = 0, userTags = cacheTags)
      }

      if (!identical(targetCRS, crs(x)))
        x <- Cache(st_transform, x = x, crs = targetCRS, userTags = cacheTags)

      if (!is.null(studyArea)) {
        x <- Cache(crop, x, studyArea, userTags = cacheTags)
      }

      if (!is.null(rasterToMatch)) {
        x <- Cache(crop, x, rasterToMatch, userTags = cacheTags)
      }
    } else {
      stop("Class ", class(x), " is not supported.")
    }
  }
  x
}

#' Mask module inputs
#'
#' This function can be used to mask module inputs from raw data.
#'
#' @param x a Raster* object
#'
#' @param studyArea SpatialPolygons* object
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom reproducible Cache
#' @rdname maskInputs
#'
maskInputs <- function(x, studyArea) {
  message("  Masking")
  fastMask(x = x, polygon = studyArea)
}

#' Write module inputs on disk
#'
#' This function can be used to write prepared inputs on disk
#'
#' @param x a Spatial*, sf or Raster* object
#'
#' @inheritParams raster::writeRaster
#'
#' @param rasterDatatype Output data type. Passed to \code{\link[raster]{writeRaster}}
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster shapefile writeRaster
#' @importFrom reproducible Cache
#' @importFrom sf st_write
#' @rdname writeInputsOnDisk
#'

writeInputsOnDisk <- function(x, filename, rasterDatatype = NULL) {
  if (is(x, "RasterLayer") ||
      is(x, "RasterStack") ||
      is(x, "RasterBrick")) {

    writeRaster(x = x, overwrite = TRUE, format = "GTiff",
                        datatype = rasterDatatype, filename = filename)
  } else if (is(x, "SpatialPolygonsDataFrame")) {
    shapefile(x = x, overwrite = TRUE, filename = filename)
  } else if (is(x, "sf"))
  {
    st_write(obj = x, delete_dsn = TRUE, dsn = filename)
  } else {
    stop("Don't know how to write object of class ", class(x), " on disk.")
  }
}
