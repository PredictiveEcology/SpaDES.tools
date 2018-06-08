if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("dataset", "files"))
}

#' Table of relevant dataset accessible using url
#'
#' Stores dataset name with their respective url and username/password.
#' The table is used by \code{listWebDatabases} to retrive file available to
#' download by using only the dataset name.
#'
#' @return \code{data.table} containing dataset available for download.
#'
#' @author Melina Houle
#' @importFrom data.table data.table
#' @keywords internal
#' @rdname urlsWide
.urlsWide <- function() {
  data.table(
    dataset = c(
      "AHCCD_daily",
      "NFDB",
      "KNN",
      #"BIOSIM",
      "LCC2005",
      "NALCMS2005",
      "NALCMS2010",
      "EOSD2000"
    ),
    url = c(
      "ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/",
      "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/",
      "http://tree.pfc.forestry.ca/",
      #"ftp://ftp.nofc.cfs.nrcan.gc.ca/uploads/MPB/",
      "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/",
      "http://www.cec.org/sites/default/files/Atlas/Files/Land_Cover_2005/",
      "http://www.cec.org/sites/default/files/Atlas/Files/Land_Cover_2010/",
      "http://tree.pfc.forestry.ca/"
    ),
    password = c(NA,
                 NA,
                 NA,
                 #NA,
                 NA,
                 NA,
                 NA,
                 NA),
    files = list(
      c(
        "Adj_Daily_Rain_v2016.zip",
        "Adj_Daily_Snow_v2016.zip",
        "Adj_Daily_TotalP_v2016.zip",
        "Adj_Precipitation_Documentation_v2016_Daily.doc",
        "Adj_Precipitation_Stations_v2016.xls",
        "Homog_daily_max_temp_v2016.zip",
        "Homog_daily_mean_temp_v2016.zip",
        "Homog_daily_min_temp_v2016.zip",
        "Homog_temperature_documentation_v2016.doc",
        "Homog_temperature_stations_v2016.xlsx",
        "Vincent_and_coauthors_Trends_in _Canada's_Climate_JClimate_June2015.pdf",
        "Vincent_et al_Second_Generation_Homog_Temp_JGR_2012.pdf",
        "Wang_Feng_Vincent_Extreme_Temp_AO_2013.pdf",
        "ZMekis_Vincent_2011.pdf"
      ),
      c(
        "NFDB_poly.zip",
        "NFDB_poly_20160712_large_fires_metadata.pdf",
        "NFDB_poly_20160712_metadata.pdf",
        "NFDB_poly_large_fires.zip"
      ),
      c(
        "FR_NFI_and_kNN_Mapping_20160628.docx",
        "NFI_and_kNN_Mapping_20160628.docx",
        "cjfr-2013-0401suppl.pdf",
        "kNN-EcozonesDomain.zip",
        "kNN-Genus.tar",
        "kNN-LandCover.tar",
        "kNN-Soils.tar",
        "kNN-Species.tar",
        "kNN-SpeciesDominant.tar",
        "kNN-SpeciesGroups.tar",
        "kNN-StructureBiomass.tar",
        "kNN-StructureStandVolume.tar"
      ),
      #c("m", "n", "v"),
      c("LandCoverOfCanada2005_V1_4.zip"),
      c("Land_Cover_2005v2_TIFF.zip"),
      c("Land_Cover_2010_TIFF.zip"),
      c("")
    )
  )
}

#' Retrive file available to download.
#'
#' Produce a character vector of files available to download.
#' It uses a preset \code{data.table} in which a list of relevant dataset name,
#' their associate url and password are stored.
#' To retrive available file, the function derive URL, and username/password using
#' the \code{datasetName}.
#'
#' @param urlTbl  A \code{data.table} that stores available dataset name, url,
#'                password and filenames found within each dataset.
#'                \code{urlTbl} is provided within the package as urls object.
#'
#' @param datasetName  Character string. Represent the dataset of interest for download.
#'                     \code{datasetName} allow to derived url and password from the \code{urlTbl}.
#'
#' @param dfile  Character string representing filename of interest to download.
#'               When missing, all files from associated url given will be listed.
#'
#' @return Vector of url to download.
#'
#' @author Melina Houle
#' @importFrom data.table setkey data.table
#' @importFrom plyr .
#' @importFrom RCurl getURL
#' @importFrom XML readHTMLTable
#' @keywords internal
#' @rdname listWebData
.listWebData <- function(urlTbl, datasetName, dfile) {
  if (missing(urlTbl)) {
    stop("You must provide a data.table that contain the link between url, datasetName and password.")
  }
  if (missing(datasetName)) {
    stop("You must provide dataset name to access url of interest.")
  }
  #if (missing(dfile)) dfile <- "all"
  setkey(urlTbl, "dataset")
  password <- as.character(urlTbl[.(datasetName)][, "password", with = FALSE])
  if (password == "NA") password <- NA_character_
  url2data <- as.character(urlTbl[.(datasetName)][, "url", with = FALSE])

  # Split url into typeConn(ftp, http) and address.
  typeConn <- paste(unlist(strsplit(url2data, "//"))[1], "//", sep = "")
  address <- unlist(strsplit(url2data, "//"))[2]

  # Create list of file to download
  if (typeConn == "ftp://") {
    # No password for the ftp site
    if (is.na(password)) {
      file.list <- getURL(url2data, ftp.use.epsv = FALSE, dirlistonly = TRUE)
      file.list <- strsplit(file.list, "\r*\n")[[1]]
      file.list <- file.list[!file.list %in% c(".", "..")]
      file.list <- paste(url2data, file.list, sep = "")
    } else {
      # Password needed
      file.list <- getURL(url2data, userpwd = password, ftp.use.epsv = FALSE, dirlistonly = TRUE)
      file.list <- strsplit(file.list,"\r*\n")[[1]]
      file.list <- file.list[!file.list %in% c(".", "..")]
      file.list <- paste(typeConn, password, "@", address, file.list, sep = "")
    }
  } else if (typeConn == "http://" || typeConn == "https://") {
    file.list <- readHTMLTable(url2data, skip.rows = 1:2)[[1]]$Name
    file.list <- paste(url2data, file.list[!is.na(file.list)], sep = "")
    file.list <- file.list[!file.list %in% c(".", "..")]
  } else {
    stop("Unrecognized url type. Currently, only http://, https://, and ftp:// are supported.")
  }

  return(file.list)
}

#' Grab the current version of the urls either locally or from the git repository
#'
#' Running this will get latest version of the urls, returned as a \code{data.table}.
#'
#' The current webDatabase is located at
#' \href{https://github.com/PredictiveEcology/webDatabases/blob/master/R/webDatabases.R}{Web Database}
#'
#' @param dbUrl Character string where to look for web database.
#'              Defaults to the web database. See details.
#'
#' @param local IGNORED. Logical. If \code{FALSE} the function gets the latest webDatabase
#'              table from the online repository. This will allow for the user to
#'              be always up to date, but it also slower than \code{TRUE}.
#'              If \code{TRUE}, then this will take the version of the webDatabase
#'              that existed when the user installed the package.
#'
#' @param wide Logical. If \code{TRUE}, returns the wide form of database. Default \code{FALSE}
#'
#' @return A data.table with 4 \code{columns}, \code{dataset}, \code{url}, \code{password},
#' and \code{files}, keyed by \code{dataset}, \code{files}.
#'
#' @author Melina Houle
#' @importFrom data.table data.table
#' @importFrom RCurl url.exists
#'
webDatabases <- function(dbUrl = NULL, local = FALSE, wide = FALSE) {
  # if (!local) {
  #   if (is.null(dbUrl) || !RCurl::url.exists(dbUrl)) {
  #     dbUrl <- "https://raw.githubusercontent.com/PredictiveEcology/webDatabases/master/R/webDatabases.R"
  #   }
  #
  #   source(dbUrl, local = TRUE)
  #   message("Database retrieved from PredictiveEcology/webDatabases")
  # } else {
  #   message("Database retrieved locally because ", dbUrl, " is not reachable.")
  # }

  urls <- .urlsWide()
  if (!wide) {
    urls <- urls[ , list(files = unlist(files)), by = "dataset,url,password" ]
    setkey(urls, dataset, files)
  }
  urls
}

#' Download file from web databases
#'
#' @param filename Character string naming the file to be downloaded.
#'
#' @param filepath Character string giving the path where the file will be written.
#'
#' @param dataset Optional character string representing the dataset of interest
#' for download. Allows for restricting the lookup for the url to a dataset,
#' thus avoiding filename collision.
#'
#' @inheritParams prepInputs
#'
#' @author Jean Marchal
#' @importFrom httr authenticate GET http_error progress write_disk
#' @importFrom stats runif
#' @include webDatabases.R
#'
downloadFromWebDB <- function(filename, filepath, dataset = NULL, quick = FALSE, overwrite = TRUE) {
  urls <- webDatabases(local = quick)

  if (!is.null(set <- dataset))
    urls <- urls[grepl(dataset, pattern = set, fixed = TRUE)]

  if (any(wh <- filename == urls$files)) {
    authenticate <- if (!is.na(urls$password[wh])) {
      split <- strsplit(urls$password[wh], split = "[:]")[[1]]
      httr::authenticate(split[1L], split[2L])
    }

    url <- urls$url[wh]

    if (httr::http_error(url))
      stop("Can not access url ", url)

    message("  Downloading ", filename, " ...")

    httr::GET(
      url = paste0(url, filename),
      authenticate,
      httr::progress(),
      httr::write_disk(filepath, overwrite = overwrite)
    )
  }
}
