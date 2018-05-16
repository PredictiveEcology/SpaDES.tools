
defineModule(sim, list(
  name = "prepInputsProblems",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("First", "Last", email = "first.last@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9011", prepInputsProblems = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "prepInputsProblems.Rmd"),
  reqdPkgs = list("sp", "raster", "fasterize"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
  ),
  inputObjects = bind_rows(
 ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    #    createsOutput(objectName = "template", objectClass = "RasterLayer", desc = "Map working"),
    createsOutput(objectName = "rtmRAS", objectClass = "RasterLayer", desc = "raster to be used as rasterToMatch"),
    createsOutput(objectName = "rtmSHP", objectClass = "RasterLayer", desc = "shapefile to be used as rasterToMatch"),
    createsOutput(objectName = "studyAreaRAS", objectClass = "RasterLayer", desc = "studyArea as a raster"),
    createsOutput(objectName = "studyAreaSHP", objectClass = "shapefile", desc = "studyArea as a shapefile"),
    createsOutput(objectName = "SHP", objectClass = "shapefile", desc = "basic shapefile template to test combinations of study area and rasterToMatch"),
    createsOutput(objectName = "SHP1", objectClass = "shapefile", desc = "SHP + studyAreaSHP"),
    createsOutput(objectName = "SHP2", objectClass = "shapefile", desc = "SHP + studyAreaRAS"),
    createsOutput(objectName = "SHP3", objectClass = "shapefile", desc = "SHP + rtmSHP"),
    createsOutput(objectName = "SHP4", objectClass = "shapefile", desc = "SHP + rtmRAS"),
    createsOutput(objectName = "SHP5", objectClass = "shapefile", desc = "SHP + studyAreaSHP + rtmSHP"),
    createsOutput(objectName = "SHP6", objectClass = "shapefile", desc = "SHP + studyAreaSHP + rtmRAS"),
    createsOutput(objectName = "SHP7", objectClass = "shapefile", desc = "SHP + studyAreaRAS + rtmSHP"),
    createsOutput(objectName = "SHP8", objectClass = "shapefile", desc = "SHP + studyAreaRAS + rtmRAS"),
    createsOutput(objectName = "SHP9", objectClass = "shapefile", desc = "SHP + studyAreaSHP + rtmRAS" + "useSAcrs = TRUE"),
    createsOutput(objectName = "SHP10", objectClass = "shapefile", desc = "SHP + studyAreaSHP + useSAcrs = TRUE"),
    createsOutput(objectName = "RAS", objectClass = "RasterLayer", desc = "basic raster template to test combinations of study area and rasterToMatch"),
    createsOutput(objectName = "RAS1", objectClass = "RasterLayer", desc = "RAS + studyAreaSHP"),
    createsOutput(objectName = "RAS2", objectClass = "RasterLayer", desc = "RAS + studyAreaRAS"),
    createsOutput(objectName = "RAS3", objectClass = "RasterLayer", desc = "RAS + rtmSHP"),
    createsOutput(objectName = "RAS4", objectClass = "RasterLayer", desc = "RAS + rtmRAS"),
    createsOutput(objectName = "RAS5", objectClass = "RasterLayer", desc = "RAS + studyAreaSHP + rtmSHP"),
    createsOutput(objectName = "RAS6", objectClass = "RasterLayer", desc = "RAS + studyAreaSHP + rtmRAS"),
    createsOutput(objectName = "RAS7", objectClass = "RasterLayer", desc = "RAS + studyAreaRAS + rtmSHP"),
    createsOutput(objectName = "RAS8", objectClass = "RasterLayer", desc = "RAS + studyAreaRAS + rtmRAS"),
    createsOutput(objectName = "RAS9", objectClass = "RasterLayer", desc = "RAS + studyAreaSHP + rtmRAS + useSAcrs = TRUE"),
    createsOutput(objectName = "RAS10", objectClass = "RasterLayer", desc = "RAS + studyAreaSHP + useSAcrs = TRUE"),
    createsOutput(objectName = "RAS11", objectClass = "RasterLayer", desc = "RAS + rtmRAS + maskWithRTM = TRUE"),
    createsOutput(objectName = "RAS12", objectClass = "RasterLayer", desc = "RAS + rtmRAS + studyAreaSHP + maskWithRTM = TRUE"),
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.prepInputsProblems = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
#===========    rastersToMatch

    sim$rtmSHP <- prepInputs(url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gccs000a11a_e.zip",
                              destinationPath = dataPath(sim)) %>%
          .[.$CCSUID == 4819006,]

      sim$rtmRAS <- prepInputs(targetFile = file.path("hapctnpp-geotiff","hapctnpp_geotiff.tif"),
                               archive = "hapctnpp-geotiff.zip",
                               url = "https://drive.google.com/file/d/12Cm1CaY_yaCIdakT5E2Ty9HnQfbNyMcS/view?usp=sharing", #googledrive example & nested folder
                               destinationPath = dataPath(sim))

      shpCan <- prepInputs(targetFile = "Canada.shp", url = "https://drive.google.com/file/d/12r-Rp1b3qmk-omXm7xLpCnsj9VPYFMJs/view?usp=sharing",
                           destinationPath = dataPath(sim), rasterToMatch = sim$rtmRAS)

      sim$rtmRAS <- Cache(cropInputs, sim$rtmRAS, studyArea = shpCan)
      

#===========    studyArea
      
      sim$studyAreaSHP <- prepInputs(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
                                     destinationPath = dataPath(sim)) %>%
        .[.$ECODISTRIC == 339,]
      
      sim$studyAreaRAS <- prepInputs(targetFile = file.path("hapctnpp-geotiff","hapctnpp_geotiff.tif"),
                                     archive = "hapctnpp-geotiff.zip",
                                     url = "https://drive.google.com/file/d/12r-Rp1b3qmk-omXm7xLpCnsj9VPYFMJs/view?usp=sharing",
                                     destinationPath = dataPath(sim))
      
#===========    SHP (when these are applied to shapefiles)
      
      sim$SHP <- prepInputs(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
                             destinationPath = dataPath(sim))
      
      sim$SHP1 <- prepInputs(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
                            destinationPath = dataPath(sim), studyArea = sim$studyAreaSHP) # SHP crop with sA. CORRECT PROJECTION

#      sim$SHP2 <- prepInputs(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
#                             destinationPath = dataPath(sim), studyArea = sim$studyAreaRAS) # EXPECTED ERROR

#      sim$SHP3 <- prepInputs(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
#                             destinationPath = dataPath(sim), rasterToMatch = sim$rtmSHP) # EXPECTED ERROR

      sim$SHP4 <- prepInputs(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
                             destinationPath = dataPath(sim), rasterToMatch = sim$rtmRAS) # Works with rgeosBuffer warning

#      sim$SHP5 <- prepInputs(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
#                             destinationPath = dataPath(sim), studyArea = sim$studyAreaSHP, rasterToMatch = sim$rtmSHP) # EXPECTED ERROR

      sim$SHP6 <- prepInputs(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
                             destinationPath = dataPath(sim), studyArea = sim$studyAreaSHP, rasterToMatch = sim$rtmRAS) #

#      sim$SHP7 <- prepInputs(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
#                             destinationPath = dataPath(sim), studyArea = sim$studyAreaRAS, rasterToMatch = sim$rtmSHP) # EXPECTED ERROR

#      sim$SHP8 <- prepInputs(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
#                             destinationPath = dataPath(sim), studyArea = sim$studyAreaRAS, rasterToMatch = sim$rtmRAS) # EXPECTED ERROR
      sim$SHP9 <- prepInputs(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
                             destinationPath = dataPath(sim), studyArea = sim$studyAreaSHP, rasterToMatch = sim$rtmRAS,
                             useSAcrs = TRUE) #
      
      sim$SHP10 <- prepInputs(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
                             destinationPath = dataPath(sim), studyArea = sim$studyAreaSHP,
                             useSAcrs = TRUE)
      
#===========    RAS (when these are applied to rasters)
      
      sim$RAS <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
                            destinationPath = dataPath(sim))
      
      sim$RAS1 <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
                             destinationPath = dataPath(sim), studyArea = sim$studyAreaSHP) #

      # sim$RAS2 <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
      #                        destinationPath = dataPath(sim), studyArea = sim$studyAreaRAS) # EXPECTED ERROR
      
      # sim$RAS3 <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
      #                        destinationPath = dataPath(sim), rasterToMatch = sim$rtmSHP) # EXPECTED ERROR
      
      sim$RAS4 <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
                             destinationPath = dataPath(sim), rasterToMatch = sim$rtmRAS) # Works with rgeosBuffer warning
      
      # sim$RAS5 <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
      #                        destinationPath = dataPath(sim), studyArea = sim$studyAreaSHP, rasterToMatch = sim$rtmSHP) # EXPECTED ERROR
      
      sim$RAS6 <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
                             destinationPath = dataPath(sim), studyArea = sim$studyAreaSHP, rasterToMatch = sim$rtmRAS) #
      
      # sim$RAS7 <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
      #                        destinationPath = dataPath(sim), studyArea = sim$studyAreaRAS, rasterToMatch = sim$rtmSHP) # EXPECTED ERROR
      
      # sim$RAS8 <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
      #                        destinationPath = dataPath(sim), studyArea = sim$studyAreaRAS, rasterToMatch = sim$rtmRAS) # EXPECTED ERROR
      
      sim$RAS9 <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
                             destinationPath = dataPath(sim), studyArea = sim$studyAreaSHP, rasterToMatch = sim$rtmRAS,
                             useSAcrs = TRUE)
      
      sim$RAS10 <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
                             destinationPath = dataPath(sim), studyArea = sim$studyAreaSHP,
                             useSAcrs = TRUE) #
      
      sim$RAS11 <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
                              destinationPath = dataPath(sim), rasterToMatch = sim$rtmRAS,
                              maskWithRTM = TRUE)
      
      sim$RAS12 <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
                              destinationPath = dataPath(sim), studyArea = sim$studyAreaSHP, rasterToMatch = sim$rtmRAS,
                              maskWithRTM = TRUE)
    },
warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
              "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  

  return(invisible(sim))
}
