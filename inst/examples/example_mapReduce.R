library(data.table)
library(terra)

ras <- rast(ext(0, 15, 0, 15), res = 1)
fullRas <- randomPolygons(ras, numTypes = 2)
names(fullRas) <- "mapcodeAll"
uniqueComms <- unique(fullRas)
reducedDT <- data.table(mapcodeAll = uniqueComms,
                        communities = sample(1:1000, length(uniqueComms)),
                        biomass = rnbinom(length(uniqueComms), mu = 4000, 0.4))
biomass <- rasterizeReduced(reducedDT, fullRas, "biomass")

# The default key is the layer name of the fullRas, so rekey incase of miskey
setkey(reducedDT, biomass)

communities <- rasterizeReduced(reducedDT, fullRas, "communities")
setColors(communities) <- c("blue", "orange", "red")
if (interactive()) {
  clearPlot()
  Plot(biomass, communities, fullRas)
}
