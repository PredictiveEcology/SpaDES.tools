library(sf)
library(raster)
library(quickPlot)

map <- raster(system.file("extdata", "map.tif", package = "SpaDES.tools"))
names(map) <- "layer"
pr <- probInit(map, p = (map/maxValue(map))^2)
agents <- initiateAgents(map, 100, pr)
if (interactive()) {
  clearPlot()
  Plot(map)
  Plot(agents, addTo = "map")
}
# Test that they are indeed selecting according to probabilities in pr
library(data.table)
dt1 <- data.table(table(round(map[agents], 0)))
setnames(dt1, old = "N", new = "count")
dt2 <- data.table(table(round(map[], 0)))
setnames(dt2, old = "N", new = "available")
dt <- dt1[dt2, on = "V1"]  # join the counts and available data.tables
setnames(dt, old = "V1", new = "mapValue")
dt[, selection := count/available]
dt[is.na(selection), selection := 0]
if (interactive())
  with(dt, {plot(mapValue, selection)})
#'
# Note, can also produce a Raster representing agents,
# then the number of points produced can't be more than
# the number of pixels:
agentsRas <- initiateAgents(map, 30, pr, asSpatialPoints = FALSE)
if (interactive()) Plot(agentsRas)
#'
if (require(dplyr)) {
  # Check that the agents are more often at the higher probability areas based on pr
  if (utils::packageVersion("raster") >= "2.8-11") {
    out <- data.frame(stats::na.omit(crosstab(agentsRas, map)), table(round(map[]))) %>%
             dplyr::mutate(selectionRatio = Freq / Freq.1) %>%
             dplyr::select(-layer.1, -Var1) %>%
             dplyr::rename(Present = Freq, Avail = Freq.1, Type = layer.2)
  } else {
    out <- data.frame(stats::na.omit(crosstab(agentsRas, map)), table(round(map[]))) %>%
             dplyr::mutate(selectionRatio = Freq/Freq.1) %>%
             dplyr::select(-Var1, -Var1.1) %>%
             dplyr::rename(Present = Freq, Avail = Freq.1, Type = Var2)
  }
  out
}
