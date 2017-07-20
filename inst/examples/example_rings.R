library(raster)
library(quickPlot)

# Make random forest cover map
emptyRas <- raster(extent(0, 1e2, 0, 1e2), res = 1)

# start from two cells near middle
loci <- (ncell(emptyRas) / 2 - ncol(emptyRas)) / 2 + c(-3, 3)

# Allow overlap
emptyRas[] <- 0
rngs <- rings(emptyRas, loci = loci, allowOverlap = TRUE, returnIndices = TRUE)
# Make a raster that adds together all id in a cell
wOverlap <- rngs[, list(sumEventID = sum(id)), by = "indices"]
emptyRas[wOverlap$indices] <- wOverlap$sumEventID
if (interactive()) {
  clearPlot()
  Plot(emptyRas)
}

# No overlap is default, occurs randomly
emptyRas[] <- 0
rngs <- rings(emptyRas, loci = loci, minRadius = 7, maxRadius = 9, returnIndices = TRUE)
emptyRas[rngs$indices] <- rngs$id
if (interactive()) {
  clearPlot()
  Plot(emptyRas)
}

# Variable ring widths, including centre cell for smaller one
emptyRas[] <- 0
rngs <- rings(emptyRas, loci = loci, minRadius = c(0, 7), maxRadius = c(8, 18),
              returnIndices = TRUE)
emptyRas[rngs$indices] <- rngs$id
if (interactive()) {
  clearPlot()
  Plot(emptyRas)
}
