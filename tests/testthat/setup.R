# Preload all Suggests so that examples don't take tons of time
requireNamespace("NLMR", quietly = TRUE)
requireNamespace("raster", quietly = TRUE)
requireNamespace("DEoptim", quietly = TRUE)
requireNamespace("sf", quietly = TRUE)
requireNamespace("sp", quietly = TRUE)
requireNamespace("terra", quietly = TRUE)
library(data.table)
origDTthreads <- getDTthreads()
setDTthreads(2)

if (interactive() && Sys.info()["user"] %in% c("emcintir", "achubaty"))
  if (requireNamespace("quickPlot", quietly = TRUE))
    quickPlot::dev()

withr::defer({
  data.table::setDTthreads(origDTthreads)
}, teardown_env())
