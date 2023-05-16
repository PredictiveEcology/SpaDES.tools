set.seed(1234)

ras <- terra::rast(terra::ext(0, 10, 0, 10), res = 1, val = 0)
rp <- randomPolygons(ras, numTypes = 10)

terra::plot(rp)

angles <- seq(0, pi * 2, length.out = 17)
angles <- angles[-length(angles)]
n <- 2
loci <- sample(terra::ncell(rp), n)
coords <- terra::vect(terra::xyFromCell(rp, loci))
stopRule <- function(landscape) landscape < 3
d2 <- spokes(rp, coords = coords, stopRule = stopRule,
             minRadius = 0, maxRadius = 50,
             returnAngles = TRUE, returnDistances = TRUE,
             allowOverlap = TRUE, angles = angles, returnIndices = TRUE)

# Assign values to the "patches" that were in the viewshed of a ray
rasB <- terra::rast(ras)
rasB[] <- 0
rasB[d2[d2[, "stop"] == 1, "indices"]] <- 1

terra::plot(rasB, add = TRUE, col = "red")

if (NROW(d2) > 0) {
  sp1 <- terra::vect(d2[, c("x", "y")])
  terra::plot(sp1, add = TRUE, pch = 19)
}
terra::plot(coords, add = TRUE, pch = 19, col = "blue")
