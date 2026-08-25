test_that("specificNumPerPatch places the requested number of agents per patch", {
  testInit(c("terra", "data.table"))

  set.seed(1234)
  Ntypes <- 4L
  ras <- randomPolygons(numTypes = Ntypes)

  ## numPerPatchTable ------------------------------------------------------
  patchDT <- data.table::data.table(pops = 1:Ntypes, num.in.pop = c(1, 3, 5, 7))
  rasAgents <- specificNumPerPatch(ras, patchDT)
  rasAgents[is.na(rasAgents)] <- 0

  expect_s4_class(rasAgents, "SpatRaster")
  ## the raster is an indicator: agents are 1, everything else 0
  expect_setequal(unique(terra::values(rasAgents)[, 1]), c(0, 1))
  ## and each patch received exactly the number asked for
  expect_equal(unname(table(ras[rasAgents])), unname(table(rep(patchDT$pops, patchDT$num.in.pop))))
  expect_identical(sum(terra::values(rasAgents)), sum(patchDT$num.in.pop))

  ## numPerPatchMap --------------------------------------------------------
  ## Remap in one pass. Rewriting in place (as the documented example does)
  ## collides: patch 2's cells become 3, which the i == 3 iteration then
  ## matches and overwrites to 5, so patch 2 silently gets patch 3's count.
  rasPatches <- ras
  terra::values(rasPatches) <-
    patchDT$num.in.pop[match(terra::values(ras)[, 1], patchDT$pops)]
  rasAgentsMap <- specificNumPerPatch(ras, numPerPatchMap = rasPatches)
  rasAgentsMap[is.na(rasAgentsMap)] <- 0

  ## the map form encodes the same request, so it places the same totals
  expect_identical(sum(terra::values(rasAgentsMap)), sum(patchDT$num.in.pop))
  expect_equal(unname(table(ras[rasAgentsMap])),
               unname(table(rep(patchDT$pops, patchDT$num.in.pop))))
})

test_that("specificNumPerPatch requires one of the two ways to say how many", {
  testInit(c("terra", "data.table"))

  set.seed(1234)
  ras <- randomPolygons(numTypes = 2L)
  expect_error(specificNumPerPatch(ras), "need numPerPatchMap or numPerPatchTable")
})

test_that("specificNumPerPatch is reproducible under a seed", {
  testInit(c("terra", "data.table"))

  set.seed(1234)
  ras <- randomPolygons(numTypes = 3L)
  patchDT <- data.table::data.table(pops = 1:3, num.in.pop = c(2, 4, 6))

  set.seed(99); a <- specificNumPerPatch(ras, patchDT)
  set.seed(99); b <- specificNumPerPatch(ras, patchDT)
  expect_identical(terra::values(a), terra::values(b))
})
