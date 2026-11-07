## spreadCpp(link =): spreadProb is a linear predictor and the probability of a cell is computed
## only when a fire reaches it. It must give exactly the cells of the same spread with the full
## probability vector, for the same seed.

powerLawRef <- function(x, pc, gamma, floor, shape) {
  pmax(pc - (pc - floor) * exp(-sign(x) * abs(x / gamma)^shape), 0)
}

test_that("link = 'powerLaw' burns exactly the cells of the full spreadProb vector", {
  testInit(c("terra", "data.table", "withr"))
  ras <- terra::rast(terra::ext(0, 150, 0, 150), resolution = 1, vals = 0)
  n <- terra::ncell(ras)
  withr::local_seed(11)
  lp <- stats::rnorm(n, 0.5, 1.2)
  lp[sample.int(n, n %/% 5)] <- NA          # unburnable cells
  loci <- sample.int(n, 40L)
  lp[loci[1:3]] <- NA                       # ignitions on unburnable cells still start
  for (shape in c(0.5, 1, 2)) {
    pars <- c(pc = 0.26, gamma = 1.9, floor = 0.12, shape = shape)
    for (s in 1:4) {
      eps <- withr::with_seed(100 + s, stats::rnorm(1))
      full <- powerLawRef(lp + eps, pars[["pc"]], pars[["gamma"]], pars[["floor"]], shape)
      full[loci] <- 1
      ## minSize, minSizeTries and jumpTries read spreadProb in all three places it is read
      args <- list(ras, loci = loci, maxSize = Inf, minSize = 9, minSizeTries = 5L, jumpTries = 20L,
                   jumpMeanDist = 3)
      a <- withr::with_seed(s, do.call(spreadCpp, c(args, list(spreadProb = full))))
      b <- withr::with_seed(s, do.call(spreadCpp, c(args, list(spreadProb = lp, link = "powerLaw",
                                                                linkPars = pars, linkOffset = eps))))
      expect_identical(b, a)
      expect_identical(attr(b, "minSizeTries"), attr(a, "minSizeTries"))
      expect_gt(nrow(a), length(loci))
    }
  }
})

test_that("link = NULL (the default) gives the results of a call without the link arguments", {
  testInit(c("terra", "data.table", "withr"))
  ras <- terra::rast(terra::ext(0, 80, 0, 80), resolution = 1, vals = 0)
  sp <- withr::with_seed(3, stats::runif(terra::ncell(ras), 0, 0.3))
  a <- withr::with_seed(4, spreadCpp(ras, loci = c(100L, 3000L), spreadProb = sp, minSize = 5))
  b <- withr::with_seed(4, spreadCpp(ras, loci = c(100L, 3000L), spreadProb = sp, minSize = 5,
                                     link = NULL, linkPars = NULL, linkOffset = 0))
  expect_identical(b, a)
})

test_that("spreadCpp(link =) checks its arguments", {
  testInit(c("terra", "data.table"))
  ras <- terra::rast(terra::ext(0, 20, 0, 20), resolution = 1, vals = 0)
  pars <- c(pc = 0.26, gamma = 1.9, floor = 0.12, shape = 1)
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "probit", linkPars = pars), "link")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "powerLaw"), "linkPars")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "powerLaw", linkPars = pars[-2]), "gamma")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "powerLaw", linkPars = pars,
                         linkOffset = c(1, 2)), "linkOffset")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0.1, linkPars = pars), "link")
})
