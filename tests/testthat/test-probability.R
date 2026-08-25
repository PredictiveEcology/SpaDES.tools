test_that("dwrpnorm2 is a valid wrapped normal density", {
  ## from the documented example: the density must integrate to ~1 over [0, 2pi]
  theta <- c(1:500) * 2 * pi / 500
  density <- dwrpnorm2(theta, pi, 0.75)

  expect_length(density, 500L)
  expect_true(all(density >= 0))
  expect_equal(sum(density * 2 * pi / 500), 1, tolerance = 1e-6)
})

test_that("dwrpnorm2 is vectorized over theta and mu", {
  ## This is the function's stated purpose over CircStats::dwrpnorm, and it
  ## was broken: a stray `if (length(var) != len) var <- rep(var, len)` picked
  ## up stats::var (a closure) and errored for any len > 1. The documented
  ## example loops one theta at a time, so it never caught this.
  theta <- c(0.5, 1.5, 2.5, 3.5)

  vec <- dwrpnorm2(theta, pi, 0.75)
  loop <- vapply(theta, function(th) dwrpnorm2(th, pi, 0.75), numeric(1))
  expect_equal(vec, loop)

  ## vectorized over mu as well, and mu recycles against theta
  mus <- c(0, pi / 2, pi, 3 * pi / 2)
  vecMu <- dwrpnorm2(theta, mus, 0.75)
  loopMu <- vapply(seq_along(theta), function(i) dwrpnorm2(theta[i], mus[i], 0.75), numeric(1))
  expect_equal(vecMu, loopMu)

  ## a scalar theta with vector mu is driven by the longest input
  expect_length(dwrpnorm2(1, mus, 0.75), length(mus))
})

test_that("dwrpnorm2 peaks at mu and sharpens with rho", {
  ## density is highest at the mean direction ...
  expect_gt(dwrpnorm2(pi, pi, 0.75), dwrpnorm2(0, pi, 0.75))

  ## ... symmetric about it ...
  expect_equal(dwrpnorm2(pi - 0.4, pi, 0.75), dwrpnorm2(pi + 0.4, pi, 0.75))

  ## ... and a larger mean resultant length concentrates the mass
  expect_gt(dwrpnorm2(pi, pi, 0.9), dwrpnorm2(pi, pi, 0.5))

  ## rho -> 0 approaches the circular uniform density, 1 / (2 * pi). The
  ## tolerance reflects `acc`, which truncates the infinite summation.
  expect_equal(dwrpnorm2(1.234, pi, 0.001), 1 / (2 * pi), tolerance = 1e-3)
})

test_that("dwrpnorm2 rejects rho outside [0, 1] and derives rho from sd", {
  expect_error(dwrpnorm2(1, pi, rho = -0.1), "rho must be between 0 and 1")
  expect_error(dwrpnorm2(1, pi, rho = 1.1), "rho must be between 0 and 1")

  ## when rho is absent it is derived as exp(-sd^2 / 2)
  expect_equal(dwrpnorm2(1, pi, sd = 0.5),
               dwrpnorm2(1, pi, rho = exp(-0.5 ^ 2 / 2)))
})
