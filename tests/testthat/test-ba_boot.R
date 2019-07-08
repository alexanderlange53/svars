test_that("Bootstrap-after-bootstrap with id.dc / id.cv works with wild.boot and modulus < 1", {
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.dc(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bb <- wild.boot(x1, distr = 'mammen', nboot = 20, n.ahead = 30, nc = 1, signrest = signrest)


  bb2 <- ba.boot(bb, nc = 1)

  expect_equal(bb2$count, 0)
  expect_equal(round(bb2$root, 2), 0.97)

  x1 <- id.cv(v1, SB = 60)
  bbcv <- wild.boot(x1, design = 'recursive', distr = 'mammen', nboot = 20, n.ahead = 30, nc = 1, signrest = signrest)

  bbcv2 <- ba.boot(bbcv, nc = 1)

  expect_equal(bbcv2$count, 0)
  expect_equal(round(bbcv2$root, 2), 0.98)
})

test_that("Bootstrap-after-bootstrap with id.dc /id.cv works with mb.boot and modulus > 1", {
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.dc(v1)

  bb <- mb.boot(x1, b.length = 15, nboot = 20, n.ahead = 30, nc = 1, signrest = NULL)

  bb2 <- ba.boot(bb, nc = 1)

  expect_equal(bb2$count, 0)
  expect_equal(round(bb2$root, 2), 0.99)

  x1 <- id.cv(v1, SB = 60)
  bbcv <- mb.boot(x1, design = 'fixed', b.length = 15, nboot = 20, n.ahead = 30, nc = 1, signrest = NULL)

  bbcv2 <- ba.boot(bbcv, nc = 1)

  expect_equal(bbcv2$count, 0)
  expect_equal(round(bbcv2$root, 2), 0.96)
})
