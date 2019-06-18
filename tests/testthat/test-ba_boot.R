test_that("Bootstrap-after-bootstrap with id.dc works with wild.boot and modulus < 1", {
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.dc(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bb <- wild.boot(x1, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = signrest)


  bb2 <- ba.boot(bb, nc = 1)

  expect_equal(bb2$count, 0)
  expect_equal(round(bb2$root, 2), 0.97)
})

test_that("Bootstrap-after-bootstrap with id.dc works with mb.boot and modulus > 1", {
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.dc(v1)

  bb <- mb.boot(x1, b.length = 15, nboot = 20, n.ahead = 30, nc = 1, signrest = NULL)

  bb2 <- ba.boot(bb, nc = 1)

  expect_equal(bb2$count, 0)
  expect_equal(round(bb2$root, 2), 0.99)
})
