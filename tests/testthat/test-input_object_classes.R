# library(tsDyn)
# library(MTS)
# library(urca)
# context("test-input_object_classes.R")
#
# ### TEST VARs from all 3 packages( varest, nlVAR, and list (MTS), as well as vec2var and VECM classes)
#
# ## test var model classes
# test_that("id.cv works for varest object and analysis (bootstrap, irf, hd fevd)", {
#   skip_on_cran()
#   set.seed(23211)
#   #Get reduced form model
#   v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
#
#   # id.cv
#   x1 <- id.cv(v1, SB = 59)
#   x2 <- id.cv_boot(v1, SB = 59)
#   expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -564.2994)
#     IRF = irf(x1)
#     expect_equal(round(sum(IRF$irf),4), 241.5486)
#     HD = hd(x1)
#     expect_equal(round(sum(HD$hidec), 4), 63.635)
#     FEVD = fevd(x1)
#     expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
#
#     # mb boot
#     signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#     bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#     expect_length(bb, 18)
#     expect_equal(bb$nboot, 10)
#
#     # wild boot
#     bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#     expect_length(bb2, 18)
#     expect_equal(bb2$nboot, 10)
#
#     # ba boot
#     bb3 <- ba.boot(bb, nc = 1)
#     expect_equal(bb3$count, 0)
#     expect_equal(round(bb3$root, 2), 0.98)
#
# })
#
# test_that("id.dc works for varest object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
#     #id.st
#     x1 <- id.st(v1)
#
#     expect_equal(round(x1$Lik, 4), -508.7511)
#     IRF = irf(x1)
#     expect_equal(round(sum(IRF$irf),4), 248.6993)
#     HD = hd(x1)
#     expect_equal(round(sum(HD$hidec), 4), 2.0775)
#     FEVD = fevd(x1)
#     expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
#
#     # mb boot
#     signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#     bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#     expect_length(bb, 18)
#     expect_equal(bb$nboot, 10)
#
#     # wild boot
#     bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#     expect_length(bb2, 18)
#     expect_equal(bb2$nboot, 10)
#
#     # ba boot
#     bb3 <- ba.boot(bb, nc = 1)
#     expect_equal(bb3$count, 0)
#     expect_equal(round(bb3$root, 1), 1)
#
#
# })
#
#
# ## test tsDyn model classes
#
# test_that("id.cv works for nlVars object and analysis (bootstrap, irf, hd fevd)", {
#   skip_on_cran()
#   set.seed(23211)
#
#   v1 <-lineVar(USA, lag = 6)
#
#   # id.cv
#   x1 <- id.cv(v1, SB = 59)
#   x2 <- id.cv_boot(v1, SB = 59)
#   expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -567.2314)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 241.5486)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 4), 63.635)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
#
#   # mb boot
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 0)
#   expect_equal(round(bb3$root, 1), 1)
#
# })
#
# test_that("id.st works for nlVars object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   v1 <-lineVar(USA, lag = 6)
#   #id.st
#   x1 <- id.st(v1)
#
#   expect_equal(round(x1$Lik, 4), -508.7511)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 248.6993)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 4), 2.0775)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
#
#   # mb boot
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 0)
#   expect_equal(round(bb3$root, 1), 1)
# })
#
# test_that("id.cvm works for nlVars object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   v1 <- lineVar(USA, lag = 6)
#   #id.cvm
#   cob <- copula::indepTestSim(v1$T - v1$lag, v1$k, verbose=FALSE)
#   x1 <- id.cvm(v1)
#
#   expect_equal(round(sum(x1$B),3), 0.864)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 206.2219)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 4), -3.5012)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
#
#   # mb boot
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 0)
#   expect_equal(round(bb3$root, 1), 1)
#
# })

test_that("id.dc works for nlVars object and analysis (bootstrap, irf, hd fevd)", {
  set.seed(23211)
  v1 <-lineVar(USA, lag = 6)
  #id.dc
  x1 <- id.dc(v1)

  expect_equal(sum(round(x1$B, 4)), 2.8497)
  IRF = irf(x1)
  expect_equal(round(sum(IRF$irf),4), 238.629)
  HD = hd(x1)
  expect_equal(round(sum(HD$hidec), 4), -3.5012)
  FEVD = fevd(x1)
  expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))

  # mb boot
  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bb, 18)
  expect_equal(bb$nboot, 10)

  # wild boot
  bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bb2, 18)
  expect_equal(bb2$nboot, 10)

  # ba boot
  bb3 <- ba.boot(bb, nc = 1)
  expect_equal(bb3$count, 0)
  expect_equal(round(bb3$root, 1), 1)

})

test_that("id.garch works for nlVars object and analysis (bootstrap, irf, hd fevd)", {
  set.seed(23211)
  v1 <-lineVar(USA, lag = 6)
  # id.garch
  x1 <- id.garch(v1)

  expect_equal(round(x1$Lik, 1),  -547.2)
  expect_equal(round(sum(x1$B), 2), 1.83)

  IRF = irf(x1)
  expect_equal(round(sum(IRF$irf),2), 219.62)
  HD = hd(x1)
  expect_equal(round(sum(HD$hidec), 4), -3.5012)
  FEVD = fevd(x1)
  expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))

  # mb boot
  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bb, 18)
  expect_equal(bb$nboot, 10)

  # wild boot
  bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bb2, 18)
  expect_equal(bb2$nboot, 10)

  # ba boot
  bb3 <- ba.boot(bb, nc = 1)
  expect_equal(bb3$count, 0)
  expect_equal(round(bb3$root, 1), 1)
})

test_that("id.ngml works for nlVars object and analysis (bootstrap, irf, hd fevd)", {
  set.seed(23211)
  v1 <-lineVar(USA, lag = 6)
  # id.ngml
  x1 <- id.ngml(v1)
  x2 <- id.ngml_boot(v1)

  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -548.1502)

  IRF = irf(x1)
  expect_equal(round(sum(IRF$irf),4), 239.0778)
  HD = hd(x1)
  expect_equal(round(sum(HD$hidec), 4), -3.5012)
  FEVD = fevd(x1)
  expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))

  # mb boot
  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bb, 18)
  expect_equal(bb$nboot, 10)

  # wild boot
  bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bb2, 18)
  expect_equal(bb2$nboot, 10)

  # ba boot
  bb3 <- ba.boot(bb, nc = 1)
  expect_equal(bb3$count, 0)
  expect_equal(round(bb3$root, 1), 1)
  })


## test MTS model classes
test_that("id.cv works for MTS list object and analysis (bootstrap, irf, hd fevd)", {
  skip_on_cran()
  set.seed(23211)
  v3 <- invisible(MTS::VAR(USA, p = 6))

  #id.cv
  x3 <- id.cv(v3, SB = 59)
  x4 <- id.cv_boot(v3, SB = 59)
  expect_equal(round(x3$Lik, 4), round(x4$Lik, 4), -564.2994)
  IRF = irf(x3)
  expect_equal(round(sum(IRF$irf),4), 241.5486)
  HD = hd(x3)
  expect_equal(round(sum(HD$hidec), 4), 63.635)
  FEVD = fevd(x3)
  expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))

  # mb boot
  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bb, 18)
  expect_equal(bb$nboot, 10)

  # wild boot
  bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bb2, 18)
  expect_equal(bb2$nboot, 10)

  # ba boot
  bb3 <- ba.boot(bb, nc = 2)
  expect_equal(bb3$count, 0)
  expect_equal(round(bb3$root, 1), 1)
})

test_that("id.st works for MTS list object and analysis (bootstrap, irf, hd fevd)", {
  set.seed(23211)
  v3 <- invisible(MTS::VAR(USA, p = 6))
  #id.st
  x1 <- id.st(v3)

  expect_equal(round(x1$Lik, 4), -508.7511)
  IRF = irf(x1)
  expect_equal(round(sum(IRF$irf),4), 248.6993)
  HD = hd(x1)
  expect_equal(round(sum(HD$hidec), 4), 2.0775)
  FEVD = fevd(x1)
  expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))

  # mb boot
  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bb, 18)
  expect_equal(bb$nboot, 10)

  # wild boot
  bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bb2, 18)
  expect_equal(bb2$nboot, 10)

  # ba boot
  bb3 <- ba.boot(bb, nc = 1)
  expect_equal(bb3$count, 0)
  expect_equal(round(bb3$root, 1), 1)
})

# test_that("id.cvm works for MTS list object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   v3 <- invisible(MTS::VAR(USA, p = 6))
#   #id.cvm
#   cob <- copula::indepTestSim(length(v3$residuals[,1]), ncol(v3$data), verbose=FALSE)
#   x1 <- id.cvm(v3)
#
#   expect_equal(round(sum(x1$B), 4), 0.8641)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 392.0223)
#   #HD = hd(x1)
#   #expect_equal(round(sum(HD$hidec), 4), -1.196476e+17)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
#
#   # mb boot
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 0)
#   expect_equal(round(bb3$root, 1), 1)
# })

test_that("id.dc works for MTS list object and analysis (bootstrap, irf, hd fevd)", {
  set.seed(23211)
  v3 <- invisible(MTS::VAR(USA, p = 6))
  #id.dc
  x1 <- id.dc(v3)

  expect_equal(sum(round(x1$B, 4)), 2.8498)
  IRF = irf(x1)
  expect_equal(round(sum(IRF$irf),4), 891.1648)
  #HD = hd(x1)
  #expect_equal(round(sum(HD$hidec), 4), -3.5012)
  FEVD = fevd(x1)
  expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))

  # mb boot
  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bb, 18)
  expect_equal(bb$nboot, 10)

  # wild boot
  bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bb2, 18)
  expect_equal(bb2$nboot, 10)

  # ba boot
  bb3 <- ba.boot(bb, nc = 1)
  expect_equal(bb3$count, 0)
  expect_equal(round(bb3$root, 1), 1)

})

# test_that("id.garch works for MTS list object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   v3 <- invisible(MTS::VAR(USA, p = 6))
#   # id.garch
#   x1 <- id.garch(v3)
#
#   expect_equal(round(x1$Lik, 1),  -547.2)
#   expect_equal(round(sum(x1$B), 3), 1.83)
#
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),3), 631.531)
#   # HD = hd(x1)
#   # expect_equal(round(sum(HD$hidec), 4), -3.5012)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
#
#   # mb boot
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 0)
#   expect_equal(round(bb3$root, 1), 1)
# })

# test_that("id.ngml works for MTS list object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   v3 <- invisible(MTS::VAR(USA, p = 6))
#   # id.ngml
#   x1 <- id.ngml(v3)
#   x2 <- id.ngml_boot(v3)
#
#   expect_equal(c(round(x1$Lik, 4),round(x2$Lik, 4)), rep(-548.1502,2))
#
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 872.4464)
#   # HD = hd(x1)
#   # expect_equal(round(sum(HD$hidec), 4), -3.5012)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
#
#   # mb boot
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 0)
#   expect_equal(round(bb3$root, 1), 1)
# })
#
# Test VECMS (vec2var, VECM)
# test_that("id.cv works for vec2var object and analysis (bootstrap, irf, hd fevd)", {
#   skip_on_cran()
#   set.seed(23211)
#   data(finland)
#   v6 <-VECM(finland, lag=2, estim="ML")
#   #id.cvm
#   cob <- copula::indepTestSim(v6$t, v6$k, verbose=FALSE)
#   x1 <- id.cvm(v6)
#
#   expect_equal(round(sum(x1$B), 4), -0.0774)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 209.9281)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 3), 5362.275)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
#
#   # mb boot
#   signrest <- list(lrm1 = c(1,1,1,1), lny = c(-1,1,1,1), lnmr = c(-1,-1,1,1), difp = c(1,-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 102)
#   expect_equal(round(bb3$root, 1), 1)
# })

#
# test_that("id.st works for vec2var object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   data(finland)
#   sjf <- finland
#   sjf.vecm <- ca.jo(sjf, ecdet = "none", type = "eigen", K = 2,
#                     spec = "longrun", season = 4)
#   v5 <- vec2var(sjf.vecm, r = 2)
#   #id.st
#   x1 <- id.st(v5)
#
#   expect_equal(round(x1$Lik, 4), 947.1035)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 209.9419)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 3), 5452.301)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
#
#   # mb boot
#   signrest <- list(lrm1 = c(1,1,1,1), lny = c(-1,1,1,1), lnmr = c(-1,-1,1,1), difp = c(1,-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 0)
#   expect_equal(round(bb3$root, 1), 1)
#
# })
#
# test_that("id.cvm works for vec2var object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   data(finland)
#   sjf <- finland
#   sjf.vecm <- ca.jo(sjf, ecdet = "none", type = "eigen", K = 2,
#                   spec = "longrun", season = 4)
#   v5 <- vec2var(sjf.vecm, r = 2)
#   #id.cvm
#   cob <- copula::indepTestSim(v5$obs, v5$K, verbose=FALSE)
#   x1 <- id.cvm(v5)
#
#   expect_equal(round(sum(x1$B), 4), -0.0847)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 207.8637)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 3), 5469.519)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
#
#   # mb boot
#   signrest <- signrest <- list(lrm1 = c(1,1,1,1), lny = c(-1,1,1,1), lnmr = c(-1,-1,1,1), difp = c(1,-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 0)
#   expect_equal(round(bb3$root, 1), 1)
# })
#
# test_that("id.dc works for vec2var object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   data(finland)
#   sjf <- finland
#   sjf.vecm <- ca.jo(sjf, ecdet = "none", type = "eigen", K = 2,
#                     spec = "longrun", season = 4)
#   v5 <- vec2var(sjf.vecm, r = 2)
#   #id.dc
#   x1 <- id.dc(v5)
#
#   expect_equal(sum(round(x1$B, 4)), 0.1272)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 210.5507)
#   # HD = hd(x1)
#   # expect_equal(round(sum(HD$hidec), 4), -3.5012)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
#
#   # mb boot
#   signrest <- list(lrm1 = c(1,1,1,1), lny = c(-1,1,1,1), lnmr = c(-1,-1,1,1), difp = c(1,-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 101)
#   expect_equal(round(bb3$root, 1), 1)
#
# })
#
# test_that("id.garch works for vec2var object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   data(finland)
#   sjf <- finland
#   sjf.vecm <- ca.jo(sjf, ecdet = "none", type = "eigen", K = 2,
#                     spec = "longrun", season = 4)
#   v5 <- vec2var(sjf.vecm, r = 2)
#   # id.garch
#   x1 <- id.garch(v5)
#
#   expect_equal(round(x1$Lik, 1),  933.9)
#   expect_equal(round(sum(x1$B), 3), 0.004)
#
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),3), 209.913)
#   # HD = hd(x1)
#   # expect_equal(round(sum(HD$hidec), 4), -3.5012)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
#
#   # mb boot
#   signrest <- list(lrm1 = c(1,1,1,1), lny = c(-1,1,1,1), lnmr = c(-1,-1,1,1), difp = c(1,-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 101)
#   expect_equal(round(bb3$root, 1), 1)
# })
#
# test_that("id.ngml works for vec2var object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   data(finland)
#   sjf <- finland
#   sjf.vecm <- ca.jo(sjf, ecdet = "none", type = "eigen", K = 2,
#                     spec = "longrun", season = 4)
#   v5 <- vec2var(sjf.vecm, r = 2)
#   # id.ngml
#   x1 <- id.ngml(v5)
#   x2 <- id.ngml_boot(v5)
#
#   expect_equal(c(round(x1$Lik, 4),round(x2$Lik, 4)), rep(933.7791,2))
#
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 208.9342)
#   # HD = hd(x1)
#   # expect_equal(round(sum(HD$hidec), 4), -3.5012)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
#
#
#   # mb boot
#   signrest <- list(lrm1 = c(1,1,1,1), lny = c(-1,1,1,1), lnmr = c(-1,-1,1,1), difp = c(1,-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 101)
#   expect_equal(round(bb3$root, 1), 1)
#
# })
#
# # test tsDyn model classes
# test_that("id.cv works for VECM object and analysis (bootstrap, irf, hd fevd)", {
#   skip_on_cran()
#   set.seed(23211)
#   data(finland)
#   v6 <-VECM(finland, lag=2, estim="ML")
#
#   #id.cv
#   x1 <- id.cv(v6, SB = 59)
#   x2 <- id.cv_boot(v6, SB = 59)
#   expect_equal(c(round(x1$Lik, 4), round(x2$Lik, 4)), rep(897.8931,2))
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 210.7234)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 3), 5349.796)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
#   # mb boot
#   signrest <- list(lrm1 = c(1,1,1,1), lny = c(-1,1,1,1), lnmr = c(-1,-1,1,1), difp = c(1,-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 109)
#   expect_equal(round(bb3$root, 1), 1)
#
#
# })
#
# test_that("id.st works for VECM object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   data(finland)
#   v6 <-VECM(finland, lag=2, estim="ML")
#   #id.st
#   x1 <- id.st(v6)
#
#   expect_equal(round(x1$Lik, 4), 909.2239)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 210.7527)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 3), 5348.792)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
#
#   # mb boot
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 111)
#   expect_equal(round(bb3$root, 1), 1)
# })
#
# test_that("id.cvm works for VECM object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   data(finland)
#   v6 <-VECM(finland, lag=2, estim="ML")
#   #id.cvm
#   cob <- copula::indepTestSim(v6$t, v6$k, verbose=FALSE)
#   x1 <- id.cvm(v6)
#
#   expect_equal(round(sum(x1$B), 4), -0.0774)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 209.9281)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 3), 5362.275)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
#
#   # mb boot
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 102)
#   expect_equal(round(bb3$root, 1), 1)
# })
#
# test_that("id.dc works for VECM object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   data(finland)
#   v6 <-VECM(finland, lag=2, estim="ML")
#   #id.dc
#   x1 <- id.dc(v6)
#
#   expect_equal(sum(round(x1$B, 3)), 0.137)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 212.1406)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 4), 5362.275)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
#
#   # mb boot
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
# })
#
# test_that("id.garch works for VECM object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   data(finland)
#   v6 <-VECM(finland, lag=2, estim="ML")
#   # id.garch
#   x1 <- id.garch(v6)
#
#   expect_equal(round(x1$Lik, 3),  877.558)
#   expect_equal(round(sum(x1$B), 3), -0.044)
#
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),3), 210.666)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 4), 5362.275)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
#
#   # mb boot
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
# })
#
# test_that("id.ngml works for VECM object and analysis (bootstrap, irf, hd fevd)", {
#   set.seed(23211)
#   data(finland)
#   v6 <-VECM(finland, lag=2, estim="ML")
#   # id.ngml
#   x1 <- id.ngml(v6)
#   #x2 <- id.ngml_boot(v6)
#
#   expect_equal(round(x1$Lik, 4), 868.0276)
#
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 211.2099)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 4), 5362.275)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
#
#   # mb boot
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bb <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb, 18)
#   expect_equal(bb$nboot, 10)
#
#   # wild boot
#   bb2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bb2, 18)
#   expect_equal(bb2$nboot, 10)
#
#   # ba boot
#   bb3 <- ba.boot(bb, nc = 1)
#   expect_equal(bb3$count, 102)
#   expect_equal(round(bb3$root, 3), 1)
# })
#

