# context("test-mb_boot.R")
#
# test_that("mb.boot returns valid object for id.dc", {
#   skip_on_cran()
#   set.seed(23211)
#   v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC")
#   x1 <- id.dc(v1)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbdc <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbdc, 20)
#   expect_equal(bbdc$nboot, 10)
#
#   bbdc1 <- mb.boot(x1, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc1, 20)
#   expect_equal(bbdc1$nboot, 10)
#
#   bbdc2 <- mb.boot(x1, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbdc2, 20)
#   expect_equal(bbdc2$nboot, 10)
#
#   bbdc3 <- mb.boot(x1, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc3, 20)
#   expect_equal(bbdc3$nboot, 10)
#
#   v2 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "trend")
#   x2 <- id.dc(v2)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbdc4 <- mb.boot(x2, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbdc4, 20)
#   expect_equal(bbdc4$nboot, 10)
#
#   bbdc5 <- mb.boot(x2, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc5, 20)
#   expect_equal(bbdc5$nboot, 10)
#
#   bbdc6 <- mb.boot(x2, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbdc6, 20)
#   expect_equal(bbdc6$nboot, 10)
#
#   bbdc7 <- mb.boot(x2, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc7, 20)
#   expect_equal(bbdc7$nboot, 10)
#
#   v3 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "both")
#   x3 <- id.dc(v3)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbdc8 <- mb.boot(x3, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbdc8, 20)
#   expect_equal(bbdc8$nboot, 10)
#
#   bbdc9 <- mb.boot(x3, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc9, 20)
#   expect_equal(bbdc9$nboot, 10)
#
#   bbdc10 <- mb.boot(x3, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbdc10, 20)
#   expect_equal(bbdc10$nboot, 10)
#
#   bbdc11 <- mb.boot(x3, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc11, 20)
#   expect_equal(bbdc11$nboot, 10)
#
#   v4 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "none")
#   x4 <- id.dc(v4)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbdc12 <- mb.boot(x4, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbdc12, 20)
#   expect_equal(bbdc12$nboot, 10)
#
#   bbdc13 <- mb.boot(x4, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc13, 20)
#   expect_equal(bbdc13$nboot, 10)
#
#   bbdc14 <- mb.boot(x4, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbdc14, 20)
#   expect_equal(bbdc14$nboot, 10)
#
#   bbdc15 <- mb.boot(x4, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc15, 20)
#   expect_equal(bbdc15$nboot, 10)
#
#   x1 <- id.dc(v1, PIT = FALSE)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbdc16 <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbdc16, 20)
#   expect_equal(bbdc16$nboot, 10)
#
#   bbdc18 <- mb.boot(x1, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc18, 20)
#   expect_equal(bbdc18$nboot, 10)
#
#   bbdc18 <- mb.boot(x1, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc18, 20)
#   expect_equal(bbdc18$nboot, 10)
#
#   bbdc20 <- mb.boot(x1, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbdc20, 20)
#   expect_equal(bbdc20$nboot, 10)
#
#   bbdc20 <- mb.boot(x1, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc20, 20)
#   expect_equal(bbdc20$nboot, 10)
#
#   x2 <- id.dc(v2, PIT = FALSE)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbdc21 <- mb.boot(x2, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbdc21, 20)
#   expect_equal(bbdc21$nboot, 10)
#
#   bbdc22 <- mb.boot(x2, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc22, 20)
#   expect_equal(bbdc22$nboot, 10)
#
#   bbdc23 <- mb.boot(x2, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc23, 20)
#   expect_equal(bbdc23$nboot, 10)
#
#   bbdc24 <- mb.boot(x2, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbdc24, 20)
#   expect_equal(bbdc24$nboot, 10)
#
#   bbdc25 <- mb.boot(x2, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc25, 20)
#   expect_equal(bbdc25$nboot, 10)
#
#   x3 <- id.dc(v3, PIT = FALSE)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbdc26 <- mb.boot(x3, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbdc26, 20)
#   expect_equal(bbdc26$nboot, 10)
#
#   bbdc27 <- mb.boot(x3, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc27, 20)
#   expect_equal(bbdc27$nboot, 10)
#
#   bbdc27 <- mb.boot(x3, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc27, 20)
#   expect_equal(bbdc27$nboot, 10)
#
#   bbdc227 <- mb.boot(x3, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbdc27, 20)
#   expect_equal(bbdc27$nboot, 10)
#
#   bbdc28 <- mb.boot(x3, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc28, 20)
#   expect_equal(bbdc28$nboot, 10)
#
#   x4 <- id.dc(v4, PIT = FALSE)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbdc29 <- mb.boot(x4, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbdc29, 20)
#   expect_equal(bbdc29$nboot, 10)
#
#   bbdc30 <- mb.boot(x4, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc30, 20)
#   expect_equal(bbdc30$nboot, 10)
#
#   bbdc31 <- mb.boot(x4, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc31, 20)
#   expect_equal(bbdc31$nboot, 10)
#
#   bbdc32 <- mb.boot(x4, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbdc32, 20)
#   expect_equal(bbdc32$nboot, 10)
#
#   bbdc33 <- mb.boot(x4, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbdc33, 20)
#   expect_equal(bbdc33$nboot, 10)
# })
#
# test_that("mb.boot returns valid object for id.cv", {
#   skip_on_cran()
#   set.seed(231)
#   v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#   x1 <- id.cv(v1, SB = 59)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbcv <- mb.boot(x1, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbcv, 20)
#   expect_equal(bbcv$nboot, 10)
#
#   bbcv1 <- mb.boot(x1, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbcv1, 20)
#   expect_equal(bbcv1$nboot, 10)
#
#   bbcv2 <- mb.boot(x1, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbcv2, 20)
#   expect_equal(bbcv2$nboot, 10)
#
#   bbcv3 <- mb.boot(x1, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbcv3, 20)
#   expect_equal(bbcv3$nboot, 10)
#
#   v2 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "trend")
#   x2 <- id.cv(v2, SB = 59)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbcv4 <- mb.boot(x2, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbcv4, 20)
#   expect_equal(bbcv4$nboot, 10)
#
#   bbcv5 <- mb.boot(x2, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbcv5, 20)
#   expect_equal(bbcv5$nboot, 10)
#
#   bbcv6 <- mb.boot(x2, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbcv6, 20)
#   expect_equal(bbcv6$nboot, 10)
#
#   bbcv7 <- mb.boot(x2, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbcv7, 20)
#   expect_equal(bbcv7$nboot, 10)
#
#   v3 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "both")
#   x3 <- id.cv(v3, SB = 59)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbcv8 <- mb.boot(x3, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbcv8, 20)
#   expect_equal(bbcv8$nboot, 10)
#
#   bbcv9 <- mb.boot(x3, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbcv9, 20)
#   expect_equal(bbcv9$nboot, 10)
#
#   bbcv10 <- mb.boot(x3, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbcv10, 20)
#   expect_equal(bbcv10$nboot, 10)
#
#   bbcv11 <- mb.boot(x3, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbcv11, 20)
#   expect_equal(bbcv11$nboot, 10)
#
#   v4 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "none")
#   x4 <- id.cv(v4, SB = 59)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbcv12 <- mb.boot(x4, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbcv12, 20)
#   expect_equal(bbcv12$nboot, 10)
#
#   bbcv13 <- mb.boot(x4, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbcv13, 20)
#   expect_equal(bbcv13$nboot, 10)
#
#   bbcv14 <- mb.boot(x4, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbcv14, 20)
#   expect_equal(bbcv14$nboot, 10)
#
#   bbcv15 <- mb.boot(x4, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbcv15, 20)
#   expect_equal(bbcv15$nboot, 10)
#
#   restmat <- matrix(NA, 3,3)
#   restmat[1,c(2,3)] <- 0
#   restmat[2,3] <- 0
#   x1 <- id.cv(v1, SB = 59, restriction_matrix = restmat)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbcv16 <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbcv16, 20)
#   expect_equal(bbcv16$nboot, 10)
#
#   bbcv18 <- mb.boot(x1, design ="fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbcv18, 20)
#   expect_equal(bbcv18$nboot, 10)
#
#   x2 <- id.cv(v2, SB = 59, restriction_matrix = restmat)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbcv18 <- mb.boot(x2, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbcv18, 20)
#   expect_equal(bbcv18$nboot, 10)
#
#   bbcv20 <- mb.boot(x2, design ="fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbcv20, 20)
#   expect_equal(bbcv20$nboot, 10)
#
#   x3 <- id.cv(v3, SB = 59, restriction_matrix = restmat)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbcv20 <- mb.boot(x3, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbcv20, 20)
#   expect_equal(bbcv20$nboot, 10)
#
#   bbcv21 <- mb.boot(x3, design ="fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbcv21, 20)
#   expect_equal(bbcv21$nboot, 10)
#
#   x4 <- id.cv(v4, SB = 59, restriction_matrix = restmat)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbcv22 <- mb.boot(x4, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbcv22, 20)
#   expect_equal(bbcv22$nboot, 10)
#
#   bbcv23 <- mb.boot(x4, design ="fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbcv23, 20)
#   expect_equal(bbcv23$nboot, 10)
#
#   # With vector as input
#   SB <- rep(0, v1$obs)
#   SB[50:80] <- 1
#   SB[100:110] <- 1
#
#   x1 <- id.cv(v1, SB = SB)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbcv24 <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbcv24, 20)
#   expect_equal(bbcv24$nboot, 10)
#
#   bbcv25 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbcv25, 20)
#   expect_equal(bbcv25$nboot, 10)
#
#   x2 <- id.cv(v2, SB = SB)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbcv26 <- mb.boot(x2, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbcv26, 20)
#   expect_equal(bbcv26$nboot, 10)
#
#   bbcv27 <- mb.boot(x2, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbcv27, 20)
#   expect_equal(bbcv27$nboot, 10)
#
#   x3 <- id.cv(v3, SB = SB)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbcv28 <- mb.boot(x3, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbcv28, 20)
#   expect_equal(bbcv28$nboot, 10)
#
#   bbcv29 <- mb.boot(x3, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbcv29, 20)
#   expect_equal(bbcv29$nboot, 10)
#
#   x4 <- id.cv(v4, SB = SB)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbcv30 <- mb.boot(x4, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbcv30, 20)
#   expect_equal(bbcv30$nboot, 10)
#
#   bbcv31 <- mb.boot(x4, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbcv31, 20)
#   expect_equal(bbcv31$nboot, 10)
# })
#
# test_that("mb.boot returns valid object for id.st", {
#   skip_on_cran()
#   set.seed(231)
#   v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#   x1 <- id.st(v1, c_fix = 80, gamma_fix = -1)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbst <- mb.boot(x1, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbst, 20)
#   expect_equal(bbst$nboot, 10)
#
#   bbst1 <- mb.boot(x1, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbst1, 20)
#   expect_equal(bbst1$nboot, 10)
#
#   bbst2 <- mb.boot(x1, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbst2, 20)
#   expect_equal(bbst2$nboot, 10)
#
#   bbst3 <- mb.boot(x1, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbst3, 20)
#   expect_equal(bbst3$nboot, 10)
#
#   v2 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "trend")
#   x2 <- id.st(v2, c_fix = 80, gamma_fix = -1)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbst4 <- mb.boot(x2, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbst, 20)
#   expect_equal(bbst$nboot, 10)
#
#   bbst4 <- mb.boot(x2, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbst4, 20)
#   expect_equal(bbst4$nboot, 10)
#
#   bbst5 <- mb.boot(x2, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbst5, 20)
#   expect_equal(bbst5$nboot, 10)
#
#   bbst6 <- mb.boot(x2, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbst6, 20)
#   expect_equal(bbst6$nboot, 10)
#
#   v3 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "both")
#   x3 <- id.st(v3, c_fix = 80, gamma_fix = -1)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbst7 <- mb.boot(x3, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbst7, 20)
#   expect_equal(bbst7$nboot, 10)
#
#   bbst8 <- mb.boot(x3, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbst8, 20)
#   expect_equal(bbst8$nboot, 10)
#
#   bbst9 <- mb.boot(x3, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbst9, 20)
#   expect_equal(bbst9$nboot, 10)
#
#   bbst10 <- mb.boot(x3, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbst10, 20)
#   expect_equal(bbst10$nboot, 10)
#
#   v4 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "none")
#   x4 <- id.st(v4, c_fix = 80, gamma_fix = -1)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbst11 <- mb.boot(x4, b.length = 20, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbst11, 20)
#   expect_equal(bbst11$nboot, 10)
#
#   bbst12 <- mb.boot(x4, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbst12, 20)
#   expect_equal(bbst12$nboot, 10)
#
#   bbst13 <- mb.boot(x4, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbst13, 20)
#   expect_equal(bbst13$nboot, 10)
#
#   bbst14 <- mb.boot(x4, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbst14, 20)
#   expect_equal(bbst14$nboot, 10)
#
#   restmat <- matrix(NA, 3,3)
#   restmat[1,c(2,3)] <- 0
#   restmat[2,3] <- 0
#   x1 <- id.st(v1, c_fix = 80, gamma_fix = -1, restriction_matrix = restmat)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbst15 <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbst15, 20)
#   expect_equal(bbst15$nboot, 10)
#
#   bbst16 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbst16, 20)
#   expect_equal(bbst16$nboot, 10)
#
#   x2 <- id.st(v2, c_fix = 80, gamma_fix = -1, restriction_matrix = restmat)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbst18 <- mb.boot(x2, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbst18, 20)
#   expect_equal(bbst18$nboot, 10)
#
#   bbst18 <- mb.boot(x2, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbst18, 20)
#   expect_equal(bbst18$nboot, 10)
#
#   x3 <- id.st(v3, c_fix = 80, gamma_fix = -1, restriction_matrix = restmat)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbst20 <- mb.boot(x3, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbst20, 20)
#   expect_equal(bbst20$nboot, 10)
#
#   bbst20 <- mb.boot(x3, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbst20, 20)
#   expect_equal(bbst20$nboot, 10)
#
#   x4 <- id.st(v4, c_fix = 80, gamma_fix = -1, restriction_matrix = restmat)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbst21 <- mb.boot(x4, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbst21, 20)
#   expect_equal(bbst21$nboot, 10)
#
#   bbst22 <- mb.boot(x4, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbst22, 20)
#   expect_equal(bbst22$nboot, 10)
# })
#
# test_that("mb.boot returns valid object for id.ngml", {
#   skip_on_cran()
#   set.seed(23211)
#   v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#   x1 <- id.ngml(v1)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbngml <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbngml, 20)
#   expect_equal(bbngml$nboot, 10)
#
#   bbngml1 <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml1, 20)
#   expect_equal(bbngml1$nboot, 10)
#
#   bbngml2 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbngml2, 20)
#   expect_equal(bbngml2$nboot, 10)
#
#   bbngml3 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml3, 20)
#   expect_equal(bbngml3$nboot, 10)
#
#   v2 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "trend")
#   x2 <- id.ngml(v2)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbngml4 <- mb.boot(x2, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbngml4, 20)
#   expect_equal(bbngml4$nboot, 10)
#
#   bbngml5 <- mb.boot(x2, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml5, 20)
#   expect_equal(bbngml5$nboot, 10)
#
#   bbngml6 <- mb.boot(x2, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbngml6, 20)
#   expect_equal(bbngml6$nboot, 10)
#
#   bbngml7 <- mb.boot(x2, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml7, 20)
#   expect_equal(bbngml7$nboot, 10)
#
#   v3 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "both")
#   x3 <- id.ngml(v3)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbngml8 <- mb.boot(x3, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbngml8, 20)
#   expect_equal(bbngml8$nboot, 10)
#
#   bbngml9 <- mb.boot(x3, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml9, 20)
#   expect_equal(bbngml9$nboot, 10)
#
#   bbngml10 <- mb.boot(x3, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbngml10, 20)
#   expect_equal(bbngml10$nboot, 10)
#
#   bbngml11 <- mb.boot(x3, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml11, 20)
#   expect_equal(bbngml11$nboot, 10)
#
#   v4 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "none")
#   x4 <- id.ngml(v4)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbngml12 <- mb.boot(x4, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbngml12, 20)
#   expect_equal(bbngml12$nboot, 10)
#
#   bbngml13 <- mb.boot(x4, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml13, 20)
#   expect_equal(bbngml13$nboot, 10)
#
#   bbngml14 <- mb.boot(x4, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbngml14, 20)
#   expect_equal(bbngml14$nboot, 10)
#
#   bbngml15 <- mb.boot(x4, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml15, 20)
#   expect_equal(bbngml15$nboot, 10)
#
#   x1 <- id.ngml(v1, stage3 = TRUE)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbngml16 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbngml16, 20)
#   expect_equal(bbngml16$nboot, 5)
#
#   bbngml18 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml18, 20)
#   expect_equal(bbngml18$nboot, 5)
#
#   bbngml18 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml18, 20)
#   expect_equal(bbngml18$nboot, 5)
#
#   bbngml20 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbngml20, 20)
#   expect_equal(bbngml20$nboot, 5)
#
#   bbngml20 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml20, 20)
#   expect_equal(bbngml20$nboot, 5)
#
#   x2 <- id.ngml(v2, stage3 = TRUE)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbngml21 <- mb.boot(x2, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbngml21, 20)
#   expect_equal(bbngml21$nboot, 5)
#
#   bbngml22 <- mb.boot(x2, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml22, 20)
#   expect_equal(bbngml22$nboot, 5)
#
#   bbngml23 <- mb.boot(x2, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml23, 20)
#   expect_equal(bbngml23$nboot, 5)
#
#   bbngml24 <- mb.boot(x2, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbngml24, 20)
#   expect_equal(bbngml24$nboot, 5)
#
#   bbngml25 <- mb.boot(x2, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml25, 20)
#   expect_equal(bbngml25$nboot, 5)
#
#   x3 <- id.ngml(v3, stage3 = TRUE)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbngml26 <- mb.boot(x3, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbngml26, 20)
#   expect_equal(bbngml26$nboot, 5)
#
#   bbngml27 <- mb.boot(x3, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml27, 20)
#   expect_equal(bbngml27$nboot, 5)
#
#   bbngml28 <- mb.boot(x3, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml28, 20)
#   expect_equal(bbngml28$nboot, 5)
#
#   bbngml29 <- mb.boot(x3, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbngml29, 20)
#   expect_equal(bbngml29$nboot, 5)
#
#   bbngml30 <- mb.boot(x3, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml30, 20)
#   expect_equal(bbngml30$nboot, 5)
#
#   x4 <- id.ngml(v4, stage3 = TRUE)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbngml31 <- mb.boot(x4, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbngml31, 20)
#   expect_equal(bbngml31$nboot, 5)
#
#   bbngml32 <- mb.boot(x4, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml32, 20)
#   expect_equal(bbngml32$nboot, 5)
#
#   bbngml33 <- mb.boot(x4, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml33, 20)
#   expect_equal(bbngml33$nboot, 5)
#
#   bbngml34 <- mb.boot(x4, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbngml34, 20)
#   expect_equal(bbngml34$nboot, 5)
#
#   bbngml35 <- mb.boot(x4, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml35, 20)
#   expect_equal(bbngml35$nboot, 5)
#
#   restmat <- matrix(NA, 3,3)
#   restmat[1,c(2,3)] <- 0
#   restmat[2,3] <- 0
#
#   x1 <- id.ngml(v1, stage3 = F, restriction_matrix = restmat)
#
#   bbngml36 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml36, 20)
#   expect_equal(bbngml36$nboot, 5)
#
#   bbngml37 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml37, 20)
#   expect_equal(bbngml37$nboot, 5)
#
#   bbngml38 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml38, 20)
#   expect_equal(bbngml38$nboot, 5)
#
#   x2 <- id.ngml(v2, stage3 = F, restriction_matrix = restmat)
#
#   bbngml39 <- mb.boot(x2, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml39, 20)
#   expect_equal(bbngml39$nboot, 5)
#
#   bbngml40 <- mb.boot(x2, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml40, 20)
#   expect_equal(bbngml40$nboot, 5)
#
#   bbngml41 <- mb.boot(x2, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml41, 20)
#   expect_equal(bbngml41$nboot, 5)
#
#   x3 <- id.ngml(v3, stage3 = F, restriction_matrix = restmat)
#
#   bbngml42 <- mb.boot(x3, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml42, 20)
#   expect_equal(bbngml42$nboot, 5)
#
#   bbngml43 <- mb.boot(x3, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml43, 20)
#   expect_equal(bbngml43$nboot, 5)
#
#   bbngml44 <- mb.boot(x3, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml44, 20)
#   expect_equal(bbngml44$nboot, 5)
#
#   x4 <- id.ngml(v4, stage3 = F, restriction_matrix = restmat)
#
#   bbngml45 <- mb.boot(x4, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml45, 20)
#   expect_equal(bbngml45$nboot, 5)
#
#   bbngml46 <- mb.boot(x4, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml46, 20)
#   expect_equal(bbngml46$nboot, 5)
#
#   bbngml47 <- mb.boot(x4, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml47, 20)
#   expect_equal(bbngml47$nboot, 5)
#
#   restmat <- matrix(NA, 3,3)
#   restmat[1,c(2,3)] <- 0
#   restmat[2,3] <- 0
#
#   x1 <- id.ngml(v1, stage3 = T, restriction_matrix = restmat)
#
#   bbngml48 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml48, 20)
#   expect_equal(bbngml48$nboot, 5)
#
#   bbngml49 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml49, 20)
#   expect_equal(bbngml49$nboot, 5)
#
#   bbngml50 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml50, 20)
#   expect_equal(bbngml50$nboot, 5)
#
#   x2 <- id.ngml(v2, stage3 = T, restriction_matrix = restmat)
#
#   bbngml51 <- mb.boot(x2, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml51, 20)
#   expect_equal(bbngml51$nboot, 5)
#
#   bbngml52 <- mb.boot(x2, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml52, 20)
#   expect_equal(bbngml52$nboot, 5)
#
#   bbngml53 <- mb.boot(x2, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml53, 20)
#   expect_equal(bbngml53$nboot, 5)
#
#   x3 <- id.ngml(v3, stage3 = T, restriction_matrix = restmat)
#
#   bbngml54 <- mb.boot(x3, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml54, 20)
#   expect_equal(bbngml54$nboot, 5)
#
#   bbngml55 <- mb.boot(x3, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml55, 20)
#   expect_equal(bbngml55$nboot, 5)
#
#   bbngml56 <- mb.boot(x3, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml56, 20)
#   expect_equal(bbngml56$nboot, 5)
#
#   x4 <- id.ngml(v4, stage3 = T, restriction_matrix = restmat)
#
#   bbngml57 <- mb.boot(x4, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml57, 20)
#   expect_equal(bbngml57$nboot, 5)
#
#   bbngml58 <- mb.boot(x4, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml58, 20)
#   expect_equal(bbngml58$nboot, 5)
#
#   bbngml59 <- mb.boot(x4, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbngml59, 20)
#   expect_equal(bbngml59$nboot, 5)
# })
#
# test_that("mb.boot returns valid object for id.garch", {
#   skip_on_cran()
#   set.seed(231)
#   v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#   x1 <- id.garch(v1)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbgarch <- mb.boot(x1, b.length = 15, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbgarch, 20)
#   expect_equal(bbgarch$nboot, 10)
#
#   bbgarch1 <- mb.boot(x1, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbgarch1, 20)
#   expect_equal(bbgarch1$nboot, 10)
#
#   bbgarch2 <- mb.boot(x1, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbgarch2, 20)
#   expect_equal(bbgarch2$nboot, 10)
#
#   bbgarch3 <- mb.boot(x1, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbgarch3, 20)
#   expect_equal(bbgarch3$nboot, 10)
#
#   v2 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "trend")
#   x2 <- id.garch(v2)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbgarch4 <- mb.boot(x2, b.length = 15, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbgarch4, 20)
#   expect_equal(bbgarch4$nboot, 10)
#
#   bbgarch5 <- mb.boot(x2, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbgarch5, 20)
#   expect_equal(bbgarch5$nboot, 10)
#
#   bbgarch5 <- mb.boot(x2, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbgarch5, 20)
#   expect_equal(bbgarch5$nboot, 10)
#
#   bbgarch6 <- mb.boot(x2, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbgarch6, 20)
#   expect_equal(bbgarch6$nboot, 10)
#
#   v3 <- vars::VAR(USA, lag.max = 10, ic = "AIC", type = "both")
#   x3 <- id.garch(v3)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbgarch7 <- mb.boot(x3, b.length = 15, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbgarch7, 20)
#   expect_equal(bbgarch7$nboot, 10)
#
#   bbgarch8 <- mb.boot(x3, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbgarch8, 20)
#   expect_equal(bbgarch8$nboot, 10)
#
#   bbgarch9 <- mb.boot(x3, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbgarch9, 20)
#   expect_equal(bbgarch9$nboot, 10)
#
#   bbgarch10 <- mb.boot(x3, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbgarch10, 20)
#   expect_equal(bbgarch10$nboot, 10)
#
#   v4 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#   x4 <- id.garch(v4)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbgarch11 <- mb.boot(x4, b.length = 15, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbgarch11, 20)
#   expect_equal(bbgarch11$nboot, 10)
#
#   bbgarch12 <- mb.boot(x4, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbgarch12, 20)
#   expect_equal(bbgarch12$nboot, 10)
#
#   bbgarch13 <- mb.boot(x4, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbgarch13, 20)
#   expect_equal(bbgarch13$nboot, 10)
#
#   bbgarch14 <- mb.boot(x4, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
#   expect_length(bbgarch14, 20)
#   expect_equal(bbgarch14$nboot, 10)
#
#   restmat <- matrix(NA, 3,3)
#   restmat[1,c(2,3)] <- 0
#   restmat[2,3] <- 0
#   x1 <- id.garch(v1, restriction_matrix = restmat)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbgarch15 <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbgarch15, 20)
#   expect_equal(bbgarch15$nboot, 10)
#
#   bbgarch16 <- mb.boot(x1, design = 'fixed', b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbgarch16, 20)
#   expect_equal(bbgarch16$nboot, 10)
#
#   x2 <- id.garch(v2, restriction_matrix = restmat)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbgarch18 <- mb.boot(x2, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbgarch18, 20)
#   expect_equal(bbgarch18$nboot, 10)
#
#   bbgarch18 <- mb.boot(x2, design = 'fixed', b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbgarch18, 20)
#   expect_equal(bbgarch18$nboot, 10)
#
#   x3 <- id.garch(v3, restriction_matrix = restmat)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbgarch20 <- mb.boot(x3, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbgarch20, 20)
#   expect_equal(bbgarch20$nboot, 10)
#
#   bbgarch20 <- mb.boot(x3, design = 'fixed', b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbgarch20, 20)
#   expect_equal(bbgarch20$nboot, 10)
#
#   x4 <- id.garch(v4, restriction_matrix = restmat)
#
#   signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#   bbgarch21 <- mb.boot(x4, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#
#   expect_length(bbgarch21, 20)
#   expect_equal(bbgarch21$nboot, 10)
#
#   bbgarch22 <- mb.boot(x4, design = 'fixed', b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
#   expect_length(bbgarch22, 20)
#   expect_equal(bbgarch22$nboot, 10)
# })
