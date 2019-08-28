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
# })
#
# test_that("id.dc works for varest object and analysis (bootstrap, irf, hd fevd)", {
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
# })
#
# test_that("id.st works for nlVars object and analysis (bootstrap, irf, hd fevd)", {
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
# })
#
# # test_that("id.cvm works for nlVars object and analysis (bootstrap, irf, hd fevd)", {
# #   #id.cvm
# #   x1 <- id.cvm(v1)
# #
# #   expect_equal(round(sum(x1$B),3), 2.498)
# #   IRF = irf(x1)
# #   expect_equal(round(sum(IRF$irf),4), 231.1769)
# #   HD = hd(x1)
# #   expect_equal(round(sum(HD$hidec), 4), -3.5012)
# #   FEVD = fevd(x1)
# #   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
# # })
#
# test_that("id.dc works for nlVars object and analysis (bootstrap, irf, hd fevd)", {
#   #id.dc
#   x1 <- id.dc(v1)
#
#   expect_equal(sum(round(x1$B, 4)), 2.8497)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 238.629)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 4), -3.5012)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
#
# })
#
# test_that("id.garch works for nlVars object and analysis (bootstrap, irf, hd fevd)", {
#   # id.garch
#   x1 <- id.garch(v1)
#
#   expect_equal(round(x1$Lik, 1),  -547.2)
#   expect_equal(round(sum(x1$B), 2), 1.83)
#
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),2), 219.62)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 4), -3.5012)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
# })
#
# test_that("id.ngml works for nlVars object and analysis (bootstrap, irf, hd fevd)", {
#   # id.ngml
#   x1 <- id.ngml(v1)
#   x2 <- id.ngml_boot(v1)
#
#   expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -548.1502)
#
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 239.0778)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 4), -3.5012)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
#   })
#
#
# ## test MTS model classes
# test_that("id.cv works for MTS list object and analysis (bootstrap, irf, hd fevd)", {
#   skip_on_cran()
#   set.seed(23211)
#   v3 <- invisible(MTS::VAR(USA, p = 6))
#
#   #id.cv
#   x3 <- id.cv(v3, SB = 59)
#   x4 <- id.cv_boot(v1, SB = 59)
#   expect_equal(round(x3$Lik, 4), round(x4$Lik, 4), -564.2994)
#   IRF = irf(x3)
#   expect_equal(round(sum(IRF$irf),4), 241.5486)
#   HD = hd(x3)
#   expect_equal(round(sum(HD$hidec), 4), 63.635)
#   FEVD = fevd(x3)
#   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
# })
#
# test_that("id.st works for MTS list object and analysis (bootstrap, irf, hd fevd)", {
#   #id.st
#   x1 <- id.st(v3)
#
#   expect_equal(round(x1$Lik, 4), -508.7511)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 248.6993)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 4), 2.0775)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
# })
#
# # test_that("id.cvm works for MTS list object and analysis (bootstrap, irf, hd fevd)", {
# #   #id.cvm
# #   x1 <- id.cvm(v3)
# #
# #   expect_equal(round(sum(x1$B), 4), -0.4718)
# #   IRF = irf(x1)
# #   expect_equal(round(sum(IRF$irf),4), 28.3943)
# #   # HD = hd(x1)
# #   # expect_equal(round(sum(HD$hidec), 4), -1.196476e+17)
# #   FEVD = fevd(x1)
# #   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
# # })
#
# test_that("id.dc works for MTS list object and analysis (bootstrap, irf, hd fevd)", {
#   #id.dc
#   x1 <- id.dc(v3)
#
#   expect_equal(sum(round(x1$B, 4)), 2.8498)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 891.1648)
#   # HD = hd(x1)
#   # expect_equal(round(sum(HD$hidec), 4), -3.5012)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$x),sum(FEVD$pi),sum(FEVD$i)), c(1000,1000,1000))
#
# })
#
# test_that("id.garch works for MTS list object and analysis (bootstrap, irf, hd fevd)", {
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
# })
#
# test_that("id.ngml works for MTS list object and analysis (bootstrap, irf, hd fevd)", {
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
#
# })
#
# # test VECMs (vec2var, VECM)
#
# ## test var model classes
# test_that("id.cv works for vec2var object and analysis (bootstrap, irf, hd fevd)", {
#   skip_on_cran()
#   set.seed(23211)
#   data(finland)
#   sjf <- finland
#
#
#   sjf.vecm <- ca.jo(sjf, ecdet = "none", type = "eigen", K = 2,
#                     spec = "longrun", season = 4)
#   v5 <- vec2var(sjf.vecm, r = 2)
#
#   #id.cv
#   x1 <- id.cv(v5, SB = 59)
#   x2 <- id.cv_boot(v5, SB = 59)
#   expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), 891.2414)
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 210.6978)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 3), 5452.361)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
# })
#
# test_that("id.st works for vec2var object and analysis (bootstrap, irf, hd fevd)", {
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
# })
#
# # test_that("id.cvm works for vec2var object and analysis (bootstrap, irf, hd fevd)", {
# #   #id.cvm
# #   x1 <- id.cvm(v5)
# #
# #   expect_equal(round(sum(x1$B), 4), -0.0387)
# #   IRF = irf(x1)
# #   expect_equal(round(sum(IRF$irf),4), 208.7843)
# #   HD = hd(x1)
# #   expect_equal(round(sum(HD$hidec), 3), 5469.519)
# #   FEVD = fevd(x1)
# #   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
# # })
#
# test_that("id.dc works for vec2var object and analysis (bootstrap, irf, hd fevd)", {
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
# })
#
# test_that("id.garch works for vec2var object and analysis (bootstrap, irf, hd fevd)", {
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
# })
#
# test_that("id.ngml works for vec2var object and analysis (bootstrap, irf, hd fevd)", {
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
#
#
# })
#
# ## test tsDyn model classes
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
#
# })
#
# test_that("id.st works for VECM object and analysis (bootstrap, irf, hd fevd)", {
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
# })
#
# # test_that("id.cvm works for VECM object and analysis (bootstrap, irf, hd fevd)", {
# #   #id.cvm
# #   x1 <- id.cvm(v6)
# #
# #   expect_equal(round(sum(x1$B), 4), -0.1596)
# #   IRF = irf(x1)
# #   expect_equal(round(sum(IRF$irf),4), 208.5466)
# #   HD = hd(x1)
# #   expect_equal(round(sum(HD$hidec), 3), 5362.275)
# #   FEVD = fevd(x1)
# #   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
# # })
#
# test_that("id.dc works for VECM object and analysis (bootstrap, irf, hd fevd)", {
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
# })
#
# test_that("id.garch works for VECM object and analysis (bootstrap, irf, hd fevd)", {
#   # id.garch
#   x1 <- id.garch(v6)
#
#   expect_equal(round(x1$Lik, 1),  877.7)
#   expect_equal(round(sum(x1$B), 3), -0.041)
#
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),3), 210.717)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 4), 5362.275)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
# })
#
# test_that("id.ngml works for VECM object and analysis (bootstrap, irf, hd fevd)", {
#   # id.ngml
#   x1 <- id.ngml(v6)
#   x2 <- id.ngml_boot(v6)
#
#   expect_equal(c(round(x1$Lik, 4),round(x2$Lik, 4)), rep(868.0276,2))
#
#   IRF = irf(x1)
#   expect_equal(round(sum(IRF$irf),4), 211.2099)
#   HD = hd(x1)
#   expect_equal(round(sum(HD$hidec), 4), 5362.275)
#   FEVD = fevd(x1)
#   expect_equal(c(sum(FEVD$lrm1),sum(FEVD$lnmr), sum(FEVD$lny), sum(FEVD$difp)), c(1000,1000,1000, 1000))
# })

