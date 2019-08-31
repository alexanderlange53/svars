context("get_restriction_matrix.R")

test_that("get_restriction_matrix works with k*k matrix", {
  restMat <- matrix(rep(NA, 9), ncol = 3)
  restMat[1,3] <- 0

  R <- get_restriction_matrix(restMat, 3)

  expect_equal(dim(R)[1], 3)
  expect_equal(dim(R)[2], 3)

  expect_equal(R[1,3], 0)
  expect_equal(all(is.na(R[-7])), TRUE)
})

test_that("get_restriction_matrix works with NULL", {
  k = 3
  R <- get_restriction_matrix(NULL, k)

  expect_equal(R, matrix(rep(NA, k*k), k,k))
})

test_that("get_restriction_matrix works with K^2*K^2 (J = 1) matrix", {
  restMat <- diag(rep(1,9))
  restMat[7,7]= 0
  R <- get_restriction_matrix(restMat, 3)

  expect_equal(dim(R)[1], 3)
  expect_equal(dim(R)[2], 3)

  expect_equal(R[1,3], 0)
  expect_equal(all(is.na(R[-7])), TRUE)
})



test_that("get_restriction_matrix works with k^2*k^2 (J = 3) matrix", {
  restMat <- diag(rep(1,9))
  restMat[7,7] = 0
  restMat[4,4] = 0
  restMat[8,8] = 0


  R <- get_restriction_matrix(restMat, 3)

  expect_equal(dim(R)[1], 3)
  expect_equal(dim(R)[2], 3)

  expect_equal(R[1,2], 0)
  expect_equal(R[1,3], 0)
  expect_equal(R[2,3], 0)
  expect_equal(all(is.na(R[-c(4,7,8)])), TRUE)
})

