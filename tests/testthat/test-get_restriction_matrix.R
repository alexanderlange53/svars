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

test_that("get_restriction_matrix works with J*k^2 (J = 1) vector", {
  restMat <- restMat <- c(0,0,0, 0,0,0, 1,0,0)

  R <- get_restriction_matrix(restMat, 3)

  expect_equal(dim(R)[1], 3)
  expect_equal(dim(R)[2], 3)

  expect_equal(R[1,3], 0)
  expect_equal(all(is.na(R[-7])), TRUE)
})

test_that("get_restriction_matrix works with J*k^2 (J = 1) matrix", {
  restMat <- restMat <- matrix(c(0,0,0, 0,0,0, 1,0,0), nrow = 1)

  R <- get_restriction_matrix(restMat, 3)

  expect_equal(dim(R)[1], 3)
  expect_equal(dim(R)[2], 3)

  expect_equal(R[1,3], 0)
  expect_equal(all(is.na(R[-7])), TRUE)
})

test_that("get_restriction_matrix works with J*k^2 (J = 3) matrix", {
  restMat <- matrix(c(0,0,0, 0,0,0, 1,0,0,
                      0,0,0, 0,0,0, 0,1,0,
                      0,0,0, 1,0,0, 0,0,0), byrow = T, nrow = 3)

  R <- get_restriction_matrix(restMat, 3)

  expect_equal(dim(R)[1], 3)
  expect_equal(dim(R)[2], 3)

  expect_equal(R[1,2], 0)
  expect_equal(R[1,3], 0)
  expect_equal(R[2,3], 0)
  expect_equal(all(is.na(R[-c(4,7,8)])), TRUE)
})

test_that("get_restriction_matrix works with k^2*J (J = 1) matrix", {
  restMat <- restMat <- matrix(c(0,0,0, 0,0,0, 1,0,0), ncol = 1)

  R <- get_restriction_matrix(restMat, 3)

  expect_equal(dim(R)[1], 3)
  expect_equal(dim(R)[2], 3)

  expect_equal(R[1,3], 0)
  expect_equal(all(is.na(R[-7])), TRUE)
})

test_that("get_restriction_matrix works with k^2*J (J = 3) matrix", {
  restMat <- matrix(c(0,0,0, 0,0,0, 1,0,0,
                      0,0,0, 0,0,0, 0,1,0,
                      0,0,0, 1,0,0, 0,0,0), byrow = T, nrow = 3)
  restMat <- t(restMat)

  R <- get_restriction_matrix(restMat, 3)

  expect_equal(dim(R)[1], 3)
  expect_equal(dim(R)[2], 3)

  expect_equal(R[1,2], 0)
  expect_equal(R[1,3], 0)
  expect_equal(R[2,3], 0)
  expect_equal(all(is.na(R[-c(4, 7, 8)])), TRUE)
})
