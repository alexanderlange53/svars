context("YLagCr")

test_that("creating regressor matrix works 3 dim 2 lag", {
  y <- matrix(seq(1, 30), 10, 3)
  p <- 2
  Z <- YLagCr(y, p)
  # check row dimensions n-p
  expect_equal(dim(Z)[1], dim(y)[1] - p)
  # check col dimensions
  expect_equal(dim(Z)[2], dim(y)[2] * p)
})

test_that("creating regressor matrix works 3 dim 4 lag", {
  y <- matrix(seq(1, 30), 10, 3)
  p <- 4
  Z <- YLagCr(y, p)
  # check row dimensions n-p
  expect_equal(dim(Z)[1], dim(y)[1] - p)
  # check col dimensions
  expect_equal(dim(Z)[2], dim(y)[2] * p)
})

test_that("creating regressor matrix works 5 dim 2 lag", {
  y <- matrix(seq(1, 50), 10, 5)
  p <- 2
  Z <- YLagCr(y, p)
  # check row dimensions n-p
  expect_equal(dim(Z)[1], dim(y)[1] - p)
  # check col dimensions
  expect_equal(dim(Z)[2], dim(y)[2] * p)
})

test_that("creating regressor matrix works 5 dim 4 lag", {
  y <- matrix(seq(1, 50), 10, 5)
  p <- 4
  Z <- YLagCr(y, p)
  # check row dimensions n-p
  expect_equal(dim(Z)[1], dim(y)[1] - p)
  # check col dimensions
  expect_equal(dim(Z)[2], dim(y)[2] * p)
})

test_that("creating regressor matrix works 2 dim 2 lag", {
  y <- matrix(seq(1, 20), 10, 2)
  p <- 2
  Z <- YLagCr(y, p)
  # check row dimensions n-p
  expect_equal(dim(Z)[1], dim(y)[1] - p)
  # check col dimensions
  expect_equal(dim(Z)[2], dim(y)[2] * p)
})

test_that("creating regressor matrix works 5 dim 4 lag", {
  y <- matrix(seq(1, 20), 10, 2)
  p <- 4
  Z <- YLagCr(y, p)
  # check row dimensions n-p
  expect_equal(dim(Z)[1], dim(y)[1] - p)
  # check col dimensions
  expect_equal(dim(Z)[2], dim(y)[2] * p)
})
