test_that("creating regressor matrix works", {
  y <- matrix(seq(1, 30), 10, 3)
  p <- 2
  Z <- y_lag_cr(y, p)
})
