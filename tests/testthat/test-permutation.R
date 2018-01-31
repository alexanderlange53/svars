context("permutation")

test_that("3x3 permutation works", {
  res <- permutation(diag(3))
  expect_length(res, 6)

  m <- matrix(c(1, 0, 0,
                0, 1, 0,
                0, 0, 1), ncol = 3, byrow = TRUE)

  expect_equal(sum(sapply(res, function(perm) all(m == perm))), 1)

  m <- matrix(c(1, 0, 0,
                0, 0, 1,
                0, 1, 0), ncol = 3, byrow = TRUE)

  expect_equal(sum(sapply(res, function(perm) all(m == perm))), 1)

  m <- matrix(c(0, 1, 0,
                1, 0, 0,
                0, 0, 1), ncol = 3, byrow = TRUE)

  expect_equal(sum(sapply(res, function(perm) all(m == perm))), 1)

  m <- matrix(c(0, 0, 1,
                1, 0, 0,
                0, 1, 0), ncol = 3, byrow = TRUE)

  expect_equal(sum(sapply(res, function(perm) all(m == perm))), 1)

  m <- matrix(c(0, 0, 1,
                0, 1, 0,
                1, 0, 0), ncol = 3, byrow = TRUE)

  expect_equal(sum(sapply(res, function(perm) all(m == perm))), 1)

  m <- matrix(c(0, 1, 0,
                0, 0, 1,
                1, 0, 0), ncol = 3, byrow = TRUE)

  expect_equal(sum(sapply(res, function(perm) all(m == perm))), 1)

})

test_that("1x1 permutation works", {
  expect_equal(as.matrix(1), permutation(as.matrix(1)))
})
