
# function which rotates the B matrix and calculates the indpendence of the structural errors
testlik <- function(theta, faklow, u, dd) {

  temp_l <- rotmat(theta, faklow)

  ser_low <- tcrossprod(u, solve(temp_l))
  ddtest <- copula::indepTest(ser_low, dd)
  ddtest$global.statistic # * 10000000

}


# function to compute the optimal B matrix
rotmat <- function(pv, faklow) {

  ts_dim <- (1 + sqrt(1 + 8 * length(pv))) / 2
  combns <- combn(ts_dim, 2, simplify = FALSE)
  rotmat <- diag(ts_dim)

  for (i in seq_along(pv)) {

    tmp <- diag(ts_dim)
    tmp[combns[[i]][1], combns[[i]][1]] <- cos(pv[i])
    tmp[combns[[i]][2], combns[[i]][2]] <- cos(pv[i])
    tmp[combns[[i]][1], combns[[i]][2]] <- - sin(pv[i])
    tmp[combns[[i]][2], combns[[i]][1]] <- sin(pv[i])
    rotmat <- rotmat %*% tmp

  }

  tcrossprod(faklow, rotmat)

}
