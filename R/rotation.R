# function which rotates the B matrix and calculates the indpendence of the structural errors
testlik_3dim <- function(theta, faklow, u, dd){
  pv <- theta

  pos <- pv[1]
  rotmat12 <- diag(3)
  rotmat12[1,1] <- cos(pos)
  rotmat12[2,2] <- cos(pos)
  rotmat12[1,2] <- sin(pos)*(-1)
  rotmat12[2,1] <- sin(pos)

  pos <- pv[2]
  rotmat13 <- diag(3)
  rotmat13[1,1] <- cos(pos)
  rotmat13[3,3] <- cos(pos)
  rotmat13[1,3] <- sin(pos)*(-1)
  rotmat13[3,1] <- sin(pos)

  pos <- pv[3]
  rotmat23 <- diag(3)
  rotmat23[2,2] <- cos(pos)
  rotmat23[3,3] <- cos(pos)
  rotmat23[2,3] <- sin(pos)*(-1)
  rotmat23[3,2] <- sin(pos)


  rotmat <- rotmat12%*%rotmat13%*%rotmat23
  temp_l <- tcrossprod(faklow, rotmat)

  ser_low <- t(tcrossprod(solve(temp_l), u))


  ddtest <- copula::indepTest(ser_low,dd)
  p <- ddtest$global.statistic.pvalue
  q <- ddtest$global.statistic

  return(q*10000000)
}


# function to compute the optimal B matrix
rotmat_3dim <- function(pv, y, faklow){

  pos <- pv[1]
  rotmat12 <- diag(3)
  rotmat12[1,1] <- cos(pos)
  rotmat12[2,2] <- cos(pos)
  rotmat12[1,2] <- sin(pos)*(-1)
  rotmat12[2,1] <- sin(pos)

  pos <- pv[2]
  rotmat13 <- diag(3)
  rotmat13[1,1] <- cos(pos)
  rotmat13[3,3] <- cos(pos)
  rotmat13[1,3] <- sin(pos)*(-1)
  rotmat13[3,1] <- sin(pos)

  pos <- pv[3]
  rotmat23 <- diag(3)
  rotmat23[2,2] <- cos(pos)
  rotmat23[3,3] <- cos(pos)
  rotmat23[2,3] <- sin(pos)*(-1)
  rotmat23[3,2] <- sin(pos)


  rotmat <- rotmat12%*%rotmat13%*%rotmat23
  temp_l <- tcrossprod(faklow, rotmat)

  return(temp_l)
}


testlik_4dim <- function(theta, faklow, u, dd){

  pv <- theta

  pos <- pv[1]
  rotmat1 <- diag(4)
  rotmat1[1,1] <- cos(pos)
  rotmat1[2,2] <- cos(pos)
  rotmat1[1,2] <- sin(pos)*(-1)
  rotmat1[2,1] <- sin(pos)

  pos <- pv[2]
  rotmat2 <- diag(4)
  rotmat2[1,1] <- cos(pos)
  rotmat2[3,3] <- cos(pos)
  rotmat2[1,3] <- sin(pos)*(-1)
  rotmat2[3,1] <- sin(pos)

  pos <- pv[3]
  rotmat3 <- diag(4)
  rotmat3[1,1] <- cos(pos)
  rotmat3[1,4] <- sin(pos)*(-1)
  rotmat3[4,1] <- sin(pos)
  rotmat3[4,4] <- cos(pos)

  pos <- pv[4]
  rotmat4 <- diag(4)
  rotmat4[2,2] <- cos(pos)
  rotmat4[3,3] <- cos(pos)
  rotmat4[2,3] <- sin(pos)*(-1)
  rotmat4[3,2] <- sin(pos)

  pos <- pv[5]
  rotmat5 <- diag(4)
  rotmat5[2,2] <- cos(pos)
  rotmat5[2,4] <- sin(pos)*(-1)
  rotmat5[4,2] <- sin(pos)
  rotmat5[4,4] <- cos(pos)

  pos <- pv[6]
  rotmat6 <- diag(4)
  rotmat6[3,3] <- cos(pos)
  rotmat6[4,3] <- sin(pos)
  rotmat6[3,4] <- sin(pos)*(-1)
  rotmat6[4,4] <- cos(pos)


  rotmat <- rotmat1%*%rotmat2%*%rotmat3%*%rotmat4%*%rotmat5%*%rotmat6
  temp_l <- tcrossprod(faklow, rotmat)

  ser_low <- t(tcrossprod(solve(temp_l), u))


  ddtest <- copula::indepTest(ser_low,dd)
  p <- ddtest$global.statistic.pvalue
  q <- ddtest$global.statistic

  return(q*10000000)
}


# function to compute the optimal B matrix
rotmat_4dim <- function(pv, faklow){

  pos <- pv[1]
  rotmat1 <- diag(4)
  rotmat1[1,1] <- cos(pos)
  rotmat1[2,2] <- cos(pos)
  rotmat1[1,2] <- sin(pos)*(-1)
  rotmat1[2,1] <- sin(pos)

  pos <- pv[2]
  rotmat2 <- diag(4)
  rotmat2[1,1] <- cos(pos)
  rotmat2[3,3] <- cos(pos)
  rotmat2[1,3] <- sin(pos)*(-1)
  rotmat2[3,1] <- sin(pos)

  pos <- pv[3]
  rotmat3 <- diag(4)
  rotmat3[1,1] <- cos(pos)
  rotmat3[1,4] <- sin(pos)*(-1)
  rotmat3[4,1] <- sin(pos)
  rotmat3[4,4] <- cos(pos)

  pos <- pv[4]
  rotmat4 <- diag(4)
  rotmat4[2,2] <- cos(pos)
  rotmat4[3,3] <- cos(pos)
  rotmat4[2,3] <- sin(pos)*(-1)
  rotmat4[3,2] <- sin(pos)

  pos <- pv[5]
  rotmat5 <- diag(4)
  rotmat5[2,2] <- cos(pos)
  rotmat5[2,4] <- sin(pos)*(-1)
  rotmat5[4,2] <- sin(pos)
  rotmat5[4,4] <- cos(pos)

  pos <- pv[6]
  rotmat6 <- diag(4)
  rotmat6[3,3] <- cos(pos)
  rotmat6[4,3] <- sin(pos)
  rotmat6[3,4] <- sin(pos)*(-1)
  rotmat6[4,4] <- cos(pos)


  rotmat <- rotmat1%*%rotmat2%*%rotmat3%*%rotmat4%*%rotmat5%*%rotmat6
  temp_l <- tcrossprod(faklow, rotmat)

  return(temp_l)
}

testlik_5dim <- function(theta, faklow, u, dd){

  pv <- theta

  pos <- pv[1]
  rotmat12 <- diag(5)
  rotmat12[1,1] <- cos(pos)
  rotmat12[2,2] <- cos(pos)
  rotmat12[1,2] <- sin(pos)*(-1)
  rotmat12[2,1] <- sin(pos)

  pos <- pv[2]
  rotmat13 <- diag(5)
  rotmat13[1,1] <- cos(pos)
  rotmat13[3,3] <- cos(pos)
  rotmat13[1,3] <- sin(pos)*(-1)
  rotmat13[3,1] <- sin(pos)

  pos <- pv[3]
  rotmat14 <- diag(5)
  rotmat14[1,1] <- cos(pos)
  rotmat14[1,4] <- sin(pos)*(-1)
  rotmat14[4,1] <- sin(pos)
  rotmat14[4,4] <- cos(pos)

  pos <- pv[4]
  rotmat15 <- diag(5)
  rotmat15[1,1] <- cos(pos)
  rotmat15[1,5] <- sin(pos)*(-1)
  rotmat15[5,1] <- sin(pos)
  rotmat15[5,5] <- cos(pos)

  pos <- pv[5]
  rotmat23 <- diag(5)
  rotmat23[2,2] <- cos(pos)
  rotmat23[3,3] <- cos(pos)
  rotmat23[2,3] <- sin(pos)*(-1)
  rotmat23[3,2] <- sin(pos)

  pos <- pv[6]
  rotmat24 <- diag(5)
  rotmat24[2,2] <- cos(pos)
  rotmat24[2,4] <- sin(pos)*(-1)
  rotmat24[4,2] <- sin(pos)
  rotmat24[4,4] <- cos(pos)

  pos <- pv[7]
  rotmat25 <- diag(5)
  rotmat25[2,2] <- cos(pos)
  rotmat25[2,5] <- sin(pos)*(-1)
  rotmat25[5,2] <- sin(pos)
  rotmat25[5,5] <- cos(pos)

  pos <- pv[8]
  rotmat34 <- diag(5)
  rotmat34[3,3] <- cos(pos)
  rotmat34[4,3] <- sin(pos)
  rotmat34[3,4] <- sin(pos)*(-1)
  rotmat34[4,4] <- cos(pos)

  pos <- pv[9]
  rotmat35 <- diag(5)
  rotmat35[3,3] <- cos(pos)
  rotmat35[3,5] <- sin(pos)*(-1)
  rotmat35[5,3] <- sin(pos)
  rotmat35[5,5] <- cos(pos)

  pos <- pv[10]
  rotmat45 <- diag(5)
  rotmat45[4,4] <- cos(pos)
  rotmat45[4,5] <- sin(pos)*(-1)
  rotmat45[5,4] <- sin(pos)
  rotmat45[5,5] <- cos(pos)


  rotmat <- rotmat12%*%rotmat13%*%rotmat14%*%rotmat15%*%rotmat23%*%rotmat24%*%rotmat25%*%rotmat34%*%rotmat35%*%rotmat45
  temp_l <- tcrossprod(faklow, rotmat)

  ser_low <- t(tcrossprod(solve(temp_l), u))


  ddtest <- copula::indepTest(ser_low,dd)
  p <- ddtest$global.statistic.pvalue
  q <- ddtest$global.statistic

  return(q*10000000)
}


# function to compute the optimal B matrix
rotmat_5dim <- function(pv, faklow){

  pos <- pv[1]
  rotmat12 <- diag(5)
  rotmat12[1,1] <- cos(pos)
  rotmat12[2,2] <- cos(pos)
  rotmat12[1,2] <- sin(pos)*(-1)
  rotmat12[2,1] <- sin(pos)

  pos <- pv[2]
  rotmat13 <- diag(5)
  rotmat13[1,1] <- cos(pos)
  rotmat13[3,3] <- cos(pos)
  rotmat13[1,3] <- sin(pos)*(-1)
  rotmat13[3,1] <- sin(pos)

  pos <- pv[3]
  rotmat14 <- diag(5)
  rotmat14[1,1] <- cos(pos)
  rotmat14[1,4] <- sin(pos)*(-1)
  rotmat14[4,1] <- sin(pos)
  rotmat14[4,4] <- cos(pos)

  pos <- pv[4]
  rotmat15 <- diag(5)
  rotmat15[1,1] <- cos(pos)
  rotmat15[1,5] <- sin(pos)*(-1)
  rotmat15[5,1] <- sin(pos)
  rotmat15[5,5] <- cos(pos)

  pos <- pv[5]
  rotmat23 <- diag(5)
  rotmat23[2,2] <- cos(pos)
  rotmat23[3,3] <- cos(pos)
  rotmat23[2,3] <- sin(pos)*(-1)
  rotmat23[3,2] <- sin(pos)

  pos <- pv[6]
  rotmat24 <- diag(5)
  rotmat24[2,2] <- cos(pos)
  rotmat24[2,4] <- sin(pos)*(-1)
  rotmat24[4,2] <- sin(pos)
  rotmat24[4,4] <- cos(pos)

  pos <- pv[7]
  rotmat25 <- diag(5)
  rotmat25[2,2] <- cos(pos)
  rotmat25[2,5] <- sin(pos)*(-1)
  rotmat25[5,2] <- sin(pos)
  rotmat25[5,5] <- cos(pos)

  pos <- pv[8]
  rotmat34 <- diag(5)
  rotmat34[3,3] <- cos(pos)
  rotmat34[4,3] <- sin(pos)
  rotmat34[3,4] <- sin(pos)*(-1)
  rotmat34[4,4] <- cos(pos)

  pos <- pv[9]
  rotmat35 <- diag(5)
  rotmat35[3,3] <- cos(pos)
  rotmat35[3,5] <- sin(pos)*(-1)
  rotmat35[5,3] <- sin(pos)
  rotmat35[5,5] <- cos(pos)

  pos <- pv[10]
  rotmat45 <- diag(5)
  rotmat45[4,4] <- cos(pos)
  rotmat45[4,5] <- sin(pos)*(-1)
  rotmat45[5,4] <- sin(pos)
  rotmat45[5,5] <- cos(pos)


  rotmat <- rotmat12%*%rotmat13%*%rotmat14%*%rotmat15%*%rotmat23%*%rotmat24%*%rotmat25%*%rotmat34%*%rotmat35%*%rotmat45
  temp_l <- tcrossprod(faklow, rotmat)

  return(temp_l)
}
