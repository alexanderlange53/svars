mb.boot_iv <- function(x, b.length = 15, n.ahead = 20, nboot = 500, nc = 1){

  # gathering informations from vars object
  y <- x$y
  p <- x$p
  obs <- x$n
  k <- x$K
  B <- x$B
  instruments <- as.matrix(x$Instrument)

  # calculating covariance from actual VAR
  A <- x$A_hat
  Z <- t(y_lag_cr(y, p)$lags)

  if(x$type == 'const'){
    Z <- rbind(rep(1, ncol(Z)), Z)
  }else if(x$type == 'trend'){
    Z <- rbind(seq(p + 1, ncol(Z)+ p), Z)
  }else if(x$type == 'both'){
    Z <- rbind(rep(1, ncol(Z)), seq(p + 1, ncol(Z) + p), Z)
  }else{
    Z <- Z
  }
  u <- t(y[-c(1:p),]) - A %*% Z

  # creating new error terms and instruments
  errors_iv <- list()

  # creating blocks
  N <- ceiling(obs/b.length)
  blocks <- array(NA, c(b.length, k, obs - b.length + 1))
  # Creating bootstrap versions of instruments
  blocks_m <- array(NA, c(b.length, ncol(instruments), obs - b.length + 1))
  u <- t(u)
  for(i in 1:(obs - b.length + 1)){
    blocks[, , i] <- u[i:(i + b.length - 1),]
    blocks_m[, , i] <- instruments[i:(i + b.length - 1),]
  }

  # centering errors and instruments
  u_center <- matrix(NA, b.length, k)
  ins_center <- matrix(NA, b.length, ncol(x$B))
  for(i in 1:b.length){
    u_center[i, ] <- colMeans(u[i:(obs-b.length+i),])
    if(ncol(instruments) == 1){
      ins_center[i, ] <- mean(instruments[i:(obs-b.length+i),])
    }else{
      ins_center[i, ] <- colMeans(instruments[i:(obs-b.length+i),])
    }
  }

  u_center <- do.call(rbind, replicate(N, u_center, simplify=FALSE))
  u_center <- u_center[1:obs, ]
  ins_center <- do.call(rbind, replicate(N, ins_center, simplify=FALSE))
  ins_center <- ins_center[1:obs, ]

  for(i in 1:nboot){
    epsilon.star <- matrix(0, b.length*N, ncol(u))
    epsilon.star <- list()

    m.star <- matrix(0, b.length*N, ncol(instruments))
    m.star <- list()
    # stacking randomly selected blocks at each other
    for(kk in 1:N){
      epsilon.star[[kk]] <- blocks[, , ceiling(runif(1, 1, obs - b.length + 1))]
      m.star[[kk]] <- as.matrix(blocks_m[, , ceiling(runif(1, 1, obs - b.length + 1))])
    }
    epsilon.star <- do.call('rbind', epsilon.star)
    m.star <- do.call('rbind', m.star)

    epsilon.star <- epsilon.star[1:obs, ]
    m.star <- m.star[1:obs, ]

    # centering new errors and instruments
    epsilon.star <- epsilon.star - u_center
    m.star <- m.star - ins_center

    errors_iv[[i]] <- list(epsilon_star = t(epsilon.star), m_star = t(m.star))
  }

  # Bootstrapfunction
  bootf_iv <- function(errors_iv_boot){

    Ustar1 <- errors_iv_boot$epsilon_star
    m_star <- errors_iv_boot$m_star

    Ystar <- matrix(0, nrow(y), k)
    # adding pre sample values
    Ystar[1:p,] <- y[1:p,]

    if(x$type == 'const' | x$type == 'trend'){
      for(i in (p+1):nrow(y)){
        for(j in 1:k){
          Ystar[i,j] <- A[j,1] + A[j,-1]%*%c(t(Ystar[(i-1):(i-p),])) + Ustar1[j, (i-p)]
        }
      }
    }else if(x$type == 'both'){
      for(i in (p+1):nrow(y)){
        for(j in 1:k){
          Ystar[i,j] <- A[j,c(1,2)] + A[j,-c(1,2)]%*%c(t(Ystar[(i-p):(i-1),])) + Ustar1[j, (i-p)]
        }
      }
    }else if(x$type == 'none'){
      for(i in (p+1):nrow(y)){
        for(j in 1:k){
          Ystar[i,j] <- A[j,]%*%c(t(Ystar[(i-p):(i-1),])) + Ustar1[j, (i-p)]
        }
      }
    }

    varb <- suppressWarnings(VAR(Ystar, p = x$p, type = x$type))

    if(ncol(as.matrix(m_star)) > nrow(as.matrix(m_star))){
      m_star <- t(as.matrix(m_star))
    }

    temp <- tryCatch(id.iv(varb, instruments = m_star),
                     error = function(e) NULL)

    if(!is.null(temp)){

      if(sqrt(sum(((x$B - temp$B))^2)) >
         sqrt(sum(((x$B - temp$B*(-1)))^2))){
        temp$B <- temp$B * (-1)
      }

      ip <- irf(temp, n.ahead = n.ahead)
      Pstar <- temp$B
      return(list(ip, Pstar))
    }else{
      return(NA)
    }
  }

  bootstraps <- pblapply(errors_iv, bootf_iv, cl = nc)

  return(bootstraps)
}
