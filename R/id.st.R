id.st <- function(x, c_lower = 0.25, c_upper = 0.75, c_step = 5,
                  gamma_lower = -3.5, gamma_upper = 3.5, gamma_step = 0.5,
                  crit = 0.01, max.iter = 50){

  # Gathering informations from reduced form model
  if(inherits(x, "var.boot")){
    u_t <- x$residuals
    Tob <- nrow(u_t)
    k <- ncol(u_t)
    residY <- u_t
  }else{
    u_t <- residuals(x)
    Tob <- nrow(u_t)
    k <- ncol(u_t)
    residY <- u_t
  }

  if(inherits(x, "var.boot")){
    p <- x$p
    y <- t(x$y)
    type = x$type
    coef_x = x$coef_x
  }else if(inherits(x, "varest")){
    p <- x$p
    y <- t(x$y)
  }else if(inherits(x, "nlVar")){
    if(inherits(x, "VECM")){
      stop("id.cv is not available for VECMs")
    }
    p <- x$lag
    y <- t(x$model[, 1:k])
  }else if(inherits(x, "list")){
    p <- x$order
    y <- t(x$data)
  }else{
    stop("Object class is not supported")
  }

  # Function for Z matrix
  y_lag_cr <- function(y, lag_length){
    # create matrix that stores the lags
    y_lag <- matrix(NA, dim(y)[1],dim(y)[2]*lag_length)
    for (i in 1:lag_length) {
      y_lag[(1+i):dim(y)[1],((i*NCOL(y)-NCOL(y))+1):(i*NCOL(y))] <- y[1:(dim(y)[1]-i),(1:NCOL(y))]
    }
    # drop first observation
    y_lag <- as.matrix(y_lag[-(1:lag_length),])
    out <- list(lags = y_lag)
  }


  Sigma_hat <- crossprod(u_t)/(Tob-1-k*p)

  init_B <- t(chol(Sigma_hat))
  init_Lambda <- diag(k)

  # Transition function
  transition <- function(gamma, cc, st){
    G <- (1 + exp(-exp(gamma)*(st - cc)))^(-1)
    return(G)
  }

  # Creating grid for iterative procedure
  gamma_grid <-  seq(gamma_lower, gamma_upper, by = gamma_step)
  cc_grid <- seq(ceiling(c_lower*Tob), floor(c_upper*Tob), by = c_step)
  grid_comb <- unique(expand.grid(gamma_grid, cc_grid))
  G_grid <- mapply(transition, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))

  # Fiterative approach
  iterative_smooth_transition <- function(transition, Tob, crit, max.iter){

    W <- solve(diag(K*(Tob))) # identity matrix as starting weighting matrix

    count <- 0 # count variable
    Exit <-  1  #Exit criterion

    while( (abs(stop_cond) > crit) && (round <= max.iter) ){
      count <- count + 1
      # Z <- t(Z)
      y_reg <- t(y)
      y_reg <- c(y_reg)
      y <- matrix(y, (T-spec$lag),K)

      # in first round VAR starting values are estimates for the VAR model, since W = I
      b <-  solve( (kronecker(t(Z),diag(K) )%*%W %*%kronecker(Z,diag(K) ))  )  %*% (kronecker(t(Z),diag(K) ))%*%W%*%y_reg
      # b <-  solve( (kronecker(diag(K),t(Z) )%*%W %*%kronecker(diag(K),Z ))  )  %*% (kronecker(diag(K), t(Z) ))%*%W%*%c(y)

      Theta <-  vector(mode="numeric", length=(K^2)*spec$lag+K)
      for (i in 1:((K^2)*spec$lag+K) ){
        Theta[i] <- b[i]
      }
      Theta <- matrix(Theta , K, K*spec$lag+1)
      u_est <- residuals3(y, Theta, Z)
      Sigma <- 1/(T-spec$lag)*t(u_est)%*%u_est


      # get initial values for first repetition only from linear var
      if (round== 1){
        # ini <- vech(sqrtm((T-spec$lag)^-1*t(u_est)%*%u_est)$B)
        ini <-  c(t(chol(Sigma)))
        ini <- c(ini, rep(0.5,K))
      }


      ###############
      #   STEP 1    #
      ###############

      # minimize the negative log-likelihood

      object_fun <- function(unknown) LogLike_smtr_apply(unknown, y, Z,tr_func, T,K, spec,u_est)
      # Minimize the negative Log-Likelihoodfunction

      output <- nlm(object_fun, p= ini)

      # names(output$estimate) <- c("int_ml","alpha_ml", "B", "lambda")
      # assign(paste(names(output$estimate), collapse=""), output$estimate)
      # estimates have to be squared to get structural parameters <- but for starting values use the estimates that are not squared
      ini <- c(output$estimate) # save the estimated structural parameters
      # calculate the estimated variances
      B <-  vector(mode="numeric", length = K^2)
      for (i in 1:K^2){
        B[i] <- output$estimate[i]
      }
      B <- matrix(B, K,K)
      Sigma1 <-  B%*%t(B)

      Lambda <- vector(mode="numeric", length = K)
      count <- 0
      for (i in (K^2+1:(K^2+K))){
        count <- count+1
        Lambda[count] <- output$estimate[i]
      }
      Lambda<- diag(Lambda, K,K)
      Sigma2 <- B%*%Lambda%*%t(B)
      l_l <- output$minimum
      old_ll[round] <- abs(l_l)
      if (round == 1){stop_cond <- stop_cond}
      else {stop_cond <- (l_l -old_ll[round-1])/old_ll[round-1]}
      ###############
      #   STEP 2    #
      ###############
      #--> re-estimate the model (see estimation of b in the next round of while loop and the last estimation is outside the loop)    # construct the weighting matrix
      Omega_vec <- matrix(NA, K^2, (T-spec$lag))
      for (i in 1:(T- spec$lag)){
        Omega_vec[,i] <- c((1-tr_func[i])*Sigma1 + tr_func[i]*Sigma2)
      }
      W <-  matrix(0, (T-spec$lag)*K,(T-spec$lag)*K)
      count <- 0
      for (i in 1:(T-spec$lag)){
        count <- count +1
        W[((i*K)-(K-1)):(i*K),((i*K)-(K-1)):(i*K)] <-  solve(matrix(Omega_vec[,count],K,K))
      }
    } # end of the while loop for iterative process to estimate parameters B and lambda
    # estimate b one more time, since loop breaks before the very last estimation
    b <-  solve( (kronecker(t(Z),diag(K) )%*%W %*%kronecker(Z,diag(K) ))  )  %*% (kronecker(t(Z),diag(K) ))%*%W%*%y_reg

    Theta <-  vector(mode="numeric", length=(K^2)*spec$lag+K)
    for (i in 1:((K^2)*spec$lag+K) ){
      Theta[i] <- b[i]
    }
    Theta <- matrix(Theta , K, K*spec$lag+1 )

    if (round <= 500){
      convergence <- 1
    } else if (round > 500){
      convergence <- 0
    }

    out <- list(B = B, Lambda =Lambda, ll = -l_l, sigma1 = Sigma1, sigma2 = Sigma2, Theta =Theta,  convergence=  convergence)
    return(out)

  }



  likelihood.st <- function(parameter, u_t){

    B <- parameter[1:k*k]
    Lambda <- parameter[(k*k+1):(k*k+k)]

    Omega <- (1 - G)*tcrossprod(B, B) + G*B%*%tcrossprod(Lambda, B)

    L <- -0.5*(log(det(Omega))) - 0.5*crossprod(u_t, Omega)%*%u_t


  }

}
