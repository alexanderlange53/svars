DataGen <- function(A_hat, Data, resid, nlag, TB){
  
  Y <- t(Data)
  
  resid <- t(resid)
  nY <- nrow(Y)

    YB <- matrix(0, nrow = nY, ncol = ncol(Y))
    YB[,1:nlag] <- Y[,1:nlag]
    
    if(nlag == 1){
      for(i in (nlag+1):ncol(Y)){
        YB[,i] <- A_hat[,(1:nY)] %*% YB[,(i-1)] + 
          if(i <= TB){
            resid[,round(runif(1, 1, ncol(Y)-TB-1))]
          }else{
            resid[,round(runif(1, TB, ncol(Y)-nlag))]
          }
      }
    }
    if(nlag == 2){
      for(i in (nlag+1):ncol(Y)){
        YB[,i] <- A_hat[,(1:nY)] %*% YB[,(i-1)] + A_hat[,((nY+1):(2*nY))] %*% YB[,(i-2)] + 
          if(i <= TB){
            resid[,round(runif(1, 1, ncol(Y)-TB-1))]
          }else{
            resid[,round(runif(1, TB, ncol(Y)-nlag))]
          }
      }
    }
    if(nlag == 3){
      for(i in (nlag+1):ncol(Y)){
        YB[,i] <- A_hat[,(1:nY)] %*% YB[,(i-1)] + A_hat[,((nY+1):(2*nY))] %*% YB[,(i-2)] + 
          A_hat[,((2*nY+1):(3*nY))] %*% YB[,(i-3)] + 
          if(i <= TB){
            resid[,round(runif(1, 1, ncol(Y)-TB-1))]
          }else{
            resid[,round(runif(1, TB, ncol(Y)-nlag))]
          }
      }
    }
    if(nlag == 4){
      for(i in (nlag+1):ncol(Y)){
        YB[,i] <- A_hat[,(1:nY)] %*% YB[,(i-1)] + A_hat[,((nY+1):(2*nY))] %*% YB[,(i-2)] + 
          A_hat[,((2*nY+1):(3*nY))] %*% YB[,(i-3)] + A_hat[,((3*nY+1):(4*nY))] %*% YB[,(i-4)] + 
          if(i <= TB){
            resid[,round(runif(1, 1, ncol(Y)-TB-1))]
          }else{
            resid[,round(runif(1, TB, ncol(Y)-nlag))]
          }
      }
    }
    if(nlag == 5){
      for(i in (nlag+1):ncol(Y)){
        YB[,i] <- A_hat[,(1:nY)] %*% YB[,(i-1)] + A_hat[,((nY+1):(2*nY))] %*% YB[,(i-2)] + 
          A_hat[,((2*nY+1):(3*nY))] %*% YB[,(i-3)] + A_hat[,((3*nY+1):(4*nY))] %*% YB[,(i-4)] + 
          A_hat[,((4*nY+1):(5*nY))] %*% YB[,(i-5)] + 
          if(i <= TB){
            resid[,round(runif(1, 1, ncol(Y)-TB-1))]
          }else{
            resid[,round(runif(1, TB, ncol(Y)-nlag))]
          }
      }
    }
    if(nlag == 6){
      for(i in (nlag+1):ncol(Y)){
        YB[,i] <- A_hat[,(1:nY)] %*% YB[,(i-1)] + A_hat[,((nY+1):(2*nY))] %*% YB[,(i-2)] + 
          A_hat[,((2*nY+1):(3*nY))] %*% YB[,(i-3)] + A_hat[,((3*nY+1):(4*nY))] %*% YB[,(i-4)] + 
          A_hat[,((4*nY+1):(5*nY))] %*% YB[,(i-5)] + A_hat[,((5*nY+1):(6*nY))] %*% YB[,(i-6)] +
          if(i <= TB){
            resid[,round(runif(1, 1, ncol(Y)-TB-1))]
          }else{
            resid[,round(runif(1, TB, ncol(Y)-nlag))]
          }
      }
    }
    if(nlag == 7){
      for(i in (nlag+1):ncol(Y)){
        YB[,i] <- A_hat[,(1:nY)] %*% YB[,(i-1)] + A_hat[,((nY+1):(2*nY))] %*% YB[,(i-2)] + 
          A_hat[,((2*nY+1):(3*nY))] %*% YB[,(i-3)] + A_hat[,((3*nY+1):(4*nY))] %*% YB[,(i-4)] + 
          A_hat[,((4*nY+1):(5*nY))] %*% YB[,(i-5)] + A_hat[,((5*nY+1):(6*nY))] %*% YB[,(i-6)] +
          A_hat[,((6*nY+1):(7*nY))] %*% YB[,(i-7)] + 
          if(i <= TB){
            resid[,round(runif(1, 1, ncol(Y)-TB-1))]
          }else{
            resid[,round(runif(1, TB, ncol(Y)-nlag))]
          }
      }
    }
    if(nlag == 8){
      for(i in (nlag+1):ncol(Y)){
        YB[,i] <- A_hat[,(1:nY)] %*% YB[,(i-1)] + A_hat[,((nY+1):(2*nY))] %*% YB[,(i-2)] + 
          A_hat[,((2*nY+1):(3*nY))] %*% YB[,(i-3)] + A_hat[,((3*nY+1):(4*nY))] %*% YB[,(i-4)] + 
          A_hat[,((4*nY+1):(5*nY))] %*% YB[,(i-5)] + A_hat[,((5*nY+1):(6*nY))] %*% YB[,(i-6)] +
          A_hat[,((6*nY+1):(7*nY))] %*% YB[,(i-7)] + A_hat[,((7*nY+1):(8*nY))] %*% YB[,(i-8)] + 
          if(i <= TB){
            resid[,round(runif(1, 1, ncol(Y)-TB-1))]
          }else{
            resid[,round(runif(1, TB, ncol(Y)-nlag))]
          }
      }
    }
    if(nlag == 9){
      for(i in (nlag+1):ncol(Y)){
        YB[,i] <- A_hat[,(1:nY)] %*% YB[,(i-1)] + A_hat[,((nY+1):(2*nY))] %*% YB[,(i-2)] + 
          A_hat[,((2*nY+1):(3*nY))] %*% YB[,(i-3)] + A_hat[,((3*nY+1):(4*nY))] %*% YB[,(i-4)] + 
          A_hat[,((4*nY+1):(5*nY))] %*% YB[,(i-5)] + A_hat[,((5*nY+1):(6*nY))] %*% YB[,(i-6)] +
          A_hat[,((6*nY+1):(7*nY))] %*% YB[,(i-7)] + A_hat[,((7*nY+1):(8*nY))] %*% YB[,(i-8)] + 
          A_hat[,((8*nY+1):(9*nY))] %*% YB[,(i-9)] + 
          if(i <= TB){
            resid[,round(runif(1, 1, ncol(Y)-TB-1))]
          }else{
            resid[,round(runif(1, TB, ncol(Y)-nlag))]
          }
      }
    }
    if(nlag == 10){
      for(i in (nlag+1):ncol(Y)){
        YB[,i] <- A_hat[,(1:nY)] %*% YB[,(i-1)] + A_hat[,((nY+1):(2*nY))] %*% YB[,(i-2)] + 
          A_hat[,((2*nY+1):(3*nY))] %*% YB[,(i-3)] + A_hat[,((3*nY+1):(4*nY))] %*% YB[,(i-4)] + 
          A_hat[,((4*nY+1):(5*nY))] %*% YB[,(i-5)] + A_hat[,((5*nY+1):(6*nY))] %*% YB[,(i-6)] +
          A_hat[,((6*nY+1):(7*nY))] %*% YB[,(i-7)] + A_hat[,((7*nY+1):(8*nY))] %*% YB[,(i-8)] + 
          A_hat[,((8*nY+1):(9*nY))] %*% YB[,(i-9)] + A_hat[,((9*nY+1):(10*nY))] %*% YB[,(i-10)] + 
          if(i <= TB){
            resid[,round(runif(1, 1, ncol(Y)-TB-1))]
          }else{
            resid[,round(runif(1, TB, ncol(Y)-nlag))]
          }
      }
    }
    if(nlag == 11){
      for(i in (nlag+1):ncol(Y)){
        YB[,i] <- A_hat[,(1:nY)] %*% YB[,(i-1)] + A_hat[,((nY+1):(2*nY))] %*% YB[,(i-2)] + 
          A_hat[,((2*nY+1):(3*nY))] %*% YB[,(i-3)] + A_hat[,((3*nY+1):(4*nY))] %*% YB[,(i-4)] + 
          A_hat[,((4*nY+1):(5*nY))] %*% YB[,(i-5)] + A_hat[,((5*nY+1):(6*nY))] %*% YB[,(i-6)] +
          A_hat[,((6*nY+1):(7*nY))] %*% YB[,(i-7)] + A_hat[,((7*nY+1):(8*nY))] %*% YB[,(i-8)] + 
          A_hat[,((8*nY+1):(9*nY))] %*% YB[,(i-9)] + A_hat[,((9*nY+1):(10*nY))] %*% YB[,(i-10)] + 
          A_hat[,((10*nY+1):(11*nY))] %*% YB[,(i-11)] + 
          if(i <= TB){
            resid[,round(runif(1, 1, ncol(Y)-TB-1))]
          }else{
            resid[,round(runif(1, TB, ncol(Y)-nlag))]
          }
      }
    }
    if(nlag == 12){
      for(i in (nlag+1):ncol(Y)){
        YB[,i] <- A_hat[,(1:nY)] %*% YB[,(i-1)] + A_hat[,((nY+1):(2*nY))] %*% YB[,(i-2)] + 
          A_hat[,((2*nY+1):(3*nY))] %*% YB[,(i-3)] + A_hat[,((3*nY+1):(4*nY))] %*% YB[,(i-4)] + 
          A_hat[,((4*nY+1):(5*nY))] %*% YB[,(i-5)] + A_hat[,((5*nY+1):(6*nY))] %*% YB[,(i-6)] +
          A_hat[,((6*nY+1):(7*nY))] %*% YB[,(i-7)] + A_hat[,((7*nY+1):(8*nY))] %*% YB[,(i-8)] + 
          A_hat[,((8*nY+1):(9*nY))] %*% YB[,(i-9)] + A_hat[,((9*nY+1):(10*nY))] %*% YB[,(i-10)] + 
          A_hat[,((10*nY+1):(11*nY))] %*% YB[,(i-11)] + A_hat[,((11*nY+1):(12*nY))] %*% YB[,(i-12)] +
          if(i <= TB){
            resid[,round(runif(1, 1, ncol(Y)-TB-1))]
          }else{
            resid[,round(runif(1, TB, ncol(Y)-nlag))]
          }
      }
    }
    
    YB <- t(YB)
    if(nY == 4){
      colnames(YB) <- c('I', 'GDP', 'PCE', 'FFR')
    }else{
      colnames(YB) <- c('I', 'Isector', 'GDP', 'PCE', 'FFR')
    }
    
    return(YB)
}