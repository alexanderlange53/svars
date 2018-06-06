y_lag_cr <- function(y, lag_length){
  # creates matrix which stores the lags
  y_lag <- matrix(NA, dim(y)[1], dim(y)[2] * lag_length)
  for (i in 1:lag_length) {
    y_lag[(1+i):dim(y)[1], ((i * ncol(y) - ncol(y))+1):(i * ncol(y))] <- y[1:(dim(y)[1] - i), (1:ncol(y))]
  }
  # drops first observation
  y_lag <- as.matrix(y_lag[-(1:lag_length), ])
  out <- list(lags = y_lag)
}
