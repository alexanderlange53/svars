#' @S3method plot boot

plot.boot <- function(x, scales = "free_y", lowerq = 0.16, upperq = 0.84, ..., base){

  impulse <- melt(x$true$irf, id = 'V1')

  confidence <- x$bootstrap
  horizon <- nrow(confidence[[1]]$irf)
  kk <- ncol(confidence[[1]]$irf)
  nboot <- length(confidence)

  intervals <- array(0, c(horizon, kk, nboot))
  for(i in 1:nboot){
    intervals[,,i] <- as.matrix(confidence[[i]]$irf)
  }

  lower <- matrix(0, horizon, kk)
  for(i in 1:horizon){
    for(j in 1:kk){
      lower[i,j] <- quantile(intervals[i,j,], probs = lowerq)
    }
  }

  upper <- matrix(0, horizon, kk)
  for(i in 1:horizon){
    for(j in 1:kk){
      upper[i,j] <- quantile(intervals[i,j,], probs = upperq)
    }
  }
  lower <- as.data.frame(lower)
  upper <- as.data.frame(upper)

  lower <- melt(lower, id = 'V1')
  upper <- melt(upper, id = 'V1')

  ggplot(impulse, aes(x = V1, y = value)) +  geom_ribbon(aes(x = lower$V1, ymin= lower$value, ymax= upper$value), alpha=.7, fill = 'grey') +
    geom_line() + geom_hline(yintercept = 0, color = 'red') +
    facet_wrap(~variable, scales = scales, labeller = label_parsed) +
    xlab("Observation Time") + ylab("Response") +
    theme_bw()
}
