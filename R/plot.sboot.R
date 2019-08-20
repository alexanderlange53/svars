#' @S3method plot sboot

plot.sboot <- function(x, scales = "free_y", lowerq = 0.16, upperq = 0.84, percentile = 'standard', ..., base){

  impulse <- melt(x$true$irf, id = 'V1')
  confidence <- x$bootstrap
  horizon <- nrow(confidence[[1]]$irf)
  kk <- ncol(confidence[[1]]$irf)
  nboot <- length(confidence)
  rest <- x$rest_mat

  intervals <- array(0, c(horizon, kk, nboot))
  for(i in 1:nboot){
    intervals[,,i] <- as.matrix(confidence[[i]]$irf)
  }

  if(percentile == 'standard' | percentile == 'hall'){
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
    if(percentile == 'hall'){
      lower <- 2*x$true$irf - lower
      upper <- 2*x$true$irf - upper
    }
  }else if(percentile == 'bonferroni'){
    rest <- matrix(t(rest), nrow = 1)
    rest[is.na(rest)] <- 1
    rest[kk] <- 1
    lower <- matrix(0, horizon, kk)
    for(i in 1:horizon){
      for(j in 1:kk){
        if(rest[j] == 0){
          lower[i,j] <- quantile(intervals[i,j,], probs = (lowerq/horizon))
        } else{
          lower[i,j] <- quantile(intervals[i,j,], probs = (lowerq/(horizon + 1)))
          }
      }
    }


    upper <- matrix(0, horizon, kk)
    for(i in 1:horizon){
      for(j in 1:kk){
        if(rest[j] == 0){
          upper[i,j] <- quantile(intervals[i,j,], probs = 1 + (((upperq - 1)/horizon)))
        }else{
          upper[i,j] <- quantile(intervals[i,j,], probs = 1 + (((upperq - 1)/(horizon + 1))))
        }
      }
    }
    lower <- as.data.frame(lower)
    upper <- as.data.frame(upper)
  }else {
    print("Invalid choice of percentile; choose between standard, hall and bonferroni")
  }

  lower <- melt(lower, id = 'V1')
  upper <- melt(upper, id = 'V1')

  ggplot(impulse, aes_(x = ~V1, y = ~value)) +  geom_ribbon(aes(x = lower$V1, ymin= lower$value, ymax= upper$value), alpha=.7, fill = 'grey') +
    geom_line() + geom_hline(yintercept = 0, color = 'red') +
    facet_wrap(~variable, scales = scales, labeller = label_parsed) +
    xlab("Observation Time") + ylab("Response") +
    theme_bw()
}
