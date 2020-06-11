#' @import ggplot2
#' @importFrom reshape2 melt
#' @method plot svarirf
#' @export

plot.svarirf <- function(x, base, scales = "free_y", selection = NULL,
                         cumulative = NULL, ...){

  # Auxiliary functions
  cutt <- function(x, selind){
    x$irf <- x$irf[selind]  ## account for the first column being "V1" the horizon counter
    return(x)
  }

  agg <- function(x, aggind){
    x$irf[,aggind] <- cumsum(x$irf[aggind])
    return(x)
  }

  # To select shocks and variables to plot
  kk <- ncol(x$irf)
  k <- sqrt(kk - 1)

  ### Calculate cumulated irfs if necessary
  if(!is.null(cumulative)){
    aggind <- list()
    temp <- (k * cumulative) + 1
    for(i in 1:length(temp)){
      aggind[[i]] <- temp[i] - (k - c(1:k))
    }
    aggind <- unlist(aggind)
    x <- agg(x, aggind)
  }

  # Select specific shocks or series
  if(!is.null(selection)){
    selind <- list()
    temp <- (k * selection[[1]]) + 1 ## for the column V1
    for(i in 1:length(temp)){
      selind[[i]] <- temp[i] - (k - selection[[2]])
    }
    selind <- c(1, unlist(selind))

    x <- cutt(x, selind)
  }


  impulse <- melt(x$irf, id = 'V1')
  ggplot(impulse, aes_(x = ~V1, y = ~value)) + geom_line() + geom_hline(yintercept = 0, color = 'red') +
    facet_wrap(~variable, scales = scales, labeller = label_parsed) +
    xlab("Horizon") + ylab("Response") +
  theme_bw()
}

