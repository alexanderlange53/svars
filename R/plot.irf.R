#' @import ggplot2
#' @importFrom reshape2 melt
#' @S3method plot irf

plot.irf <- function(x, base, scales = "free_y", ...){


  impulse <- melt(x$irf, id = 'V1')
  ggplot(impulse, aes(x = ~V1, y = ~value)) + geom_line() + geom_hline(yintercept = 0, color = 'red') +
    facet_wrap(~variable, scales = scales, labeller = label_parsed) +
    xlab("Observation Time") + ylab("Response") +
  theme_bw()
}

