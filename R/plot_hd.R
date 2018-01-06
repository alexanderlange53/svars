#' @import ggplot2
#' @importFrom reshape2 melt
#' @S3method plot hd

plot.hd <- function(x){

  dat <- melt(x$hidec, id = 'V1')

  ggplot(dat, aes(x = V1, y = value)) + geom_line() +
    facet_wrap(~variable, ncol = 1) +
    xlab("Time") + theme_bw()

}
