#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom ggfortify fortify.ts
#' @S3method plot hd

plot.hd <- function(x, ...){

  V1 <- NULL
  value <- NULL

  if(inherits(x$hidec, "ts")){
    x$hidec = fortify(x$hidec)
    names(x$hidec)[1] = "V1"
  }
  dat <- melt(x$hidec,id = 'V1')
  ggplot(dat, aes(x = V1, y = value)) + geom_line() +
    facet_wrap(~variable, ncol = 1) +
    xlab("Time") + theme_bw()

}
