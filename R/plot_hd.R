#' @import ggplot2
#' @importFrom reshape2 melt
#' @import zoo
#' @S3method plot hd

plot.hd <- function(x, ...){

  Index <- NULL
  value <- NULL
  PlotData <- as.data.frame(x$hidec)
  if(inherits(x$hidec, "ts")){
    tsStructure = attr(x$hidec, which = "tsp")
    PlotData$Index <- seq(from = tsStructure[1], to = tsStructure[2], by = 1/tsStructure[3])

    PlotData$Index <- as.Date(yearmon(PlotData$Index))

  } else {
    PlotData$Index <- 1:nrow(PlotData)
    PlotData$V1 <- NULL
  }

  dat <- melt(PlotData,id = 'Index')
  ggplot(dat, aes(x = Index, y = value)) + geom_line() +
    facet_wrap(~variable, ncol = 1) +
    xlab("Time") + theme_bw()

}


