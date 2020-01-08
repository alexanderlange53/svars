#' @import ggplot2
#' @importFrom reshape2 melt
#' @export

plot.svarfevd <- function(x, ...){

  V1 <- NULL
  value <- NULL
  Variables <- NULL

    fe <- data.frame(V1 = seq(1, nrow(x[[1]])),
                     Variables = c(sapply(names(x), rep, nrow(x[[1]]))), sapply(x, unlist))

    for(i in 3:ncol(fe)){
      colnames(fe)[i] <- paste("FEVD of", colnames(fe)[i])
    }

    fe <- melt(fe, id = c('V1', 'Variables'))

    ggplot(fe, aes(x = V1, y = value, fill = Variables)) + geom_bar(stat="identity", position='stack') +
      facet_wrap(~variable, ncol = 1) +
      xlab("Horizon") + ylab("Contribution to FEV [in %]") + scale_fill_grey() +
      theme_bw() + theme(legend.title=element_blank())

}
