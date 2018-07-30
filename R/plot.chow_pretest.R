#' @import ggplot2
#' @S3method plot chow_pretest

plot.chow_pretest <- function(x, ...){

  if(inherits(x$y, "ts")){
    tsStructure = attr(x$y, which = "tsp")
    Index <- seq(from = tsStructure[1] + x$p/tsStructure[3], to = tsStructure[2], by = 1/tsStructure[3])

    Index <- as.Date(yearmon(Index))
  } else {
    Index <- seq(1:length(x$teststat_bp))
  }

  trans <- data.frame(t = Index[x$from:x$to], break_point = x$teststat_bp[x$from:x$to], sample_split = x$teststat_sp[x$from:x$to])
  colnames(trans) <- c('t', 'Break point', 'Sample split')
  hline.dat <- data.frame(variable = c('Break point', 'Sample split'),
                          Z = c(trans$t[which.max(trans$'Break point')], trans$t[which.max(trans$'Sample split')]))
  trans <- melt(trans, id = 't')
  ggplot(trans, aes(x = t, y = value)) + geom_line() + xlab("Time") + ylab("Test statistic") + ggtitle('Break point and sample split test') +
    facet_wrap(~variable, ncol = 1, scales = 'free_y') + theme_bw() + geom_vline(data = hline.dat, aes(xintercept = Z),  color = 'red')
}

