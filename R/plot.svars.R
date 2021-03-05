#' @import ggplot2
#' @method plot svars
#' @export

plot.svars <- function(x, ...){
  if(x$method != 'Smooth transition'){
    stop('Only resulting objects from id.st are supported by plot')
  }

  if(inherits(x$y, "ts")){
    tsStructure = attr(x$y, which = "tsp")
    Index <- seq(from = tsStructure[1] + x$p/tsStructure[3], to = tsStructure[2], by = 1/tsStructure[3])

    Index <- as.Date(yearmon(Index))

  } else {
    Index <- seq(1:length(x$transition_function))
  }

  if(length(x$est_c) > 1) {
    trans <- data.frame(t = Index, c = x$transition_function, transition = 'Regime 1')
    trans2 <- data.frame(t = Index, c = x$transition_function2, transition = 'Regime 2')
    trans3 <- data.frame(t = Index, c = x$transition_function3, transition = 'Regime 3')
    trans_all <- rbind(trans, trans2, trans3)
    ggplot(trans_all, aes(x = t, y = c)) + geom_line() + xlab("Time") + ylab("") + ggtitle('Transition Functions') +
      facet_wrap(~transition, ncol = 1) + theme_bw()
  } else {
    trans <- data.frame(t = Index, c = x$transition_function)
    ggplot(trans, aes(x = t, y = c)) + geom_line() + xlab("Time") + ylab("") + ggtitle('Transition Function') +
      theme_bw()
  }

}
