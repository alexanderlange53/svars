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

  trans <- data.frame(t = Index, c = x$transition_function)
  ggplot(trans, aes(x = t, y = c)) + geom_line() + xlab("Time") + ylab("") + ggtitle('Transition Function') +
    theme_bw()
}
