#' @import ggplot2
#' @S3method plot svars

plot.svars <- function(x, ...){
  if(x$method != 'Smooth transition'){
    stop('Only resulting objects from id.st are supported by plot')
  }

  trans <- data.frame(t = seq(1:length(x$transition_function)), transition = x$transition_function)
  ggplot(trans, aes(x = t, y = transition)) + geom_line() + xlab("Time") + ylab("") + ggtitle('Transition Function') +
    theme_bw()
}
