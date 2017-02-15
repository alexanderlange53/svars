#' @export

plot.irf <- function(x, scales = "fixed", ..., base){

  impulse <- melt(x$irf, id = 'V1')
  ggplot(impulse, aes(x = V1, y = value)) + geom_line() + geom_hline(yintercept = 0, color = 'red') +
    facet_wrap(~variable, scales = scales) +
    xlab("Observation Time") + ylab("Response") +
  theme_bw()
}
