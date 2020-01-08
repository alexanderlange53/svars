#' @import ggplot2
#' @importFrom reshape2 melt
#' @import zoo
#' @export

plot.cf <- function(x, ...){

  Index <- NULL
  value <- NULL
  group <- NULL
  actual <- as.data.frame(x$actual)
  counter <- as.data.frame(x$counter)

  if(inherits(x$actual, "ts")){
    tsStructure = attr(x$actual, which = "tsp")
    actual$Index <- seq(from = tsStructure[1], to = tsStructure[2], by = 1/tsStructure[3])

    actual$Index <- as.Date(yearmon(actual$Index))

  } else {
    actual$Index <- 1:nrow(actual)
    actual$V1 <- NULL
  }

  colnames(actual)[1:(ncol(counter))] <- colnames(counter)
  counter$Index <- actual$Index


  dat_a <- melt(actual,id = 'Index')
  dat_c <- melt(counter,id = 'Index')

  dat <- rbind(dat_a, dat_c)
  dat$group <- 'n'
  dat$group[1:nrow(dat_a)] <- 'Actual'
  dat$group[(nrow(dat_a) + 1):nrow(dat)] <- 'Counterfactual'

  ggplot(dat) + geom_line(aes(x = Index, y = value, group = group, linetype = group)) +
    facet_wrap(~variable, ncol = 1) +
    xlab("Time") +  theme_bw()  + theme(legend.position="bottom", legend.title = element_blank())

}


