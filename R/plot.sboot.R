#' @S3method plot sboot

plot.sboot <- function(x, scales = "free_y", lowerq = 0.16, upperq = 0.84,
                       percentile = "standard", ..., base) {

  n.ahead <- nrow(x$true$irf)

  shocks <- names(x$true$irf)[-1]
  shocks <- sub("epsilon\\[ *", "", shocks)
  shocks <- strsplit(shocks, " *] *%->% *")

  shocks <- as.data.frame(do.call(rbind, shocks))
  shocks <- shocks[rep(1:nrow(shocks), each = n.ahead),]
  names(shocks) <- c("Impulse", "Response")
  row.names(shocks) <- NULL

  irfs <- melt(x$true$irf, id.vars = "V1")
  irfs <- cbind(shocks, irfs[names(irfs) != "variable"])
  names(irfs)[3:4] <- c("Time", "IRF")

  cbs <- sapply(x$bootstrap, function(x) unlist(x$irf[-1]))
  cbs <- t(apply(cbs, 1, quantile, probs = c(lowerq, upperq)))
  colnames(cbs) <- c("Lower", "Upper")
  rownames(cbs) <- NULL

  irfs <- cbind(irfs, cbs)

  if (percentile == "hall") {
    irfs$Lower <- 2 * irfs$IRF - irfs$Lower
    irfs$Upper <- 2 * irfs$IRF - irfs$Upper
  }

  lvls <- as.character(unique(irfs$Impulse))
  irfs$Response <- factor(irfs$Response, levels = lvls)
  irfs$Impulse <- factor(irfs$Impulse, levels = lvls)

  ggplot(irfs, aes(x = Time)) +
    geom_line(aes(y = IRF)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.7, fill = "grey") +
    geom_hline(yintercept = 0, color = "red") +
    facet_grid(Response ~ Impulse, scales = scales, labeller = label_both) +
    xlab("Observation Time") +
    ylab("Response") +
    theme_bw()

}
