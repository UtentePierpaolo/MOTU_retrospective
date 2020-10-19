# responsescale_plotconfintglht

# from
# multcomp:::plot.confint.glht

# modified to plot on response scale (x 1000)


# fcg(confint(wht))

fcg <- function (x, xlim, xlab, ylim, ...)
{
  # xi <- x$confint
  xi <- exp(x$confint)
  xrange <- c(min(xi[, "lwr"]), max(xi[, "upr"]))
  if (!is.finite(xrange[1]))
    xrange[1] <- min(xi[, "Estimate"])
  if (!is.finite(xrange[2]))
    xrange[2] <- max(xi[, "Estimate"])
  yvals <- nrow(xi):1
  if (missing(xlim))
    xlim <- xrange
  if (missing(ylim))
    ylim <- c(0.5, nrow(xi) + 0.5)
  plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals,
                                            2), type = "n", axes = FALSE, xlab = "",
       ylab = "", xlim = xlim, ylim = ylim, ...)
  axis(1, ...)
  axis(2, at = nrow(xi):1, labels = dimnames(xi)[[1]], las = 1,
       ...)
  abline(h = yvals, lty = 1, lwd = 1, col = "lightgray")
  # abline(v = 0, lty = 2, lwd = 1, ...)
  abline(v = 1, lty = 2, lwd = 1, ...)
  left <- xi[, "lwr"]
  left[!is.finite(left)] <- min(c(0, xlim[1] * 2))
  right <- xi[, "upr"]
  right[!is.finite(right)] <- max(c(0, xlim[2] * 2))
  segments(left, yvals, right, yvals, ...)
  points(xi[, "lwr"], yvals, pch = "(", ...)
  points(xi[, "upr"], yvals, pch = ")", ...)
  points(xi[, "Estimate"], yvals, pch = 20, ...)
  main <- list(...)$main
  if (is.null(main)) {
    if (attr(x, "type") == "adjusted") {
      main <- paste(format(100 * attr(x$confint, "conf.level"),
                           2), "% family-wise confidence level\n",
                    sep = "")
    }
    else {
      main <- paste(format(100 * attr(x$confint, "conf.level"),
                           2), "% confidence level\n", sep = "")
    }
  }
  else {
    main <- NULL
  }
  if (missing(xlab))
    # xlab <- "Linear Function"
    xlab <- "Incidence rate ratio"
  title(main = main, xlab = xlab, cex.lab=1.5, cex.main=1.5)
  box()
}
# <bytecode: 0x00000275800f5398>
#   <environment: namespace:multcomp>