#' plot.powerByFactor
#'
#' Plotting method for \code{powerByFactor} object class
#'
#' @param x \code{powerByFactor} object
#' @details Plots graphs of statistical power (i.e. proportion of significant results) against number of samples from a \code{powerByFactor} object.
#' @export

plot.powerByFactor <- function(x) {
  titles <- names(x$result)
  panelCols <- round(sqrt(length(titles)))
  panelRows <- ceiling(length(titles)/panelCols)
  par(mfrow = c(panelRows, panelCols))
  plotWithLine <- function(x, y, main, alpha) {
    plot(x, y, main = main, xlim = c(0, max(x)), ylim = c(0, 1), xlab = "n samples", ylab = "Proportion significant")
    lines(x = c(-100, max(x)*2), y = c(1-alpha, 1-alpha), col = "red")
  }
  invisible(mapply(plotWithLine, y = x$result, main = titles, MoreArgs = list(x = x$nSample, alpha = x$alpha)))
  par(mfrow = c(1, 1))
}
