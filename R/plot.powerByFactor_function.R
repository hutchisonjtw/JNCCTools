#' @export

plot.powerByFactor <- function(x, y, ...) {
  titles <- names(x$result)
  panelCols <- round(sqrt(length(titles)))
  panelRows <- ceiling(length(titles)/panelCols)
  par(mfrow = c(panelRows, panelCols))
  plotWithLine <- function(x, y, main, alpha) {
    plot(x, y, main = main, xlim = c(0, max(x)), ylim = c(0, 1), xlab = "n samples", ylab = "Proportion significant")
    lines(x = c(-100, max(x)*2), y = c(target.power, target.power), col = "red")
  }
  invisible(mapply(plotWithLine, y = x$result, main = titles, MoreArgs = list(x = x$nSample, target.power = x$target.power)))
  par(mfrow = c(1, 1))
}
