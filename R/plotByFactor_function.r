#' plotByFactor
#'
#' Splits a dataset by a factor then runs a plotting function on each subset, plotting the results in a grid.
#'
#' @author James Hutchison
#' @param x numeric vector of data to be plotted
#' @param f factor vector of same length as \code{x}, which is used to subset the data
#' @param plotFunction Function used to plot the data.
#' @export

plotByFactor <- function(x, f, plotFunction) {
  splitData <- split(x, f, drop=TRUE)
  panelCols <- round(sqrt(nlevels(f)))
  panelRows <- ceiling(nlevels(f)/panelCols)
  par(mfrow = c(panelRows, panelCols))
  mapply(plotFunction, splitData, main = levels(f))
  par(mfrow = c(1, 1))
  invisible(NULL)
}
