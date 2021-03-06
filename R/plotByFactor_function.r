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
  if (any(sapply(splitData, length)==1)) {
    if (any(sapply(splitData, length)>1)) {
      warning(paste("The following factor levels have only one value and will not be used: ", paste(names(which(sapply(splitData, length)==1)), collapse=", ")))
      splitData <- splitData[sapply(splitData, length) > 1]
    } else {
      stop("No level has more than one data value. Please check your data and re-run")
    }
  }
  panelCols <- round(sqrt(nlevels(as.factor(f))))
  panelRows <- ceiling(nlevels(as.factor(f))/panelCols)
  par(mfrow = c(panelRows, panelCols), oma = c(2.5,0,0,0))
  mapply(plotFunction, splitData, main = names(splitData), MoreArgs = list(legend = FALSE))
  par(mfrow = c(1, 1), oma=c(0,0,0,0), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
  legend(0,-0.18, c("nbinom", "pois", "norm", "lnorm"), fill = 1:4, horiz=TRUE, xpd=NA)
  invisible(NULL)
}
