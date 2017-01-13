#' plot.histWithDist
#'
#' Plotting method for \code{histWithDist} object class
#'
#' @author James Hutchison
#' @param x \code{histWithDist} object
#' @details Plots histogram with distribution curves and legend from \code{histWithDist} object.

plot.histWithDist <- function(x) {
  plot(x$histogram, freq = FALSE, main = x$main)
  mapply(lines, x$curves, col = 1:4)
  legend("topright", x$distr, col = 1:4)
  invisible(NULL)
}
