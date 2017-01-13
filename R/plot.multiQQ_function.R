#' plot.multiQQ
#'
#' Plotting method for multiQQ object
#'
#' @author James Hutchison
#' @param x \code{multiQQ} object for plotting.
#' @details Generates quantile-quantile plot with 1:1 line from \code{multiQQ} object.
#'

plot.multiQQ <- function(x) {
  plot(1, type = "n", xlab = "Theoretical quantiles", ylab = "Sample quantiles", xlim = c(0, x$xmax), ylim = c(0, x$xmax), main = x$main)
  lines(rbind(c(0, 0), c(x$xmax, x$xmax)))
  mapply(points, x$theoreticalProbs, col = 1:4, pch = 1:4)
  legend("topleft", x$distr, col = 1:4, pch = 1:4)
  invisible(NULL)
}
