#' @export

plot.histWithDist <- function(x, y, ...) {
  plot(x$histogram, freq = FALSE, main = x$main)
  mapply(lines, x$curves, col = 1:4)
  legend("topright", x$distr, col = 1:4)
  invisible(NULL)
}
