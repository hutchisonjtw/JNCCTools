#' @export

print.powerByFactor <- function(x, ...) {
  results <- do.call(cbind, x$result)
  final <- data.frame(Samples = x$nSample, results)
  print(final)
  invisible(NULL)
}
