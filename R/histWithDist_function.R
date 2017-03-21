#' histWithDist
#'
#' Plots histogram with fitted distribution curves
#'
#' @author James Hutchison
#' @param x Numeric vector of data values to be plotted to be plotted as a histogram.
#' @param main Title for the plot. Default value is \code{"Histogram with fitted distributions"}.
#' @param distr Character vector of distribution curves to overlay on the histogram. Note that this uses the standard R names for the distributions which differ from those used in emon. Should be one or more of "norm" (normal), "pois" (Poisson), "lnorm" (log normal) and "nbinom" (negative binomial). By default all four are plotted.
#' @details This function uses \code{MASS::fitdistr} to fit distribution curves to \code{x} for the distributions specified, then overlays these on a histogram of \code{x}.
#' @return Primarily used directly plotting, but also invisibly returns a \code{histWithDist} object that can be stored for later plotting if needed.
#' @export
#'

histWithDist <- function(x, main = "Histogram with fitted distributions", distr = c("nbinom", "pois", "norm", "lnorm"), legend = TRUE) {
  MASSLoaded <- require(MASS)
  if(!isTRUE(MASSLoaded)) stop("Package 'MASS' could not be loaded. Is it installed?")
  distrCheck <- distr %in% c("nbinom", "pois", "norm", "lnorm")
  if(any(!distrCheck)) stop("distr must be one or more of 'norm', 'pois', 'lnorm', 'nbinom'. Other values are not permitted.")
  h <- hist(x, plot=FALSE)
  distLines <- list()
  if ("nbinom" %in% distr) {
    nbinomLine <- matrix(c(0:max(h$breaks), dnbinom(0:max(h$breaks), size = fitdistr(x, "Negative Binomial")$estimate[1], mu = fitdistr(x, "Negative Binomial")$estimate[2])), ncol = 2)
    distLines$nbinomLine <- nbinomLine
  }
  if ("pois" %in% distr) {
    poisLine <- matrix(c(0:max(h$breaks), dpois(0:max(h$breaks), fitdistr(x, "Poisson")$estimate)), ncol = 2)
    distLines$poisLine <- poisLine
  }
  if ("norm" %in% distr) {
    normLine <- matrix(c(0:max(h$breaks), dnorm(0:max(h$breaks), mean = fitdistr(x, "Normal")$estimate[1], sd = fitdistr(x, "Normal")$estimate[2])), ncol = 2)
    distLines$normLine <- normLine
  }
  if ("lnorm" %in% distr) {
    if (any(x==0)) {
      warning("Data contains 0s so lognormal distribution cannot be fitted. If you need the lognormal distribution please correct the data and try again")
    } else {
      lnormLine <- matrix(c(0:max(h$breaks), dlnorm(0:max(h$breaks), meanlog = fitdistr(x, "Lognormal")$estimate[1], sdlog = fitdistr(x, "Lognormal")$estimate[2])), ncol = 2)
    distLines$lnormLine <- lnormLine
    }
  }
  if (length(distLines) == 0) stop("distr must be one or more of 'norm', 'pois', 'lnorm', 'nbinom'")
  ymax <- max(sapply(distLines, FUN = function(x) max(x[ ,2])))
  ymax <- max(c(ymax, h$density))
  plot(h, freq=FALSE, main = main, xlim = c(0, max(h$breaks)), ylim = c(0,ymax))
  mapply(lines, distLines, col = 1:4)
  if(legend) {legend("topright", distr, col = 1:4, lty = 1)}
  output <- list(histogram = h, curves = distLines, distr = distr, main = main)
  class(output) <- "histWithDist"
  invisible(output)
}
