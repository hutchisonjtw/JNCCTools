#' multiQQPlot
#'
#' Plots quantile-quantile plots for multiple probability distributions on one set of axes.
#'
#' @param x Numeric vector of data values to be plotted to be plotted as a histogram.
#' @param main Title for the plot. Default value is \code{"QQ plot of sample data against likely distributions"}.
#' @param distr Character vector of distributions for which to produce QQ plots. Note that this uses the standard R names for the distributions which differ from those used in \code{emon}. Should be one or more of \code{"norm"} (normal), \code{"pois"} (Poisson), \code{"lnorm"} (log normal) and \code{"nbinom"} (negative binomial). By default all four are plotted.
#' @details QQ plots are used to visually assess how well a data sample fits a given probability distribution.
#' @return Primarily used for plotting, but invisibly returns an object of class multiQQ, which can be stored for later plotting if required.
#' @export
#'

multiQQPlot <- function(x, main = "QQ plot of sample data against likely distributions", distr = c("nbinom", "pois", "norm", "lnorm")) {
  MASSLoaded <- require(MASS)
  if(!isTRUE(MASSLoaded)) stop("Package 'MASS' could not be loaded. Is it installed?")
  distrCheck <- distr %in% c("nbinom", "pois", "norm", "lnorm")
  if(any(!distrCheck)) stop("distr must be one or more of 'norm', 'pois', 'lnorm', 'nbinom'. Other values are not permitted.")
  probabilities <- 1:length(x)/(length(x) + 1)
  theoreticalProbs <- list()
  if ("nbinom" %in% distr) {
    nbinomQuantiles <- cbind(sort(x), qnbinom(probabilities, size = fitdistr(x, "Negative Binomial")$estimate[1], mu = fitdistr(x, "Negative Binomial")$estimate[2]))
    theoreticalProbs$nbinom <- nbinomQuantiles
  }
  if ("pois" %in% distr) {
    poisQuantiles <- cbind(sort(x), qpois(probabilities, fitdistr(x, "Poisson")$estimate))
    theoreticalProbs$pois <- poisQuantiles
  }
  if ("norm" %in% distr) {
    normQuantiles <- cbind(sort(x), qnorm(probabilities, mean = fitdistr(x, "Normal")$estimate[1], sd = fitdistr(x, "Normal")$estimate[2]))
    theoreticalProbs$norm <- normQuantiles
  }
  if ("lnorm" %in% distr) {
    lnormQuantiles <- cbind(sort(x), qlnorm(probabilities, meanlog = fitdistr(x, "Lognormal")$estimate[1], sdlog = fitdistr(x, "Lognormal")$estimate[2]))
    theoreticalProbs$lnorm <- lnormQuantiles
  }
  if (length(theoreticalProbs) == 0) stop("distr must be one or more of 'norm', 'pois', 'lnorm', 'nbinom'")
  plot(x, x, type = "n", xlab = "Theoretical quantiles", ylab = "Sample quantiles", main = main)
  lines(rbind(c(0,0),c(max(x), max(x))))
  mapply(points, theoreticalProbs, col = 1:4, pch = 1:4)
  legend("topleft", distr, col = 1:4, pch = 1:4)
  output <- list(xmax = max(x), theoreticalProbs = theoreticalProbs, distr = distr, main = main)
  class(output) <- "multiQQ"
  invisible(output)
}
