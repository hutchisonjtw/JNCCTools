#' power.groupsByFactor
#'
#' Subsets a dataset by a factor then uses power.groups to calculate statistical power for each group
#'
#' @author James Hutchison
#' @param x Numerical vector from which the distribution parameters will be calculated using \code{fitdistr}
#' @param f Factor the same length as \code{x}, which is used to subset the data
#' @param change Mean of second group minus mean of first group (i.e. mu2-mu1) or percentage change in mu1 to create mu2 (depending on value of \code{change.type}).
#' @param change.type Whether the parameter change represents an additive (\code{"A"}) or percentage (\code{"M"}) change.
#' @param n1 Vector of sample sizes for group 1. Must be of same dimension as \code{n2}.
#' @param n2 Vector of sample sizes for group 2. Must be of same dimension as \code{n1}.
#' @param distribution The statistical distribution for the two groups. Can be either: \code{"Normal"}, \code{"Poisson"}, \code{"Lognormal"} or \code{"Negbin"}.
#' @param test Should be \code{"P"} for parametric or \code{"NP"} for non-parametric. See \code{?power.groups} for more details.
#' @param alternative A character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}. You can specify just the initial letter. As an example, "less" means that the alternative is that the mean of the first group is less than the mean of the second.
#' @param alpha The type 1 error for assessing statistical significance (default is 0.05) in the power simulations.
#' @param nsims Number of repeat simulations to estimate power (default is 1000).
#' @param nreps Number of repeat permutations for randomisation test (default is 999).
#' @param target.power The desired power. This is not used in the calculation, but is passed into the resulting object and used to show the target line when the object is plotted.
#' @details This function splits \code{x} into subsets based on \code{f}, then runs \code{power.groups} on each subset. \code{pars1} and \code{pars2} for \code{power.groups} are automatically calculated based on \code{x} and \code{distribution} using basic summary stats (e.g. \code{mean}, \code{sd}) or using \code{MASS::fitdistr} if \code{distribution = "Negbin"}.
#' @return An object of class \code{powerByFactor}, which is essentially a list containing the power results for each level of \code{f}. The package includes methods to \code{plot() powerByFactor} objects.
#' @export
#'


power.groupsByFactor <- function(x, f=NULL, change, change.type, n1, n2, distribution, test, alternative="two.sided", alpha=0.05, nsims=1000, nreps=999, target.power=0.8) {
  MASSLoaded <- require(MASS)
  if(!isTRUE(MASSLoaded)) stop("Package 'MASS' could not be loaded. Is it installed?")
  emonLoaded <- require(emon)
  if(!isTRUE(emonLoaded)) stop("Package 'emon' could not be loaded. Is it installed?")
  distrCheck <- distribution %in% c('Normal', 'Poisson', 'Lognormal', 'Negbin')
  if(any(!distrCheck)) stop("distr must be one of 'Normal', 'Poisson', 'Lognormal' or 'Negbin'. Other values are not permitted.")
  if (!is.null(f)) {
    splitData <- split(x, f, drop=TRUE)
  } else {
    splitData <- list(Power=x)
  }
  if (any(sapply(splitData, length)==1)) {
    if (any(sapply(splitData, length)>1)) {
      warning(paste("The following factor levels have only one value and will not be used: ", paste(names(which(sapply(splitData, length)==1)), collapse=", ")))
      splitData <- splitData[sapply(splitData, length) > 1]
    } else {
      stop("No level has more than one data value. Please check your data and re-run")
    }
  }
  if (any(sapply(splitData, length)<=10)) {
    warning(paste("The following factor levels have small data sets (10 values or fewer). Results will still be calculated but should be interpreted with caution: ", paste(names(which(sapply(splitData, length)<=10), collapse=", "))))
  }
  if (distribution == "Normal") {
    pars_1.1 <- lapply(splitData, mean)
    pars_1.2 <- lapply(splitData, sd)
    pars_2 <- pars1.2
    pars_1 <- mapply(c, pars1.1, pars1.2, SIMPLIFY = FALSE)
  } else if (distribution == "Poisson") {
    pars_1 <- lapply(splitData, mean)
    pars_2 <- NULL
  } else if (distribution == "Lognormal") {
    if (any(lapply(splitData, function(x) 0 %in% x))) {
      stop(paste("The following levels contain 0s, so cannot be used with the lognormal distribution. Please correct the data then re-run: ", paste(names(which(sapply(splitData, function(x) 0 %in% x))), collapse=", ")))
    }
    splitDataLog <- lapply(splitData, log)
    pars_1.1 <- lapply(splitDataLog, mean)
    pars_1.2 <- lapply(splitDataLog, sd)
    pars_2 <- pars1.2
    pars_1 <- mapply(c, pars1.1, pars1.2, SIMPLIFY = FALSE)
  } else if (distribution == "Negbin") {
    NegbinPars <- function(x) {fitdistr(x, "Negative Binomial")$estimate}
    pars_1 <-  lapply(splitData, FUN = function(x) NegbinPars(x)[c(2,1)])
    if (change.type == "M") {
      pars_2 <-  lapply(splitData, FUN = function(x, y = change) {
        size2.samevar(NegbinPars(x)[2], NegbinPars(x)[2]*(100+y)/100, NegbinPars(x)[1])
      })
    } else if (change.type == "A") {
      pars_2 <- lapply(splitData, FUN = function(x, y = change) {
        size2.samevar(NegbinPars(x)[2], NegbinPars(x)[2]*(100+y)/100, NegbinPars(x)[1])
      })
    }
  } else stop("distr must be one of 'Normal', 'Poisson', 'Lognormal' or 'Negbin'")
  result <- mapply(power.groups, pars1 = pars_1, pars2 = pars_2, MoreArgs = list(change = change, change.type = change.type, n1 = n1, n2 = n2, distribution = distribution, test = test, alternative = alternative, alpha = alpha, nsims = nsims, nreps = nreps), SIMPLIFY = FALSE)
  output <- list(result = result, nSample = n2, alpha = alpha, target.power = target.power)
  class(output) <- "powerByFactor"
  return(output)
}
