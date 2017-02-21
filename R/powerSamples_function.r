#' PowerSamples
#'
#' Uses iteration to calculate the number of samples needed to achieve a desired power
#'
#' @author James Hutchison
#' @param
#'

PowerSamples <- function(x, f, change, change.type, power, distribution, test, alternative="two.sided", alpha=0.05, nsims=1000, nreps=999) {
  MASSLoaded <- require(MASS)
  if(!isTRUE(MASSLoaded)) stop("Package 'MASS' could not be loaded. Is it installed?")
  emonLoaded <- require(emon)
  if(!isTRUE(emonLoaded)) stop("Package 'emon' could not be loaded. Is it installed?")
  distrCheck <- distribution %in% c('Normal', 'Poisson', 'Lognormal', 'Negbin')
  if(any(!distrCheck)) stop("distr must be one of 'Normal', 'Poisson', 'Lognormal' or 'Negbin'. Other values are not permitted.")
  if (distribution == "Normal") {
    pars_1.1 <- lapply(splitData, mean)
    pars_1.2 <- lapply(splitData, sd)
    pars_2 <- pars1.2
    pars_1 <- mapply(c, pars1.1, pars1.2, SIMPLIFY = FALSE)
  } else if (distribution == "Poisson") {
    pars_1 <- lapply(splitData, mean)
    pars_2 <- NULL
  } else if (distribution == "Lognormal") {
    splitDataLog <- lapply(splitData, log)
    pars_1.1 <- lapply(splitDataLog, mean)
    pars_1.2 <- lapply(splitDataLog, sd)
    pars_2 <- pars1.2
    pars_1 <- mapply(c, pars1.1, pars1.2, SIMPLIFY = FALSE)
  } else if (distribution == "Negbin") {
    NegbinPars <- fitdistr(x, "Negative Binomial")$estimate
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
  result <- power.groups(change = change, change.type = change.type, n1 = startVal, n2 = startVal, pars1 = pars_1, pars2 = pars_2, distribution = distribution, test = test)
  i <- startVal
  if (result < power) {
    while(result < power) {
      i <- i*2
      result <- power.groups(change = change, change.type = change.type, n1 = i, n2 = i, pars1 = pars_1, pars2 = pars_2, distribution = distribution, test = test)
    }
    j <- i
    i <- i/2
  } else if (result > power){
    while(result > power) {
      i <- round(i/2)
      result <- power.groups(change = change, change.type = change.type, n1 = i, n2 = i, pars1 = pars_1, pars2 = pars_2, distribution = distribution, test = test)
    }
    j <- i*2
  }
  gap <- j - i
  while(gap > 1) {
    k <- round(i + 0.5*gap)
    result_k <- power.groups(change = change, change.type = change.type, n1 = k, n2 = k, pars1 = pars_1, pars2 = pars_2, distribution = distribution, test = test)
    if (result_k < power) {
      i <- k
    } else if (result_k > power){
      j <- k
    }
    gap <- j - i
  }
}



