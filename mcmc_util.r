#' Gets a list with the point estimate (mu) and HDI (hdi.lo, hdi.hi) of an MCMC chain
#'
#' This function uses \link{\code{median}} to calculate the point estimate and \link{\code{BEST::HDI}}
#' to calculate the HDI
#'
#' @param mcmc a numeric vector containing the mcmc draws
#' @param cred.mass a 0..1 number specifying the mass within the HDI
#' @return a list with "mu": point estimate
#'                     "hdi.lo": lower bound of the HDI
#'                     "hdi.hi": higher bound of the HDI
#'                     "cred.mass": credible mass
#' @examples
#' summary_mcmc(rnorm(1000))
summary_mcmc <- function(mcmc, cred.mass=0.95){
  mcmc.hdi <- as.numeric(BEST::hdi(mcmc, cred.mass))
  list(mu = median(mcmc), hdi.lo = mcmc.hdi[1], hdi.hi = mcmc.hdi[2], cred.mass = cred.mass)
}

#' Executes summary_mcmc on the columns of a matrix or dataframe, and returns a data frame
summary_mcmcs <- function(mcmcs, cred.mass=0.95){
  do.call(rbind.data.frame,
          apply(mcmcs, 2, summary_mcmc, cred.mass))
}

#' Gives the probability that one set of mcmc draws is greater than another
#'
#' lhs and rhs must both be numeric vectors, with equal lengths
#' @examples
#' p_greater_mcmc(rnorm(1000, mean=1), rnorm(1000))
p_greater_mcmc <- function(lhs, rhs){
  sum(lhs > rhs)/length(lhs)
}
