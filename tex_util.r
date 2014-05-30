#' Print the summary of an mcmc in tex format
#'
#' The summary is printed in the following way "mu cred.mass*100\% HDI[hdi.lo, hdi.hi]"
#' optionally encloses the output within "$" if math.mode == T
#' The summary is calculated using \link\code{summary_mcmc}
#'
#' @param mcmc a numeric vector containing the mcmc draws
#' @param cred.mass a 0..1 number specifying the mass within the HDI
#' @param est.str Character string to prepend to the point estimate. For example "Mdn=". By default it's ""
#' @param math.mode if T, enclose output within "$"
#' @digits digits number of decimal places to print
#'
#' @examples
#' tex_summary_mcmc(rnorm(1000))
#' # [1] "$0.09$, $95\\%$ HDI $[-1.83, 2.09]$"
#' # Prepend "Mdn=" to estimate
#' tex_summary_mcmc(rnorm(1000), est.str="Mdn=", digits=3)
#' # [1] "$Mdn=-0.033$, $95\\%$ HDI $[-1.961, 1.8]$"
#' # The same without math mode
#' tex_summary_mcmc(rnorm(1000), est.str="Mdn=", digits=3, math.mode = F)
#' # [1] "Mdn=0.03, 95\\% HDI [-1.869, 1.983]"
tex_summary_mcmc <- function(mcmc, cred.mass=0.95, est.str = "", math.mode = T, digits=texu_digits()){
  mcmc.sum <- lapply(summary_mcmc(mcmc), round, digits)
  ans <- with(mcmc.sum, paste0("$", est.str, mu, "$, $", cred.mass*100, "\\%$ HDI $[", hdi.lo, ", ", hdi.hi, "]$"))
  if(!math.mode) ans <- stringr::str_replace_all(ans, stringr::fixed("$"), "")
  ans
}

#' Gets the default digits for tex_util methods
#'
#' By default digits = 2
#' To change the default digits call \code\{link{set_texu_digits}}
texu_digits <- function(){
  if(!exists("tex_util.__digits")){
    set_texu_digits()
  }
  tex_util.__digits
}

#' Gets the default digits for tex_util methods
#'
#' By default digits = 2
set_texu_digits <- function(digits=2){
  tex_util.__digits <<- digits
}