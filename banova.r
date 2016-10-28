#' TODO document all functions

# Standard deviation of coeficients (use for dummy coded)
sd_coef_mcmc <- function(chains){
  chains <- cbind(0, chains)
  apply(chains, 1, sd)
}
# Standard deviation of coeficients (use for hierarchical)
sd_hcoef_mcmc <- function(chains){
  apply(chains, 1, sd)
}
# Bayesian ANOVA plot, as described in Gelman 2007, Ch. 22
banova_plot <- function(s_chains, ...){
  HDIs <- sapply(s_chains, .CI, credMass=.95)
  coef_plot_mcmc(s_chains[, ncol(s_chains):1], ...) + scale_x_discrete("Source") + scale_y_continuous("Est. sd. of coefficients", expand=c(0,0), limits=c(0, max(HDIs)))
}
# The same, but with catseye plots
banova_catseye <- function(s_chains, ...){
  coef_catseye_mcmc(s_chains[, ncol(s_chains):1], ...) + scale_x_discrete("Source") + scale_y_continuous("Est. sd. of coefficients", expand=c(0,0), limits=c(0, max(s_chains)))
}
effect_plot <- function(chains, to_sum0=T, ...){
  chains <- t(apply(chains, 1, to.sum0))
  coef_plot_mcmc(chains, ...)
}
int_plot <- function(chains, to_sum0=T, coef.names = NULL, ...){
  require(dplyr)
  chains <- reshape2::melt(chains)
  # default name for first colum
  names(chains)[1] <- "iter"
  chains %>%
    group_by(iter) %>%
    mutate(value=value-mean(value))
  stop("not working")
  coef_plot_mcmc(chains, ...)
}
to.sum0 <- function(coef.row){
  coef.row - mean(coef.row)
}
#to.sum0 <- function(coef.row){
#  -sum(coef.row)/(length(coef.row)+1) + c(0, coef.row)
#}
