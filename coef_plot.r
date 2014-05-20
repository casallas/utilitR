library("ggplot2")

#' Plots the coefficients of a fit using ggplot2
#' @param fit a fitted model object
#' @param coef.names an optional vector containing the desired coefficient names in the output.
#' @param parse.coef parse the coef names in the output. See \code{\link{plotmath}} for the syntax.
#' @param digits number of decimal digits to show per coefficient
#' @param order order coefficients per estimate value.
#'   When set to TRUE, beware of coefficient scales to avoid misleading results and 
#'   consider standardizing (e.g. using \code{\link{arm::standardize}}).
#' @param ... further parameters passed to implementing methods
coef_plot <- function(fit, coef.names=NULL, parse.coef=T, digits=1, order.coef=F, ...){
	UseMethod("coef_plot")
}

#' Plots the coefficients of an lm fit using ggplot2
#' @param fit an lm object
#' @param intercept include the intercept in the plot?
#' @param ... further parameters passed to \code{\link{.coef_plot}}
#'
#' @examples
#'
#' fit1 <- lm(price ~ carat + cut + depth + color + table + clarity + x + y + z, data=diamonds)
#' # Misleading result
#' coef_plot(fit1, order.coef=T)
#' # Better
#' fit1.z <- arm::standardize(fit1, standardize.y = T)
#' coef_plot(fit1.z, order.coef=T)
coef_plot.lm <- function(fit, intercept=T, ...){
  fit.se <- summary(fit)$coefficients[,2] # se is col 2
  fit.CI <- confint(fit)
  fit.coef <- data.frame(
    mu = coef(fit),
    p025 = fit.CI[,1],
    p975 = fit.CI[,2],
    se = fit.se
  )
  if(!intercept){
    fit.coef <- fit.coef[-1,]
  }

  fit.coef <- within(fit.coef,{
    # Create cols for +/-1 se
    p159 <- mu - se
    p841 <- mu + se
  })
  .coef_plot(fit.coef, ...)
}

#' Plots the coefficients of a stan fit using ggplot2
#' @param fit a stanfit object
#' @param pars A vector of character strings specifying the parameters to be plotted.
#'   "beta" by default
#' @param ... further parameters passed to \code{\link{coef_plot_mcmc}} and \code{\link{.coef_plot}}
coef_plot.stanfit <- function(fit, pars="beta", ...){
  betas <- extract(fit, pars)[pars]
  coef_plot_mcmc(betas, ...)
}

#' Plots the coefficients of MCMC draws using ggplot2
#'
#' Although this method is intended for MCMC draws, it can be used with any variable composed of numeric elements.
#' The input is cast to a "long" data frame using \code{\link{reshape2::melt}}, so it's important to either have
#' the input set with (col)names or pass meaningful names via coef.names to avoid "noise" or blank coefficient names
#' as axis breaks
#'
#' @param mcmc usually a data frame, matrix, list of vectors or vector containing MCMC draws
#' @param ... further parameters passed to \code{\link{coef_plot_mcmc}} and \code{\link{.coef_plot}}
coef_plot_mcmc <- function(mcmc, ...){
  betas <- reshape2::melt(as.data.frame(mcmc), variable.name="coefficient")
  if(!require("plyr")) stop("plyr library not installed")
  fit.coef <- ddply(betas, .(coefficient), summarize,
    mu = mean(value),
	p025 = quantile(value, .025),
	p975 = quantile(value, .975),
	p159 = quantile(value, .159),
	p841 = quantile(value, .841))
  .coef_plot(fit.coef, ...)
}

#' Plots the coefficients of a fit using ggplot2
#'
#' Users should call the higher-level generic "coef_plot", or implement a method for the
#' corresponding class to get fit.coef from the specific object
#'
#' @param fit.coef a data frame containing columns mu, p025, p975, p159, and p841.
#' @param coef.names an optional vector containing the desired coefficient names in the output.
#' @param parse.coef parse the coef names in the output. See \code{\link{plotmath}} for the syntax.
#' @param digits number of decimal digits to show per coefficient
#' @param order order coefficients per estimate value.
#'   When set to TRUE, beware of coefficient scales to avoid misleading results and
#'   consider standardizing (e.g. using \code{\link{arm::standardize}}).
.coef_plot <- function(fit.coef, coef.names=NULL, parse.coef=T, digits=1, order.coef=F){
  # Add a column for the evaluated factor based on row names, keeping order
  fit.coef$coefficient <- factor(rownames(fit.coef), levels = rownames(fit.coef))
  
  if(!is.null(coef.names)){
    fit.coef <- within(fit.coef, {
      coefficient <- plyr::mapvalues(coefficient, levels(coefficient), coef.names)
    })
  }
  if(order.coef){
    fit.coef <- fit.coef[order(fit.coef$mu), ]
	# Reorder coefficient names keeping order
	fit.coef$coefficient <- with(fit.coef, factor(as.character(coefficient), levels = as.character(coefficient)))
  }
  
  # Parse the text of the factors to make them expressions
  labs <- sapply(levels(fit.coef$coefficient),
                 function(x){
                   ifelse(parse.coef, parse(text = x), x)
                   })
  names(labs) <- levels(fit.coef$coefficient)
  fit.coef$digits <- digits
  
  ggplot(fit.coef, aes(x=coefficient, y=mu, ymin = p025, ymax = p975)) +
    scale_x_discrete("coefficient", labels = labs)+
    scale_y_continuous("estimate") +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_pointrange(aes(ymin = p159, ymax = p841), size = 1, colour="#e41a1c") + # se
    geom_pointrange(colour="#e41a1c") + # CI
    geom_text(aes(label=paste0(round(mu, digits),"\n"))) +
    coord_flip() +
    theme_bw()
}
