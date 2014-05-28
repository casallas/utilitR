library("ggplot2")

#' Plots the coefficients of a fit using ggplot2
#' @param fit a fitted model object
#' @param coef.names an optional vector containing the desired coefficient names in the output.
#' @param parse.coef parse the coef names in the output. See \code{\link{plotmath}} for the syntax.
#' @param digits number of decimal digits to show per coefficient. If NA the estimates are not shown.
#' @param order order coefficients per estimate value.
#'   When set to TRUE, beware of coefficient scales to avoid misleading results and 
#'   consider standardizing (e.g. using \code{\link{arm::standardize}}).
#' @param ... further parameters passed to implementing methods
coef_plot <- function(fit, coef.names=NULL, parse.coef=F, digits=1, order.coef=F, ...){
	UseMethod("coef_plot")
}

#' Plots the coefficients of an lm fit using ggplot2
#' @param fit an lm object
#' @param intercept include the intercept in the plot?
#' @param ... further parameters passed to \code{\link{.coef_plot}}
#'
#' @examples
#'
#' # Ordering
#' fit1 <- lm(price ~ carat + cut + depth + color + table + clarity + x + y + z, data=diamonds)
#' ## Misleading result
#' coef_plot(fit1, order.coef=T)
#' ## Better
#' fit1.z <- arm::standardize(fit1, standardize.y = T)
#' coef_plot(fit1.z, order.coef=T)
#'
#' # Renaming
#' fit2 <- lm(price ~ carat, data=diamonds)
#' coef_plot(fit2, order.coef=T, parse.coef=T, coef.names=paste0("beta[", 0:1, "]"))
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
  # Add a column for the evaluated factor based on row names, keeping order
  fit.coef$coefficient <- factor(rownames(fit.coef), levels = rownames(fit.coef))

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
#' @param fit.coef a data frame containing columns coefficient, mu, p025, p975, p159, and p841.
#' @param coef.names an optional vector containing the desired coefficient names in the output.
#' @param parse.coef parse the coef names in the output. See \code{\link{plotmath}} for the syntax.
#' @param digits number of decimal digits to show per coefficient. If NA the estimates are not shown.
#' @param order order coefficients per estimate value.
#'   When set to TRUE, beware of coefficient scales to avoid misleading results and
#'   consider standardizing (e.g. using \code{\link{arm::standardize}}).
.coef_plot <- function(fit.coef, coef.names=NULL, parse.coef=F, digits=1, order.coef=F){
  # Replace coefficient names
  .coef.names(fit.coef) <- coef.names
  # Reorder coefficients
  .coef.order(fit.coef) <- order.coef
  # This allows to round mu within geom_text
  fit.coef$digits <- digits

  p <- ggplot(fit.coef, aes(x=coefficient, y=mu, ymin = p025, ymax = p975)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_pointrange(aes(ymin = p159, ymax = p841), size = 1, colour="#e41a1c") + # se
    geom_pointrange(colour="#e41a1c")
  if(!is.na(digits)){
    p <- p + # CI
      geom_text(aes(label=paste0(round(mu, digits),"\n")))
  }
  if(require(magrittr, quietly=T))
    p %>% .coef_plot_deco(fit.coef, parse.coef=parse.coef)
  else
    .coef_plot_deco(p, fit.coef, parse.coef=parse.coef)
}

#' Plots the coefficients of the posterior draws of a fit using Cat's eye plots (via ggplot2 violins)
#'
#' The Cat's Eye plot idea comes from Cumming 2013, Ch. 4. It makes sense here because,
#' unlike confidence intervals, we are using draws from the posterior distribution.
#'
#' @param fit a fitted model object
#' @param coef.names an optional vector containing the desired coefficient names in the output.
#' @param parse.coef parse the coef names in the output. See \code{\link{plotmath}} for the syntax.
#' @param digits number of decimal digits to show per coefficient. If NA the estimates are not shown
#'  this is the default too avoid having a busy plot
#' @param order order coefficients per estimate value.
#'   When set to TRUE, beware of coefficient scales to avoid misleading results and
#'   consider standardizing or any other form of rescaling coefficients
coef_catseye <- function(fit, coef.names=NULL, parse.coef=F, digits=1, order.coef=F, ...){
  UseMethod("coef_catseye")
}

#' Plots the coefficients of the posterior draws of an lm regression using Cat's eye plots (via ggplot2 violins)
#'
#' The Cat's Eye plot idea comes from Cumming 2013, Ch. 4. It makes sense here because,
#' unlike confidence intervals, we are using draws from the posterior distribution.
#' This method uses \code{\link{arm::sim}} to get the posterior simulations.
#'
#' @param fit an lm object
#' @param intercept include the intercept in the plot?
#' @param sigma include the residual sd in the plot?
#' @param post.sims number of simulation draws from the posterior. Defaults to 1000
#' @param ... further parameters passed to \code{\link{.coef_plot}}
#'
#' @examples
#'
#' fit1 <- lm(weight ~ Diet, data=ChickWeight)
#' coef_catseye(fit1, order.coef=T)
#'
#' # Skewed sigma
#' fit2 <- lm(weight ~ Diet, data=ChickWeight[sample(nrow(ChickWeight), 20),])
#' coef_catseye(fit2, sigma=T)
coef_catseye.lm <- function(fit, intercept=T, sigma=F, post.sims=1000, ...){
  if (require(arm)){
    fit.sim <- sim(fit, post.sims)
    sim.coef <- fit.sim@coef
    sim.coef <- data.frame(sim.coef)
    # Give default names to predictors
    names(sim.coef) <- names(coef(fit))
    if(!intercept){
      sim.coef <- sim.coef[,-1]
    }
    if(sigma){
      sim.coef <- cbind(sim.coef, sigma=fit.sim@sigma)
    }
    coef_catseye_mcmc(sim.coef, ...)
  }else{
    warning("'arm' package is required in order to use draws from the posterior distribution of lm")
    post.sims = 0
  }
}

#' Plots the coefficients of MCMC draws using Cat's eye plots (via ggplot2 violins)
#'
#' The Cat's Eye plot idea comes from Cumming 2013, Ch. 4. It makes sense here because,
#' unlike confidence intervals, MCMC draws do represent a distribution.
#' Although this method is intended for MCMC draws, it can be used with any variable composed of numeric elements.
#' The input is cast to a "long" data frame using \code{\link{reshape2::melt}}, so it's important to either have
#' the input set with (col)names or pass meaningful names via coef.names to avoid "noise" or blank coefficient names
#' as axis breaks
#'
#' @param mcmc usually a data frame, matrix, list of vectors or vector containing MCMC draws
#' @param coef.names an optional vector containing the desired coefficient names in the output.
#' @param parse.coef parse the coef names in the output. See \code{\link{plotmath}} for the syntax.
#' @param digits number of decimal digits to show per coefficient. If NA the estimates are not shown
#'  this is the default too avoid having a busy plot
#' @param order order coefficients per estimate value.
#'   When set to TRUE, beware of coefficient scales to avoid misleading results and
#'   consider standardizing or any other form of rescaling coefficients
coef_catseye_mcmc <- function(mcmc, coef.names=NULL, parse.coef=F, digits=NA, order.coef=F){
  betas <- reshape2::melt(as.data.frame(mcmc), variable.name="coefficient")
  if(!require(dplyr)) stop("The coef_catseye plot requires the dplyr library")
  # Create a summarized data frame
  fit.coef <- betas %>%
    group_by(coefficient) %>%
    summarize(mu = mean(value))

  # Replace coefficient names
  .coef.names(fit.coef) <- coef.names
  # Reorder coefficients in the summarized data frame
  .coef.order(fit.coef) <- order.coef
  # And the original data frame
  betas$coefficient <- with(betas, factor(as.character(coefficient), levels = as.character(fit.coef$coefficient)))
  # This allows to round mu within geom_text
  fit.coef$digits <- digits

  p <- ggplot(fit.coef, aes(x=coefficient, y=mu)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_violin(aes(x=coefficient, y=value), data=betas, colour="grey", adjust=2, fill=NA) +
    geom_violin(aes(x=coefficient, y=value), data= betas %>%
                  group_by(coefficient) %>%
                  filter(value > quantile(value, .025) &
                           value < quantile(value, .975)), adjust=2, fill=NA) +
    geom_point(colour="#e41a1c")
  if(!is.na(digits)){
    p <- p + # CI
      geom_text(aes(label=paste0(round(mu, digits),"\n")))
  }
  # Possible thanks to magrittr
  p %>% .coef_plot_deco(fit.coef, parse.coef=parse.coef)
}

#' Replaces the values of "fit.coef$coefficient" by those given by "value"
#' @param fit.coef a data frame containing a "coefficient"
#' @param value A vector of replacement values. length(value) == length(unique(fit.coef$coefficients))
`.coef.names<-` <- function(fit.coef, value){
  if(!is.null(value)){
    fit.coef <- within(fit.coef, {
      coefficient <- plyr::mapvalues(coefficient, levels(coefficient), value)
    })
  }
  fit.coef
}

#' Re-orders fit.coef by fit.coef$mu, levels of fit.coef$coefficient are also reordered
#' @param fit.coef a data frame containing "coefficient" and "mu" columns
#' @param value A number defining the order:
#'  "ascending" : (value > 0) | (value == T)
#'  "descending" : value < 0
#'  "as-is" : value == (0 | F)
`.coef.order<-` <- function(fit.coef, value){
  if(value){
    fit.coef <- fit.coef[order(fit.coef$mu, decreasing = value < 0), ]
    # Reorder coefficient names keeping order
    fit.coef$coefficient <- factor(as.character(fit.coef$coefficient), levels = as.character(fit.coef$coefficient))
  }
  fit.coef
}

#' Flips coordinates, adds scale (parsed) labels, names and \code{\link{theme_bw}} to the plot
#' @param p plot to modify
#' @param fit.coef a data frame containing a "coefficient" columns, which may be parsed
#' @param parse.coef should scale labels be the parsed values of fit.coef$coefficient? See \code{\link{plotmath}} for the syntax.
.coef_plot_deco <- function(p, fit.coef, parse.coef){
  # Parse the text of the factors to make them expressions
  labs <- sapply(levels(fit.coef$coefficient),
                 function(x){
                   ifelse(parse.coef, parse(text = x), x)
                 })
  names(labs) <- levels(fit.coef$coefficient)

  p + scale_x_discrete("Coefficient", labels = labs)+
    scale_y_continuous("Estimate") +
    coord_flip() +
    theme_bw()
}
