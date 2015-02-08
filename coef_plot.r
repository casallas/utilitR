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
    outerCI.0 = fit.CI[,1],
    outerCI.1 = fit.CI[,2],
    se = fit.se
  )
  if(!intercept){
    fit.coef <- fit.coef[-1,]
  }

  fit.coef <- within(fit.coef,{
    # Create cols for +/-1 se
    innerCI.0 <- mu - se
    innerCI.1 <- mu + se
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
#'
#' @examples
#' if(require(arm)){
#'   fit1 <- lm(weight ~ Diet, data=ChickWeight)
#'   fit1.sim <- sim(fit1, 10000)
#'   # Fix coef names in the simulation
#'   colnames(fit1.sim@coef) <- names(coef(fit1))
#'   coef_plot_mcmc(fit1.sim@coef)
#' }
coef_plot_mcmc <- function(mcmc, innerCredMass = innerCM(), outerCredMass = outerCM(), ...){
  betas <- reshape2::melt(as.data.frame(mcmc), variable.name="coefficient")
  if(!require("dplyr")) stop("dplyr library not installed")
  fit.coef <- betas %>%
    group_by(coefficient) %>%
    summarize(mu = median(value),
              outerCI.0 = .CI(value, outerCredMass)[1],
              outerCI.1 = .CI(value, outerCredMass)[2],
              innerCI.0 = .CI(value, innerCredMass)[1],
              innerCI.1 = .CI(value, innerCredMass)[2])
  .coef_plot(fit.coef, ...)
}

#' Plots the coefficients of a fit using ggplot2
#'
#' Users should call the higher-level generic "coef_plot", or implement a method for the
#' corresponding class to get fit.coef from the specific object
#'
#' @param fit.coef a data frame containing columns coefficient, mu, outerCI.0, outerCI.1, innerCI.0, and innerCI.1.
#' @param coef.names an optional vector containing the desired coefficient names in the output.
#' @param parse.coef parse the coef names in the output. See \code{\link{plotmath}} for the syntax.
#' @param digits number of decimal digits to show per coefficient. If NA the estimates are not shown.
#' @param order order coefficients per estimate value.
#'   When set to TRUE, beware of coefficient scales to avoid misleading results and
#'   consider standardizing (e.g. using \code{\link{arm::standardize}}).
#' @param math.mu When set to TRUE estimates (fit.coef) are enclosed within $..$
#' @param math.mu_axis When set to TRUE the labels in the mu (x) axis are enclosed within $.$
.coef_plot <- function(fit.coef, coef.names=NULL, parse.coef=F, digits=1, order.coef=F,
                       math.mu = F, math.mu_axis = math.mu){
  # Replace coefficient names
  .coef.names(fit.coef) <- coef.names
  # Reorder coefficients
  .coef.order(fit.coef) <- order.coef
  # This allows to round mu within geom_text
  fit.coef$digits <- digits
  # mu enclosing character
  fit.coef$mu.enc <- ifelse(math.mu, "$", "")

  p <- ggplot(fit.coef, aes(x=coefficient, y=mu, ymin = outerCI.0, ymax = outerCI.1)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_linerange(colour=coef_color()) + # outer interval
    geom_linerange(aes(ymin = innerCI.0, ymax = innerCI.1), size = innerCI_size(), colour=coef_color()) + # inner interval
    geom_point(colour=coef_color(), size=mu_size()) # point estimate
  if(!is.na(digits)){
    p <- p + # CI
      geom_text(aes(label=paste0(mu.enc, round(mu, digits), mu.enc, "\n")))
  }
  if(!require(magrittr)) stop("magrittr library not installed")
  p %>% .coef_plot_deco(fit.coef, parse.coef=parse.coef, math.mu_axis = math.mu_axis)
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
#' library(arm)
#' coef_catseye(standardize(lm(dist ~ speed, data=cars), standardize.y=T))
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
coef_catseye_mcmc <- function(mcmc, coef.names=NULL, parse.coef=F, digits=NA, order.coef=F,
                              credMass = outerCM()){
  betas <- reshape2::melt(as.data.frame(mcmc), variable.name="coefficient")
  if(!require(dplyr)) stop("The coef_catseye plot requires the dplyr library")
  # Create a summarized data frame
  fit.coef <- betas %>%
    group_by(coefficient) %>%
    summarize(mu = median(value))

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
                                                   filter(value > .CI(value, credMass)[1] &
                                                          value < .CI(value, credMass)[2]),
                adjust=2, fill=NA) +
    geom_point(size=mu_size(), colour=coef_color())
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
#' @param math.mu_axis When set to TRUE the labels in the mu (y) axis are enclosed within $.$
.coef_plot_deco <- function(p, fit.coef, parse.coef, math.mu_axis = F){
  # Parse the text of the factors to make them expressions
  labs <- sapply(levels(fit.coef$coefficient),
                 function(x){
                   ifelse(parse.coef, parse(text = x), x)
                 })
  names(labs) <- levels(fit.coef$coefficient)

  p <- p + scale_x_discrete("Coefficient", labels = labs)
  if(math.mu_axis)
    p <- p + scale_y_continuous("Estimate", labels = tex_math)
  else
    p <- p + scale_y_continuous("Estimate")
  p +
    coord_flip() +
    theme_bw()
}

#' Gets the inner Credible Mass for MCMC chains
#'
#' By default the inner Credible Mass is 0.5
#' To change the default inner Credible Mass call \code\{link{set_innerCM}}
innerCM <- function(){
  if(!exists("coef_plot.__innerCM")){
    set_innerCM()
  }
  coef_plot.__innerCM
}

#' Sets the inner Credible Mass of MCMC chains
#'
#' By default the inner Credible Mass is 0.5
#' Changing the default Credible Mass will change the plots generated by \code{\link{coef_plot_mcmc}}, and \code{\link{coef_catseye_mcmc}}
set_innerCM <- function(credMass=0.5){
  coef_plot.__innerCM <<- credMass
}

#' Gets the outer Credible Mass for MCMC chains
#'
#' By default the outer Credible Mass is 0.5
#' To change the default outer Credible Mass call \code\{link{set_outerCM}}
outerCM <- function(){
  if(!exists("coef_plot.__outerCM")){
    set_outerCM()
  }
  coef_plot.__outerCM
}

#' Sets the outer Credible Mass of MCMC chains
#'
#' By default the outer Credible Mass is 0.95
#' Changing the default Credible Mass will change the plots generated by \code{\link{coef_plot_mcmc}}, and \code{\link{coef_catseye_mcmc}}
set_outerCM <- function(credMass=0.95){
  coef_plot.__outerCM <<- credMass
}

#' Gets the Credible Interval for MCMC chains
#'
#' To change this function use \code\link{set_CI.fun}}
.CI <- function(mcmc, credMass=0.95){
  CI.fun()(mcmc, credMass)
}

#' Gets the Credible Interval function for MCMC chains
#'
#' By default the Credible Interval function is \code\{link{BEST::hdi}}
#' To change the default Credible Interval function call \code\{link{set_CI.fun}}
CI.fun <- function(){
  if(!exists("coef_plot.__CI.fun")){
    set_CI.fun()
  }
  coef_plot.__CI.fun
}

#' Sets the Credible Interval function for MCMC chains
#'
#' By default the Credible Interval function is \code\{link{BEST::hdi}}
#' Changing the default Credible Interval function will change the plots generated by \code{\link{coef_plot_mcmc}}, and \code{\link{coef_catseye_mcmc}}
#' @examples
#' set_CI.fun(credSet)
set_CI.fun <- function(CI_fun = BEST::hdi){
  coef_plot.__CI.fun <<- CI_fun
}

#' Gets the Credible Set for MCMC chains
#' @see BEST::hdi for the Highest Density Interval
credSet <- function(mcmc, credMass = 0.95){
  quantile(mcmc, c(0, 1) + c(1, -1)*((1-credMass)/2))
}

#' Gets the color for the points and lineranges of coef_plot and catseye_plot
#'
#' By default the color is #e41a1c, which corresponds to the first color in ColorBrewer's Set1
#' To change the default color, call \code\{link{set_coef_color}}
coef_color <- function(){
  if(!exists("coef_plot.__coef_color")){
    set_coef_color()
  }
  coef_plot.__coef_color
}

# Aesthetics

#' Sets the color for the points and lineranges of coef_plot and catseye_plot
#'
#' By default the color is #e41a1c, which corresponds to the first color in ColorBrewer's Set1
#'
#' @examples
#' set_coef_color("blue")
#' f1 <- lm(weight ~ Diet, data = ChickWeight)
#' coef_plot(f1)
#' coef_catseye(f1)
set_coef_color <- function(color="#e41a1c"){
  coef_plot.__coef_color <<- color
}

#' Gets the inner interval line size of coef_plot
#'
#' By default the inner interval line size is 2
#' To change the default inner interval size call \code\{link{set_innerCI_size}}
innerCI_size <- function(){
  if(!exists("coef_plot.__innerCI_size")){
    set_innerCI_size()
  }
  coef_plot.__innerCI_size
}

#' sets the inner interval line size of coef_plot
#'
#' By default the inner interval line size is 2
set_innerCI_size <- function(size=2){
  coef_plot.__innerCI_size <<- size
}

#' Gets the point estimate point size of coef_plot and catseye_plot
#'
#' By default the point size is 4
#' To change the default inner interval size call \code\{link{set_mu_size}}
mu_size <- function(){
  if(!exists("coef_plot.__mu_size")){
    set_mu_size()
  }
  coef_plot.__mu_size
}

#' Sets the point estimate point size of coef_plot and catseye_plot
#'
#' By default the point size is 4
set_mu_size <- function(size=4){
  coef_plot.__mu_size <<- size
}
