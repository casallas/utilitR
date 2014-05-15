library("ggplot2")

#' Plots the coefficients of an lm fit using ggplot2
#' @param fit an lm object
#' @param intercept include the intercept in the plot?
#' @param coef.names an optional vector containing the desired coefficient names in the output.
#' @param parse.coef parse the coef names in the output. See \code{\link{plotmath}} for the syntax.
#' @param digits number of decimal digits to show per coefficient
#' @param order order coefficients per estimate value.
#'   When set to TRUE, beware of coefficient scales to avoid misleading results and 
#'   consider standardizing (e.g. using \code{\link{arm::standardize}}).
#'
#' @examples
#'
#' fit1 <- lm(price ~ carat + cut + depth + color + table + clarity + x + y + z, data=diamonds)
#' # Misleading result
#' coef_plot.lm(fit1, order.coef=T)
#' # Better
#' fit1.z <- arm::standardize(fit1, standardize.y = T)
#' coef_plot.lm(fit1.z, order.coef=T)
coef_plot.lm <- function(fit, intercept=T, coef.names=NULL, parse.coef=T, digits=1, order.coef=F){
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
  if(order.coef){
    fit.coef <- fit.coef[order(fit.coef$mu), ]
  }

  fit.coef <- within(fit.coef,{
    # Create cols for +/-1 se
    p159 <- mu - se
    p841 <- mu + se
    # Add a column for the evaluated factor based on row names, keeping order
    coefficient <- factor(rownames(fit.coef), levels = rownames(fit.coef))
  })
  
  if(!is.null(coef.names)){
    fit.coef <- within(fit.coef, {
      coefficient <- plyr::mapvalues(coefficient, levels(coefficient), coef.names)
    })
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
