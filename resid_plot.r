library(ggplot2)
#' Residual plot of an lm fit using ggplot2
#' @param fit an lm object
#' @param smooth add a smoother line to the residuals? Default is FALSE
resid_plot.lm <- function(fit, smooth=F){
  p <- ggplot(aes(x=.fitted, y=.resid), data=fit) +
    geom_hline(yintercept=0, colour="grey") +
	geom_hline(yintercept=c(-1,1)*summary(fit1.f)$sigma, linetype="dashed", colour="grey")
  if(smooth){
    p <- p + geom_smooth(colour="#377eb8", se=F)
  }	
  p +
	geom_point(colour="#e41a1c") +
	theme_bw()
}
