library(ggplot2)
#' Plot of a simple linear regression based on an lm fit using ggplot2
#' @param fit an lm object
#' @param post.sims number of simulations from the joint posterior to display (requires arm package).
slr_plot.lm <- function(fit, post.sims=100){
  coefs <- coef(fit)
  coef_names <- names(coefs)
  col_names <- names(fit$model)
  # "sanitize" names giving defaults to avoid potential conflicts with ggplot2
  names(fit$model) <- c("Y", "X")
  
  if(post.sims > 0 ){
     if (require(arm)){
	fit.sim <- sim(fit, post.sims)
	sim.coef <- fit.sim@coef
	sim.coef <- data.frame(sim.coef)
	# Give default names to predictors
	names(sim.coef) <- paste0("b", 0:(ncol(sim.coef)-1))
      }else{
        warning("'arm' package is required in order to use post.sims, post.sims will be treated as zero")
        post.sims = 0
      }
  }
  else
  {
  post.sims = 0
  }
  
  p <- ggplot(aes(y=Y, x=X), data=fit$model)
  if(post.sims > 0){
	p <- p +
	  geom_abline(aes(intercept=b0, slope=b1), data=sim.coef, alpha=0.075)
  }
  p + geom_abline(intercept=coefs[[1]], slope=coefs[[2]]) +
	geom_point(colour="#e41a1c") +	
	ylab(col_names[1]) +
	xlab(col_names[2]) +
	theme_bw()
}
