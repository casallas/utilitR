# Juan Sebastian Casallas, 2014
# Utils for the ordinal package

# An equidistance graphical test
# @param fit.clm a clm or clmm object
# @param numThresh the number of thresholds in fit.clm
# @param n.sims the number of simulations to calculate the 95% CI of the difference
equidist_gtest <- function(fit.clm, numThresh, n.sims = 1e04){
  # Threshold estimates
  gamma.hat <- coef(fit.clm)[1:numThresh]
  gamma.diff <- diff(gamma.hat)
  # Get the standard error for the threshold coefficients (second column)
  gamma.se <- summary(fit.clm)$coefficients[1:numThresh, 2]

  gamma.sim <- list()
  gamma.diff.se <- list()
  gamma.diff.CI95 <- list()
  for(g in 1:numThresh){
    gamma.sim[[names(gamma.hat)[g]]] <- rnorm(n.sims, gamma.hat[[g]], gamma.se[[g]])
    if(g != 1){
      gamma.sim.diff <- gamma.sim[[g]] - gamma.sim[[g-1]]
      # +/-1 se ~= (0.159, .841) quantiles
      gamma.diff.se[[names(gamma.hat)[g]]] <- quantile(gamma.sim.diff, c(0.159, 0.841))
      # 95% CI
      gamma.diff.CI95[[names(gamma.hat)[g]]] <- quantile(gamma.sim.diff, c(0.025, 0.975))
    }
  }
  gamma.diff.df <- as.data.frame(t(rbind(gamma.diff,
                                         simplify2array(gamma.diff.se),
                                         simplify2array(gamma.diff.CI95))))
  gamma.diff.df$gammas <- paste(rownames(gamma.diff.df), "-", names(gamma.hat)[1:(numThresh-1)])
  colnames(gamma.diff.df)[2:3] <- c("p159", "p841")
  colnames(gamma.diff.df)[4:5] <- c("p025", "p975")
  rownames(gamma.diff.df) <- NULL

  ggplot(gamma.diff.df, aes(gammas, gamma.diff, ymin = p025, ymax = p975)) +
    scale_x_discrete("Thresholds") +
    scale_y_continuous("Distance") +
    geom_pointrange(aes(ymin = p159, ymax = p841), size = 1) + # se
    geom_pointrange() + # CI
    coord_flip() +
    theme_bw()
}

# Compares the coefficients of clmm and lmer graphically
# @param fit.clmm a clmm object
# @param fit.lmer
# @param numThresh the number of thresholds
ord_int_gtest <- function(fit.clmm, fit.lmer, numThresh){
  fit.clmm.se <- summary(fit.clmm)$coefficients[,2] # se is col 2
  fit.clmm.CI <- confint(fit.clmm)
  fit.clmm.coef <- data.frame(
    mu = coef(fit.clmm),
    p025 = fit.clmm.CI[,1],
    p975 = fit.clmm.CI[,2],
    se = fit.clmm.se
  )[-(1:numThresh),] # Exclude thresholds

  fit.clmm.coef <- within(fit.clmm.coef,{
    # Create cols for +/-1 se
    p159 <- mu - se
    p841 <- mu + se
    # Add a column for the evaluated factor based on row names, keeping order
    Factor <- factor(rownames(fit.clmm.coef), levels = rownames(fit.clmm.coef))
    # Add a column for the method
    Method <- factor("clmm")
  })

  fit.lmer.sigma <- summary(fit.lmer)$sigma
  fit.lmer.se <- summary(fit.lmer)$coefficients[-1, 2] # Exclude intercept
  fit.lmer.CI <- confint(fit.lmer)
  fit.lmer.coef <- data.frame(
    mu = lme4::fixef(fit.lmer)[-1]/fit.lmer.sigma, # Exclude intercept
    p025 = fit.lmer.CI[-(1:3), 1]/fit.lmer.sigma,  # Exclude .sig01, .sigma and intercept
    p975 = fit.lmer.CI[-(1:3), 2]/fit.lmer.sigma,
    se = fit.lmer.se/fit.lmer.sigma
  )
  fit.lmer.coef <- within(fit.lmer.coef,{
    # Create cols for +/-1 se
    p159 <- mu - se
    p841 <- mu + se
    # Add a column for the evaluated factor based on row names, keeping order
    Factor <- factor(rownames(fit.lmer.coef), levels = rownames(fit.lmer.coef))
    # Add a column for the method
    Method <- factor("lmm")
  })

  ## Do some remapping to show factors as expressions
  # Interaction factors are those with a ":"
  interFactors <- with(fit.lmer.coef,levels(Factor)[stringr::str_detect(levels(Factor),":")])
  # Add prefix and infix parentheses to interactions
  interFactorsMod <- paste0("(", interFactors, ")")
  interFactorsMod <- stringr::str_replace_all(interFactorsMod,":", ") %*% (")

  coef.df <- within(rbind(fit.clmm.coef, fit.lmer.coef),{
    # Add prefix and infix parentheses to interactions (if any)
    if(length(interFactors)>0)
      Factor <- plyr::mapvalues(Factor, interFactors, interFactorsMod)
    Factor <- plyr::mapvalues(Factor, levels(Factor),
                              stringr::str_replace_all(levels(Factor),"\\.nom", " == "))
    Factor <- plyr::mapvalues(Factor, levels(Factor),
                              stringr::str_replace_all(levels(Factor),"th", "theta"))
  })

  dodge <- position_dodge(width=0.4)

  # Parse the text of the factors to make them expressions
  labs <- sapply(levels(coef.df$Factor), function(x) parse(text = x))
  names(labs) <- levels(coef.df$Factor)

  ggplot(coef.df, aes(Factor, mu, ymin = p025, ymax = p975, group = Method, colour = Method)) +
    scale_x_discrete("Factor", labels = labs)+
    scale_y_continuous("Coefficient") +
    geom_pointrange(aes(ymin = p159, ymax = p841), position=dodge, size = 1) + # se
    geom_pointrange(position=dodge) + # CI
    geom_hline(yintercept=0, linetype="dashed") +
    coord_flip() +
    theme_bw() +
    theme(legend.position="top") +
    scale_colour_brewer(palette="Set1")
}
