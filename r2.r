# Adjusted r2
# Uses the standard definition of r2, for r2.rob, use r2.fun=r2.rob
# p is the number of parameters *including* the intercept
adj.r2 <- function(y, y.hat, p, r2.fun = r2){
  n <- length(y)
  1 - (1 - r2.fun(y, y.hat))*(n - 1)/(n - p)
}

# Standard definition of r2
r2 <- function(y, y.hat){
  y.bar <- mean(y)

  SSres <- sum((y - y.hat)^2)
  SStot <- sum((y - y.bar)^2)
  1 - (SSres/SStot)
}

# This corresponds to R_9^2. Eq. 11, p. 283 from Kvalseth 1985
# Is is useful for robust regression
r2.rob <- function(y, y.hat){
  y.bar <- mean(y)
  1 - median(abs(y - y.hat))/median(abs(y - y.bar))
}

.noop <- function(x)x

# trans specifies a function to transform the dv and predictions
# trans.y specifies a function to transform the dv
# trans.y.hat specifies a function to transform the predictions
r2.fit <- function(fit, y=predict(fit)+resid(fit), trans=NULL, trans.y.hat=NULL, trans.y=NULL){
  if(!is.null(trans))
    trans.y <- trans.y.hat <- trans
  if(is.null(trans.y))
    trans.y <- .noop
  if(is.null(trans.y.hat))
    trans.y.hat <- .noop
  
  r2(trans.y(y), trans.y.hat(predict(fit)))
}

adj.r2.fit <- function(fit, y=predict(fit)+resid(fit), p=length(coef(fit)), trans=NULL, trans.y.hat=NULL, trans.y=NULL){
  if(!is.null(trans))
    trans.y <- trans.y.hat <- trans
  if(is.null(trans.y))
    trans.y <- .noop
  if(is.null(trans.y.hat))
    trans.y.hat <- .noop
  
  adj.r2(trans.y(y), trans.y.hat(predict(fit)), p)
}

sigma <- function(y, y.hat, p){
  n <- length(y)
  SSres <- sum((y - y.hat)^2)
  sqrt(SSres/(n - p))
}

sigma.fit <- function(fit, y=predict(fit)+resid(fit), p=length(coef(fit)), trans=NULL, trans.y.hat=NULL, trans.y=NULL){
  if(!is.null(trans))
    trans.y <- trans.y.hat <- trans
  if(is.null(trans.y))
    trans.y <- .noop
  if(is.null(trans.y.hat))
    trans.y.hat <- .noop
  
  sigma(trans.y(y), trans.y.hat(predict(fit)), p)
}