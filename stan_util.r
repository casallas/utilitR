#' Original function by Gelman posted in the stan user group?
#' @return variance of each column in a
colVars <- function (a){
  diff <- a - matrix (colMeans(a), nrow(a), ncol(a), byrow=TRUE)
  vars <- colMeans (diff^2)*nrow(a)/(nrow(a)-1)
  return (vars)
  #apply(a, 2, var) # TODO faster?
}

#' Wrapper for .WAIC specific for stanfit objects
WAIC.stanfit <- function (stanfit){
  log_lik <- extract (stanfit, "log_lik")$log_lik
  .WAIC(log_lik)
}

#' Original function by Gelman posted in the stan user group?
#' waic calculation follows the recommendation in BDA
#' TODO maybe migrate to mcmc_util
#' @return lppd, p_waic_1, p_waic_2, and waic, defined as 2*(lppd - p_waic_2)
.WAIC <- function (log_lik){
  lppd <- sum (log (colMeans(exp(log_lik))))
  p_waic_1 <- 2*sum (log(colMeans(exp(log_lik))) - colMeans(log_lik))
  p_waic_2 <- sum (colVars(log_lik))
  waic_2 <- -2*lppd + 2*p_waic_2
  list (waic=waic_2, p_waic=p_waic_2, lppd=lppd, p_waic_1=p_waic_1)
}

#' Wrapper for .DIC specific for stanfit objects
DIC.stanfit <- function(stanfit){
  dev <- extract(stanfit, "dev")$dev
  .DIC(dev)
}

#' DIC using PD_alt as explained in equation 7.10 of BDA 3 (Gelman 2013),
#'  or pd(2) in p. 186 of BDA 2
#' TODO maybe migrate to mcmc_util
#' @return dev, penalty, DIC, defined as mean(dev) + var(dev)/2
.DIC <- function(dev){
  list(dev=mean(dev), penalty=.5*var(dev), DIC=mean(dev) + .5*var(dev))
}

#' Calls stanc and stan_model
#' TODO document
stanc_model <- function(file, model_name){
  rt <- stanc(file, model_name = model_name)
  stan_model(stanc_ret=rt)
}

#' reads a stan model, otherwise it compiles it and saves it
#' TODO document
# source("data_util.r") # readOrRunSave
readCompile_stan <- function(model_name, file.rds=NULL, file.stan=NULL){
	if(is.null(file.stan)) file.stan = paste0(model_name,".stan")
	if(is.null(file.rds)) file.rds = paste0(model_name,".rds")
	readOrRunSave(filePath=file.rds, fun=stanc_model, file=file.stan, model_name=model_name)
}

#' Extracts one parameter from a stanfit object
#' TODO document
extract1 <- function(fit, .par){
  rstan::extract(fit, .par)[[.par]]
}

#' Tries sampling several times with different seeds
#' TODO document
try_sampling <- function(seed, chain_id, mdl, data, iter=100, n.trials=100, ...) {
    for(i in 1:n.trials){
      cat("XXChain: ", chain_id, " trial: ", i, "XX\n")
      init <- "random"
      if(i == n.trials){
        cat("VVVChain: ", chain_id, " failed after ", i, ' trials, using "0" initVVV\n')
        init <- "0"
      }
      my_start <- Sys.time()
      ans <- sampling(mdl, data=data, init=init, iter=iter, seed = seed, chains = 1, chain_id = chain_id, refresh = ifelse(chain_id == 1, 200, -1), ...)
      if(length(ans@sim)!=0){
        cat("<<Chain ", chain_id, " ended after", i," trials, in ", Sys.time() - my_start, " seconds, with ", init, "init>>\n")
        cat("<<Chain ", chain_id, " saved>>\n")
        return(ans)
      }
    }
    cat("XXXChain: ", chain_id, " failed after ", n.trials, " trialsXXX\n")
}

#' parallel stan::sampling call
#' TODO document
psampling <- function(mdl, data, chains=4, iter=100, seed=sample.int(.Machine$integer.max, 1), mc.cores=2, ...){
  stopifnot(require("parallel"))
  sflist1 <- mclapply(1:chains, mc.cores = mc.cores, function(i) try_sampling(seed=seed, chain_id=i, mdl=mdl, data=data, iter=iter))
  sflist2stanfit(sflist1)
}
