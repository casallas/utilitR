# Juan Sebastian Casallas, 2013
# Data Utils

# Gets the values of a numeric vector that fall into a string interval
# x: numeric vector
# interval: string interval in the form '[num,num]', '(num,num)', '(num,num]' or '[num,num)'
# value: the values of x that fall into the given interval
# example: interval_str(1:10,'(4,9]')
interval_str <- function(x,interval){
	parens <- simplify2array(stringr::str_split(interval,'\\d*,\\d*'))
	lims <- substr(interval,2,nchar(interval)-1)
	lims <- sapply(stringr::str_split(lims,','),as.numeric)
	
	ans <- (x > lims[1]) & (x < lims[2]) # closed parentheses by default
	
	if(parens[1]=='[')
		ans <- ans | (x == lims[1])
	if(parens[2]==']')
		ans <- ans | (x == lims[2])
	x[ans]
}

# x: ordinal vector, whose levels will be remapped by cutting
# intervals: number of intervals into which the levels of 'x' should be cut
# value: 'x', with it's levels remapped according to the given intervals
# Note: this function can also work with unordered factors, but the new categories will be meaningless
# See also: base::cut, plyr::mapvalues
# example:
# lvls <- c('Strongly disagree','Disagree','Neither agree nor disagree','Agree','Strongly agree')
# ans <- sapply(1+.Random.seed[1:16] %% length(lvls),function(x){lvls[x]}) # Generate 16 random answers
# ans <- ordered(ans,lvls) # Establish the order of the answers
# cut_ordinal(ans,2) # Cut answers into two categories
cut_ordinal <- function(x,intervals){
	lvls <- levels(x)
	num_lvls <- seq_along(lvls)
	remap_lvls <- cut(num_lvls,intervals)
	# Cut gives us factor intervals, but we want them as indices, to get the corresponding level names
	remap_lvls <- lapply(as.character(remap_lvls),function(interval){
		interval_str(num_lvls,interval)
	})
	# Now we can get the names of the levels corresponding to those indices
	remap_lvls <- sapply(remap_lvls,function(indx){paste(lvls[indx],collapse=',')})
	# Remap levels accordingly
	plyr::mapvalues(x,lvls,remap_lvls)
}

# Encodes a vector as an ordered factor, whose levels are ordered ascendingly by applying a numeric function
# x: a vector
# num_func: a function that gives a numeric value to each member of x
ordered_func <- function(x, num_func){
	# Get the current unique values in x, omitting NA
	cur_x <- as.character(unique(na.omit(x)))
	# Order them using the value given by the numeric function
	cur_x <- cur_x[order(num_func(cur_x))]

	# Make values ordinal, with the ordered levels
	ordered(x,levels=cur_x)
}

# Wrapper of reshape::melt.data.frame, when all the measure vars produce only one type of measure
# thus, the "value" column produced by melt, can be renamed accordingly.
# Note: This is the same as calling df <- melt(...); colnames(df)[ncol(df)] <- measure_name
melt_1measure <- function(data, id.vars, measure.vars, measure_name, variable_name = "variable", na.rm = FALSE){
	df <- reshape::melt.data.frame(data, id.vars, measure.vars, variable_name, na.rm = na.rm)
	colnames(df)[ncol(df)] <- measure_name
	df
}

# Wrapper of plyr::mapvalues, when all elements in "from" should be mapped to only one element
# to1: single element to which all elements in "from" will be mapped
# Note: This is equivalent to calling mapvalues(x, from, rep(to, length(from)))
mapvalues_to1 <- function(x, from, to, warn_missing = TRUE){
	plyr::mapvalues(x, from, rep(to, length(from)), warn_missing = warn_missing)
}

# Replaces a string in a string vector that matches exactly the given string
# within_str: string vector on which the replacement will be made
# matching_str: a literal string to be matched
# replacement: the replacement string vector, or single string
str_replace_exact <- function(within_str, matching_str, replacement){
  within_str[within_str==matching_str] <- replacement
  within_str
}

# Returns the number of unique entries in a given vector
n.unique <- function(vec){
  length(unique(vec))
}

# Reads an RDS file or runs a function, saves and returns its value
readOrRunSave <- function(filePath, fun, forceRun = F, ...){
  if(!forceRun & file.exists(filePath)){
    readRDS(filePath)
  }else{
    obj <- fun(...)
    saveRDS(obj, filePath)
    obj
  }
}

#' Gets a list with the point estimate (mu) and HDI (hdi.lo, hdi.hi) of an MCMC chain
#'
#' This function uses \link{\code{median}} to calculate the point estimate and \link{\code{BEST::HDI}}
#' to calculate the HDI
#'
#' @param mcmc a numeric vector containing the mcmc draws
#' @param cred.mass a 0..1 number specifying the mass within the HDI
#' @return a list with "mu": point estimate
#'                     "hdi.lo": lower bound of the HDI
#'                     "hdi.hi": higher bound of the HDI
#'                     "cred.mass": credible mass
#' @examples
#' summary_mcmc(rnorm(1000))
summary_mcmc <- function(mcmc, cred.mass=0.95){
  mcmc.hdi <- as.numeric(BEST::hdi(mcmc, cred.mass))
  list(mu = median(mcmc), hdi.lo = mcmc.hdi[1], hdi.hi = mcmc.hdi[2], cred.mass = cred.mass)
}

#' Executes summary_mcmc on the columns of a matrix or dataframe, and returns a data frame
summary_mcmcs <- function(mcmcs, cred.mass=0.95){
  do.call(rbind.data.frame,
          apply(mcmcs, 2, summary_mcmc, cred.mass))
}

#' Gives the probability that one set of mcmc draws is greater than another
#'
#' lhs and rhs must both be numeric vectors, with equal lengths
#' @examples
#' p_greater_mcmc(rnorm(1000, mean=1), rnorm(1000))
p_greater_mcmc <- function(lhs, rhs){
  sum(lhs > rhs)/length(lhs)
}
