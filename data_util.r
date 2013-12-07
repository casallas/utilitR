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