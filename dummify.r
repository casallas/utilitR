#' Gets the string separator between variables and their values
#'
#' By default the separator is "", which imitates the dummy coding used by functions as \code{\link{lm}} on factors,
#' but can be given other values that may seem more intuitive, e.g. for plotting, like "=="
#' To change the default separator call \code\{link{dummify.set_sep}}
dummify.sep <- function(){
  if(!exists("dummify.__sep")){
    dummify.set_sep()
  }
  dummify.__sep
}

#' Sets the default string separator between variables and their values
#'
#' By default the separator is "", which imitates the dummy coding used by functions as \code{\link{lm}} on factors,
#' but can be given other values that may seem more intuitive, e.g. for plotting, like "=="
#' Changing the default separator will change the default column names returned by \code{\link{dummify}}, and \code{\link{dummify_interaction}}
dummify.set_sep <- function(sep=""){
  dummify.__sep <<- sep
}

#' Gets the string separator between two interaction variables
#'
#' By default the separator is ":", which imitates the dummy coding used by functions as \code{\link{lm}} on factors,
#'   but can be given other values that may seem more intuitive, e.g. for plotting, like "%*%"
#' To change the default interaction separator call \code\{link{dummify.set_sep_inter}}
dummify.sep_inter <- function(){
  if(!exists("dummify.__sep_inter")){
    dummify.set_sep_inter()
  }
  dummify.__sep_inter
}

#' Gets the string separator between two interaction variables
#'
#' By default the separator is ":", which imitates the dummy coding used by functions as \code{\link{lm}} on factors,
#'   but can be given other values that may seem more intuitive, e.g. for plotting, like "%*%"
#' To change the default interaction separator call \code\{link{dummify.set_sep_inter}}
#' Changing the default separator will change the default column names returned by \code{\link{dummify_interaction}}
dummify.set_sep_inter <- function(sep_inter=":"){
  dummify.__sep_inter <<- sep_inter
}

#' Returns a data frame containing the dummy coded given variable
#' @param x variable to dummy code; it can be of any type that can be converted to a factor
#' @param ref the reference, or "0", value of the variable. See \code\{\link{relevel}}
#' @param sep string that separates the variable from its values.
#'   By default it uses the separator given by \code{\link{dummify.sep}}.
#'   To change the default separator call \code\{link{dummify.set_sep}}
#' @return a data.frame with the dummy coding of x.
#'   \code{nrow(dummify(x)) = length(x)}, \code{ncol(dummify(x)) = length(unique(x))-1}.
#'   Note that the column names of the data frame may not be "syntactically valid". See \code\{\link{make.names}}
#'   for information on syntactically valid names
dummify <- function(x, ref=NULL, sep=dummify.sep()){
  x.name <- deparse(substitute(x))
  x <- factor(x)
  if(!is.null(ref)){
	  x <- relevel(x, ref)
  }
  x.dum <- list()
  for(lv in levels(x)[-1]){
    x.dum[[paste(x.name, lv, sep=sep)]] <- as.numeric(x==lv)
  }
  data.frame(x.dum, check.names=F)
}

#' Returns a data frame containing the dummy coded interaction between two variables
#' @param x first variable of the interaction; it can be of any type that can be converted to a factor
#' @param y first variable of the interaction; it can be of any type that can be converted to a factor
#' @param ref.x the reference, or "0", value of the x variable. See \code\{\link{relevel}}
#' @param ref.y the reference, or "0", value of the x variable. See \code\{\link{relevel}}
#' @param sep string that separates x and y from their values.
#'   By default it uses the separator given by \code{\link{dummify.sep}}.
#'   To change the default separator call \code\{link{dummify.set_sep}}
#' @param sep.inter string that separates the x and y.
#'   By default it uses the separator given by \code{\link{dummify.sep_inter}}.
#'   To change the default separator call \code\{link{dummify.set_sep_inter}}
#' @return a data.frame with the dummy coding of the interaction between x and y.
#'   Note that the column names of the data frame may not be "syntactically valid". See \code\{\link{make.names}}
#'   for information on syntactically valid names
dummify_interaction <- function(x, y, ref.x=NULL, ref.y=NULL, sep=dummify.sep(), sep.inter=dummify.sep_inter()){
  x.name <- deparse(substitute(x))
  y.name <- deparse(substitute(y))
  x <- factor(x)
  if(!is.null(ref.x)){
	  x <- relevel(x, ref.x)
  }
  y <- factor(y)
  if(!is.null(ref.y)){
	  y <- relevel(y, ref.y)
  }
  xy.dum <- list()
  for(lv.x in levels(x)[-1]){
    for(lv.y in levels(y)[-1]){
      xy.dum[[paste0(x.name, sep, lv.x, sep.inter, y.name, sep, lv.y)]] <-
        as.numeric(x==lv.x & y==lv.y)
    }
  }
  data.frame(xy.dum, check.names=F)
}
