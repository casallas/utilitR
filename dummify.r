#' Returns a data frame containing the dummy coded given variable
#' @param x variable to dummy code; it can be of any type that can be converted to a factor
#' @param ref the reference, or "0", value of the variable. See \code\{\link{relevel}}
#' @param sep string that separates the variable from its values.
#'   By default it's "", which imitates the dummy coding used by functions as \code{\link{lm}} on factors,
#'   but can be given other values that may seem more intuitive, e.g. for plotting, like "=="
#' @return a data.frame with the dummy coding of x.
#'   \code{nrow(dummify(x)) = length(x)}, \code{ncol(dummify(x)) = length(unique(x))-1}.
#'   Note that the column names of the data frame may not be "syntactically valid". See \code\{\link{make.names}}
#'   for information on syntactically valid names
dummify <- function(x, ref=NULL, sep=""){
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
#'   By default it's "", which imitates the dummy coding used by functions as \code{\link{lm}} on factors,
#'   but can be given other values that may seem more intuitive, e.g. for plotting, like "=="
#' @param sep.inter string that separates the x and y.
#'   By default it's ":", which imitates the dummy coding used by functions as \code{\link{lm}} on factors,
#'   but can be given other values that may seem more intuitive, e.g. for plotting, like "%*%"
#' @return a data.frame with the dummy coding of the interaction between x and y.
#'   Note that the column names of the data frame may not be "syntactically valid". See \code\{\link{make.names}}
#'   for information on syntactically valid names
dummify_interaction <- function(x, y, ref.x=NULL, ref.y=NULL, sep="", sep.inter=":"){
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
