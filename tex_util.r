#' Returns the argument in math mode
#'
#' @param arg any object that can be converted to character
#' @return "$arg$"
tex_math <- function(arg){
  paste0("$", arg, "$")
}

#' Returns a number in tex format
#'
#' The number is rounded and optionally enclosed within "$"
#'
#' @param num a numeric object
#' @param prefix Character string to prepend to the number estimate. For example "Mdn=". By default it's ""
#' @param postfix Character string to append to the point estimate. For example "cm". By default it's ""
#' @param math.mode if T, enclose output within "$"
#' @param digits digits number of decimal places to print
#' @param big.mark a character to use every 3 decimals before the decimal point
#' @param force.digits should the number of digits be enforced?
#' @param ... aditional options passed to formatC
tex_num <- function(num, prefix = "", postfix = "", math.mode = T,
                    digits=texu_digits(), big.mark = texu_big.mark(math.mode),
                    force.digits = F, ...){
  num <- formatC(num, digits = digits, format = "f", big.mark = big.mark,
                 drop0trailing = !force.digits, ...)
  ans <- paste0("$", prefix, num, "$", postfix)
  if(!math.mode) ans <- stringr::str_replace_all(ans, stringr::fixed("$"), "")
  ans
}

#' Spells the 'num' digit in english
#' if  num < 1 || num > 10 returns NULL
tex_spell_digit <- function(num){
  switch(as.character(num),
         '1' = 'one',
         '2' = 'two',
         '3' = 'three',
         '4' = 'four',
         '5' = 'five',
         '6' = 'six',
         '7' = 'seven',
         '8' = 'eight',
         '9' = 'nine')
}

#' Spells 'num' in english.
#' If capitalize = T the first letter of the string is capitalized.
#' Currently works for |num| < 100 iff num is an integer.
#' Other numbers throw an error
tex_spell_num <- function(num, capitalize = F, zero.str = 'none', ...){
  if(num != as.integer(num)) stop("Don't know how to spell floats")
  ans <- ""
  if(num < 0){
    ans <- "minus "
    num <- num*-1
  }
  if(num == 0){
    ans <- zero.str
  } else if(num < 10){
    ans <- paste0(ans, tex_spell_digit(num))
  } else if(num < 20){
    ans <-paste0(ans,
                switch(as.character(num),
                       '10' = 'ten',
                       '11' = 'eleven',
                       '12' = 'twelve',
                       '13' = 'thirteen',
                       '14' = 'fourteen',
                       '15' = 'fifteen',
                       '16' = 'sixteen',
                       '17' = 'seventeen',
                       '18' = 'eighteen',
                       '19' = 'nineteen')
    )
  } else if(num < 100){
    tens <- 10*floor(num/10)
    digits <- num - tens
    ans <- paste0(ans,
                 switch(as.character(tens),
                        '20' = 'twenty',
                        '30' = 'thirty',
                        '40' = 'fourty',
                        '50' = 'fifty',
                        '60' = 'sixty',
                        '70' = 'seventy',
                        '80' = 'eighty',
                        '90' = 'ninety',
                        '100' = 'one-hundred'))
    if(digits > 0)
      ans <- paste0(ans, '-', tex_spell_digit(digits))
  } else {
    stop("Don't know how to spell ", num)
  }
  if(capitalize){
    ch0 <- substr(ans, 1, 1)
    substr(ans, 1, 1) <- toupper(ch0)
  }
  ans
}

#' Wrapper for tex_num that generates the mean of a vector
#' By default the mean is preceded by "M=", which is the default APA style for means
tex_mean <- function(vec, prefix = "M=", ...){
  tex_num(mean(vec), prefix, ...)
}

#' Wrapper for tex_num that generates the median of a vector
#' By default the median is preceded by "\\mathit{Mdn}=", which is the default APA style for medians
tex_median <- function(vec, prefix = "\\mathit{Mdn}=", ...){
  tex_num(median(vec), prefix, ...)
}

#' Returns a dataframe in tex format
#' @param df data frame or matrix to convert to a tex table. Note that colnames need to be set
#' @param digits number of decimal places to print. Defaults to \link{\code{texu_digits}}
#' @param drops columns from df to exclude in the output. Defaults to none
#' @param hline vector of rows which will have a horizontal line on top. The default "1" places a line after the header.
#' @param col.names an optional vector containing the desired column names in the output.
#' @param math.cols which cols should be treated as math (i.e. enclosed in $.$)?
tex_df <- function(df, digits = texu_digits(), drops = c(), hline=1, col.names = NULL, math.cols = c()){
  stopifnot(require(magrittr))
  df <- df[, !colnames(df) %in% drops]
  for(cur.col in 1:ncol(df)){
    if(df[, cur.col] %>% is.numeric){
      df[, cur.col] <- df[, cur.col] %>% round(digits)
    }else{
      df[, cur.col] <- df[, cur.col] %>% as.character # To ensure we don't have any factors remaining
    }
  }
  for(mcol in math.cols){
    df[, mcol] <- tex_math(df[, mcol])
  }
  tbl.hdr <- df %>% ncol %>% rep("c", .) %>% paste0(collapse = "") %>%  # Generate ncol "c"
             paste0("\\begin{tabular}{", ., "}\n") # insert them within begin{tabular}{________}
  if(is.null(col.names)) col.names <- colnames(df)
  df <- rbind(col.names, df)
  df.str <- df %>% apply(1, paste, collapse = " & ") %>% drop # A vector of strings with the elements of each row separated by &
  df.str[1:(length(df.str)-1)] <- df.str[1:(length(df.str)-1)] %>% paste0("\\\\") # Append "\\" to each line except the last one
  for(i in 0:(length(hline)-1)) # Insert additional "\hline" lines
    df.str <- df.str %>% append("\\hline", after=(hline[i+1]+i))
  tbl.ftr <- "\\end{tabular}\n"
  paste(tbl.hdr,
        df.str %>% paste(collapse="\n"), # Convert the contents of the df to one long string, separating rows with newlines
        tbl.ftr, sep="\n")
}

#' Returns a latex table with one row per parameter in mcmcs and two columns
#' The first column contains the median of each parameter.
#' The second colum contains the .95 HDI
#' @see summary_mcmcs
tex_summary_mcmcs <- function(mcmcs, cred.mass=0.95, source.hdr = "Source", source.names = NULL,
                              estimate.hdr = "$\\mathit{Mdn}$", math.mode = T, digits=texu_digits(), hdi.hdr = "\\,\\mathrm{HDI}", ...){
  mcmc.sum <- summary_mcmcs(mcmcs, cred.mass) %>% round(digits)
  mcmc.names <- rownames(mcmc.sum)
  if(!is.null(source.names)) mcmc.names <- source.names
  mcmc.est <- sapply(mcmc.sum[, 1], tex_math)
  mcmc.hdi <- mcmc.sum[, c("hdi.lo", "hdi.hi")] %>%
    apply(1, paste, collapse=", ") %>% paste0("$[", ., "]$")
  mcmc.df <- cbind.data.frame(mcmc.names, mcmc.est, mcmc.hdi)
  colnames(mcmc.df) <- c(source.hdr, estimate.hdr,
    paste0("$", cred.mass*100, "\\%", hdi.hdr, "$"))
  ans <- tex_df(mcmc.df, ...)
  if(!math.mode) ans <- stringr::str_replace_all(ans, stringr::fixed("$"), "")
  ans
}

#' Specific method for hierarchical mcmc fits
#' Has three columns, the first one contains the specification of each model
#' @param ... additional parameters for tex_df
tex_mcmc_hfit <- function(mcmcs, cred.mass=0.95, source.hdr = "Estimand", source.names = NULL,
                              estimate.hdr = "$\\mathit{Mdn}$", math.mode = T, digits=texu_digits(),
                              mod.names = NULL, mod.hdr = "Model", hdi.hdr = "\\,\\mathrm{HDI}", ...){
  mcmc.sum <- summary_mcmcs(mcmcs, cred.mass) %>% round(digits)
  mcmc.names <- rownames(mcmc.sum)
  if(!is.null(source.names)) mcmc.names <- source.names
  mcmc.est <- sapply(mcmc.sum[, 1], tex_math)
  mcmc.hdi <- mcmc.sum[, c("hdi.lo", "hdi.hi")] %>%
    apply(1, paste, collapse=", ") %>% paste0("$[", ., "]$")
  mcmc.df <- cbind.data.frame(mcmc.names, mcmc.est, mcmc.hdi)
  colnames(mcmc.df) <- c(source.hdr, estimate.hdr,
    paste0("$", cred.mass*100, "\\%", hdi.hdr, "$"))
  if(!is.null(mod.names) & (nrow(mcmc.df) == length(mod.names))){
    mcmc.df <- cbind(mod.names, mcmc.df)
    colnames(mcmc.df)[1] <- mod.hdr
  } else {
    err_str <- "Model names column not added to the answer, "
    if(is.null(mod.names)) warning(err_str, "because mod.names is null")
    else if(nrow(mcmc.df) != length(mod.names)) warning(err_str, "because its length was ", length(mod.names), " but the output table has ", nrow(mcmc.df), " rows")
  }
  ans <- tex_df(mcmc.df, ...)
  if(!math.mode) ans <- stringr::str_replace_all(ans, stringr::fixed("$"), "")
  ans
}

#' Returns a latex table with one column per parameter in mcmcs and two rows
#' The first row contains the median of each parameter.
#' The second row contains the .95 HDI
#' @see summary_mcmcs
tex_wsummary_mcmcs <- function(mcmcs, cred.mass=0.95, source.hdr = "Source", source.names = NULL,
                              math.mode = T, digits=texu_digits(), ...){
  mcmc.sum <- summary_mcmcs(mcmcs, cred.mass) %>% round(digits)
  mcmc.names <- rownames(mcmc.sum)
  if(!is.null(source.names)) mcmc.names <- source.names
  mcmc.est <- sapply(mcmc.sum[, 1], tex_math)
  mcmc.hdi <- mcmc.sum[, c("hdi.lo", "hdi.hi")] %>% apply(1, paste, collapse=", ") %>% paste0("$[", ., "]$")
  mcmc.df <- rbind.data.frame(mcmc.est, mcmc.hdi)
  colnames(mcmc.df) <- mcmc.names
  ans <- tex_df(mcmc.df, ...)
  if(!math.mode) ans <- stringr::str_replace_all(ans, stringr::fixed("$"), "")
  ans
}

#' Wrapper for tex_wsummary_mcmcs and tex_summary_mcmcs
#' By default returns a wide table (tex_wsummary_mcms)
tex_mcmc_fit <- function(mcmc_fit, coef.names = NULL, wide = T, ...){
  if(wide) tex_wsummary_mcmcs(mcmc_fit, source.hdr = "Estimand", source.names = coef.names, ...)
  else tex_summary_mcmcs(mcmc_fit, source.hdr = "Estimand", source.names = coef.names, ...)
}

#' Same as tex_w_summary_mcmcs, but with estimates specified directly rather than being calculated from distribution medians
tex_est_mcmc_fit <- function(ests, mcmcs, cred.mass=0.95, source.hdr = "Source", coef.names = NULL,
                              math.mode = T, digits=texu_digits(), ...){
  source.names = coef.names
  mcmc.sum <- summary_mcmcs(mcmcs, cred.mass) %>% mutate(mu = ests) %>% round(digits)
  mcmc.names <- rownames(mcmc.sum)
  if(!is.null(source.names)) mcmc.names <- source.names
  mcmc.est <- sapply(mcmc.sum[, 1], tex_math)
  mcmc.hdi <- mcmc.sum[, c("hdi.lo", "hdi.hi")] %>% apply(1, paste, collapse=", ") %>% paste0("$[", ., "]$")
  mcmc.df <- rbind.data.frame(mcmc.est, mcmc.hdi)
  colnames(mcmc.df) <- mcmc.names
  ans <- tex_df(mcmc.df, ...)
  if(!math.mode) ans <- stringr::str_replace_all(ans, stringr::fixed("$"), "")
  ans
}

#' Same as tex_w_summary_mcmcs, but without credible estimates
tex_lm_fit <- function(fit, source.hdr = "Source", coef.names = NULL,
                       math.mode = T, digits=texu_digits(), ...){
  ests <- c(coef(fit), with(summary(fit), c(sigma, adj.r.squared)))
  source.names <- coef.names
  ests <- ests %>% as.list %>% data.frame
  ans <- tex_df(ests, col.names = coef.names, digits = digits, ...)
  if(!math.mode) ans <- stringr::str_replace_all(ans, stringr::fixed("$"), "")
  ans
}

#' Returns a vector with the estimates of the parameters (including sigma), and adj.R2 from an lm
extract_lm_ests <- function(fit) c(coef(fit), summary(fit)$sigma, summary(fit)$adj.r.squared)

#' A latex table for multiple slr fit with lm
#' @param source.names column names for the estimates of the models
#' @param mod.names model names column to be prepended to the table. If NULL, no column is added
#' @param ... additional parameters for tex_df
tex_slm_fits <- function(mod.list, source.names = c("$\\mathit{Intercept}$", "$\\mathit{Slope}$", "$\\hat{\\sigma}$", "$\\hat{R}^2$"),
                         mod.names = NULL, ...){
  ans <- do.call(rbind.data.frame, lapply(mod.list, extract_lm_ests))
  if(!is.null(mod.names)){
    ans <- cbind(mod.names, ans)
    source.names <- c("Model", source.names)
  }
  tex_df(ans, col.names=source.names, ...)
}

#' Returns the summary of an mcmc in tex format
#'
#' The summary is printed in the following way "mu units, cred.mass*100\% HDI[hdi.lo, hdi.hi]"
#' optionally encloses the numeric output within "$",e.g. "$mu$ units, $cred.mass*100\%\,\mathrm{HDI}~[hdi.lo, hdi.hi]$", if math.mode == T
#' The summary is calculated using \link\code{summary_mcmc}
#'
#' @param mcmc a numeric vector containing the mcmc draws
#' @param cred.mass a 0..1 number specifying the mass within the HDI
#' @param est.str Character string to prepend to the point estimate. For example "Mdn=". By default it's ""
#' @param units Character string to append to the point estimate. For example "cm". By default it's ""
#' @param math.mode if T, enclose output within "$"
#' @digits digits number of decimal places to print
#' @hdi.label text between cred.mass and the HDI range. By default it's "\\,\\mathrm{HDI}~"
#'  It might be useful to change this, e.g., to "~HDI~" with math.mode = F
#'  For outline equations it is also recommended to
#'  replace ", 95\\%" by "\\text{, }95\\%, for example using gsub
#'
#' @examples
#' tex_summary_mcmc(rnorm(1000))
#' # [1] "$0.09$, $95\\%$ HDI $[-1.83, 2.09]$"
#' # Prepend "Mdn" to estimate
#' tex_summary_mcmc(rnorm(1000), est.str="\\mathit{Mdn}=", digits=3)
#' # [1] "$\\mathit{Mdn}=-0.033$, $95\\%$ HDI $[-1.961, 1.8]$"
#' # The same without math mode
#' tex_summary_mcmc(rnorm(1000), est.str="\\mathit{Mdn}=", digits=3, math.mode = F)
#' # [1] "\\mathit{Mdn}=0.03, 95\\% HDI [-1.869, 1.983]"
tex_summary_mcmc <- function(mcmc, cred.mass=0.95, est.str = "", units = "", math.mode = T, digits=texu_digits(),
                             hdi.label = "\\,\\mathrm{HDI}~"){
  mcmc.sum <- lapply(summary_mcmc(mcmc), round, digits)
  ans <- with(mcmc.sum, paste0("$", est.str, mu, "$", units,
                               ", $", cred.mass*100, "\\%",
                               hdi.label, "[", hdi.lo, ", ", hdi.hi, "]$"))
  if(!math.mode) ans <- stringr::str_replace_all(ans, stringr::fixed("$"), "")
  ans
}

#' Gets the default digits for tex_util methods
#'
#' By default digits = 2
#' To change the default digits call \code\{link{set_texu_digits}}
texu_digits <- function(){
  if(!exists("tex_util.__digits")){
    set_texu_digits()
  }
  tex_util.__digits
}

#' Sets the default digits for tex_util methods
#'
#' By default digits = 2
set_texu_digits <- function(digits=2){
  tex_util.__digits <<- digits
}

#' Gets the default big.mark for tex_util methods
#'
#' By default big.mark = {,} for math.mode and , otherwise
#' To change the default digits call \code\{link{set_texu_digits}}
texu_big.mark <- function(math.mode = F, tex_mode = ifelse(math.mode, "math", "text")){
  if(!exists("tex_util.__big.mark")){
    set_texu_big.mark()
  }
  tex_util.__big.mark[[tex_mode]]
}

#' sets the default digits for tex_util methods
#'
#' By default big.mark = c(math="{,}", text=",")
set_texu_big.mark <- function(big.mark = c(text=",", math="{,}")){
  if(length(big.mark) == 1) big.mark <- c(math=big.mark[[1]], text=big.mark[[1]])
  else if(length(big.mark) != 2) stop("can only specify 1 or 2 values for big.mark")

  if(is.null(names(big.mark)) || !(
       all(names(big.mark) %in% c("text", "math")) &&(
         length(unique(names(big.mark)))==2) #/&&
     )#/|| !
  ){
    names(big.mark) <- c("text", "math")
    warning('"', big.mark[1], '" will be set to text mode, and "', big.mark[2], '" will be set to math mode')
  }
  tex_util.__big.mark <<- big.mark
}
