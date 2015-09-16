#' Replace NAs with zeros
#'
#' This function replaces NAs with zeros.
#' @param x A R object containing zero to be replaced by NAs.
#' @param zero Replacement for missing values in x (Default is 0)
#' @keywords NA
#' @export
#' @examples
#' na2zero(c(1,2,NA,3,5))
na2zero <- function(x, zero = 0){
  x[is.na(x)] <- ifelse(is.numeric(zero), zero, paste(zero))
  return(x)
}

#' Replace zeros with NAs
#'
#' This function replaces zeros with NAs.
#' @param x A R object containing zero to be replaced by NAs.
#' @param zero Value to be replaced (Default is 0)
#' @param na Replacement for value given in zero (Default is NA)
#' @keywords NA
#' @export
#' @examples
#' zero2na(c(1,2,0,3,5))
zero2na <- function(x, zero = 0, na = NA){
  x[x== ifelse(is.numeric(zero), zero, paste(zero))] <-  ifelse(is.numeric(na) | is.na(na), na, paste(na))
  return(x)
}


#' Remove white space from strings
#'
#' This function removes blanks before and after a string expression (found via \href{http://stackoverflow.com/a/2261149}{SO, credit goes to f3lix}).
#' @param x A string
#' @keywords remove blanks string
#' @export
#' @examples
#' trim("   Yo, wuzzup?     ")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
