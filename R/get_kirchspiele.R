#' Recode locality labels in KH33 database (internal function)
#'
#' This function standardizes labels of localities in the KH33 database.
#' @param x A character vector containing locality labels which should be standardized.
#' @param valid_entries A character vector containing all valid standard locality labels.
#' @keywords labels KH33
#' @export
#' @examples
#' get_kirchspiele("CA")
#' get_kirchspiele("WY")
get_kirchspiele <- function(x, valid_entries = c("CA", "CH", "CI", "CN", "EI", "FR", "GH",
                                                 "GI", "GM", "GS", "HI", "HW", "JE", "LA", "LO", "LP", "LV", "MA", "MW", "NE",
                                                 "PE", "PI", "RY", "SU", "TW", "UP", "UT", "VI", "WE", "WI", "WO", "WY", "WZ")){
    x <- trim(x)
    return(ifelse(nchar(x)<2, "x.unknown",    ifelse(substr(x, 1, 2) %in% valid_entries & !(substr(x, 3, 3) %in% LETTERS),
           substr(x, 1, 2),  "x.out")))
  }
