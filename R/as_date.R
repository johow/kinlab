#' Numeric Days from 1970 as Date
#'
#' This function returns a Date for a numeric value for the number of days since 1970-01-01.
#' @param evdat A numerical to be converted to date
#' @keywords numeric date
#' @export
#' @examples
#' as_date(0)
as_date <- function(evdat){
  return(as.Date(evdat, origin="1970-01-01"))
}
