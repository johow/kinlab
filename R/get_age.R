#' Returns a given individual's age at a given date
#'
#' This function returns the age of a given individual (with known birth date) at a given  date.
#' @param x Individual ID
#' @param evdat Dates of interest in the Year-month-day format (e.g. "1850-01-01")
#' @param round.digits Number of digits for rounding (default is NA, i.e. no rounding)
#' @param evmat A matrix for event data (see code{link{get_evmat}})
#' @keywords kh spouse
#' @export
#' @examples
#' \dontrun{
#' df_ind <- get_exmpl_df()
#' df_ind$bdate <- sample(seq(as.Date("1774-12-31"), as.Date("1874-12-31"), 100), nrow(df_ind))
#' df_fam <- data.frame(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
#' evmat <- get_evmat(df_ind, df_fam)
#' my_id <- sample_kh(df_ind, df_fam)
#' get_age(my_id, paste(df_ind$bdate[df_ind$id == my_id]+21*365-25), evmat)
#'}
get_age <- function(x, evdat = NA, round.digits=NA, evmat){
  if(!is.na(round.digits)){
  return(round(as.numeric(ifelse(as.numeric(as.Date(evdat)) - evmat[paste(x),1,1]>=0, (as.numeric(as.Date(evdat)) - evmat[paste(x),1,1])/365.25, NA)), round.digits))
  } else return(as.numeric(ifelse(as.numeric(as.Date(evdat)) - evmat[paste(x),1,1]>=0, (as.numeric(as.Date(evdat)) - evmat[paste(x),1,1])/365.25, NA)))
}
