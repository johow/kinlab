#' Returns the ID of an individual's spouse at a fixed date
#'
#' This function returns the ID of an individual's spouse at a fixed date (accounting for multiple marriages, if possible).
#' @param x An individual ID
#' @param evdat Dates of interest in the Year-month-day format (e.g. "1850-01-01")
#' @param df_ped A dataframe
#' @param df_fam A dataframe containing variables for date of union (\code{dat4}) and the IDs of the spouses (female: \code{idf}; male: code{idm})
#' @keywords kh spouse
#' @export
#' @examples
#' df_ind <- get_exmpl_df()
#' df_ind$bdate <- sample(seq(as.Date("1774-12-31"), as.Date("1874-12-31"), 100), nrow(df_ind))
#' df_fam <- data.frame(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
#' actual_spouse(sample_kh(df_ind, df_fam),
#'    paste(sample(df_ind$bdate, 1)),
#'    df_ind, df_fam)
actual_spouse <- function(x=NULL, evdat=NULL, df_ped=NULL, df_fam=NULL){return(
  ifelse(length(get_spouses(x, df_ped))==1, get_spouses(x, df_ped),
  ifelse(
  is.na(df_fam$idm[which(
    df_fam$idf == x &
      df_fam$dat4 == max(df_fam$dat4[df_fam$idf==x & df_fam$dat4 <= evdat], na.rm=TRUE))]),
  df_fam$idm[df_fam$idf==x][which(is_alive(
    df_fam$idm[df_fam$idf==x], paste(evdat))==1)],
  df_fam$idm[which(
    df_fam$idf == x &
    df_fam$dat4 == max(df_fam$dat4[df_fam$idf==x & df_fam$dat4 <= evdat], na.rm=TRUE))])))
}
