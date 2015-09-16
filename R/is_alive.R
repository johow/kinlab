#' Test for estimated survival status of a given individual at a given date.
#'
#' Returns estimated survival status of a given individual at a given date. Estimates are based on recorded deaths and the assumption that individuals neither dying nor being mentioned elsewhere did survive at least until the age of 15 (and later possibly outmigrated).
#' @param x An individual's ID
#' @param evdat Date of interest for estimated survival status of individual x in the format "Year-month-day".
#' @param df_ind A dataframe containing birth and death dates of individuals in x
#' @keywords survival status
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }
is_alive <- function(x, evdat, df_ind){
  df_tmp <- merge(data.frame(id=x, ord=1:length(x)), df_ind[,c("id", "bdate", "ddate")], by = "id", all.x=TRUE)
  return(ifelse(!is.na(df_tmp$bdate) & df_tmp$bdate >as.Date(evdat), 0,
    ifelse(!is.na(df_tmp$ddate) & df_tmp$ddate <= as.Date(evdat), 0,
                ifelse(!is.na(df_tmp$ddate) & df_tmp$ddate > as.Date(evdat), 1,
                       ifelse(is.na(df_tmp$ddate) & !is.na(df_tmp$bdate) & df_tmp$bdate < as.Date(evdat) & df_tmp$bdate > (as.Date(evdat)-15*365.25), 1,
                              0)))[order(df_tmp$ord)]))
}
