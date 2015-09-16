#' Returns a given individual's date of birth or death
#'
#' This function returns the birth date of a given individual, if known.
#' @param x Individual ID
#' @param df_ind A dataframe containing IDs and birth dates
#' @param var_id Variable name for ID (default is 'id')
#' @param var_date Variable name for birth date  (default is 'bdate')
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
get_date <- function(x=NULL, df_ind=NULL, df_fam=NULL, var_id="id", var_date=c("bdate", "ddate", "mdate")[1]){
return(    as.Date(ifelse(var_date=="mdate",
              ifelse(x %in%  df_fam$idf | x %in%  df_fam$idm,
                     ifelse(any(!is.na(df_fam[df_fam$idm%in%x | df_fam$idf%in%x, "dat4"])),
min(df_fam[df_fam$idm%in%x | df_fam$idf%in%x, "dat4"], na.rm=TRUE),
NA), NA),
df_ind[df_ind[,paste(var_id)]%in%x, paste(var_date)])[order(x)], origin="1970-01-01"))

}
