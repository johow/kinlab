#' Get family's grasen value for a given ID of a parent
#'
#' This function returns a family's value of 'grasen' (a measure of land size) for a given ID of a parent.
#' @param x ID of a parent
#' @param df_fam A dataframe containing ID and grasen values
#' @keywords pedigree
#' @export
#' @examples
#' \dontrun{
#' df_ind$bdate <- sample(seq(as.Date("1774-12-31"), as.Date("1874-12-31"), 100), nrow(df_ind))
#' df_fam <- data.frame(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
#' sample_kh(df_ind, df_fam)
#' }
get_grasen <- function(x=NULL, df_fam=NULL){
  return(ifelse(any(!is.na(df_fam$grasen[df_fam$idf == x | df_fam$idm == x])),
                max(df_fam$grasen[df_fam$idf == x], na.rm=TRUE),
                NA))
}
