#' Sample a Quebec mother
#'
#' This function returns random draws of mother IDs from a selection of families being classified as 'fully documented'.
#' @param df_ind A dataframe for individual data
#' @param n Number of draws to perform. For n equal or larger the number of mothers in initial sample, the vector of all mother IDs is returned instead. Default is 1.
#' @param sub_sample A vector of IDs, where IDs to return must be included.
#' @keywords pedigree
#' @export
#' @examples
#' df_ind <- get_exmpl_df()
#' df_ind$bdate <- sample(seq(as.Date("1774-12-31"), as.Date("1874-12-31"), 100), nrow(df_ind))
#' df_fam <- data.frame(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
#' sample_kh(df_ind, df_fam)
sample_qb <- function(df_ind,  n=1, sub_sample=NULL){
  qb_mom <- unique(df_ind$momid[
  df_ind$bdate >= as.Date("1680-01-01") &
    df_ind$bdate <  as.Date("1710-01-01") &
    df_ind$momid>0 & df_ind$momid %in% df_ind$id[df_ind$momid>0  &
                                                   !is.na(df_ind$bdate)] &
    df_ind$dadid %in% df_ind$id[df_ind$momid>0& !is.na(df_ind$bdate)]])
  if (n<length(qb_mom)){qb_mom <- sample(qb_mom, n)}
  if (!is.null(sub_sample)){qb_mom <- qb_mom[qb_mom %in% sub_sample]}
  return(qb_mom[order(qb_mom)])
}



