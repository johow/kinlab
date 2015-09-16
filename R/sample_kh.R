#' Sample a Krummh\enc{ö}{oe}rn mother
#'
#' This function returns random draws of mother IDs from a selection of families being classified as 'fully documented'.
#' @param df_ind A dataframe
#' @param df_fam A dataframe containing
#' @param n Number of draws to perform. For n equal or larger the number of mothers in initial sample, the vector of all mother IDs is returned instead. Default is 1.
#' @keywords pedigree
#' @export
#' @examples
#' df_ind <- get_exmpl_df()
#' df_ind$bdate <- sample(seq(as.Date("1774-12-31"), as.Date("1874-12-31"), 100), nrow(df_ind))
#' df_fam <- data.frame(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
#' sample_kh(df_ind, df_fam)
sample_kh <- function(df_ind, df_fam, n=1){
  stopifnot("idf" %in% names(df_fam))
  kh_mom <- unique(df_ind$momid[which(df_ind$momid %in% df_ind$id[df_ind$bdate > as.Date("1720-01-01") &
                                                                    df_ind$bdate < as.Date("1874-12-31")] &
                                        df_ind$momid %in% df_ind$id[df_ind$momid %in% df_fam$idf[df_fam$fall %in% c("C","P")]] &
                                        df_ind$dadid %in% df_ind$id[df_ind$momid %in% df_fam$idf[df_fam$fall %in% c("C","P")]] &
                                        df_ind$momid %in% df_fam$idf[df_fam$fall %in% c("C", "P")] &
                                        df_ind$momid %in% df_fam$idf[(df_fam$todf4 >=  df_fam$todm8 & !is.na(df_fam$todm4) &
                                                                        df_fam$todm4==df_fam$todm8 & !is.na(df_fam$todm8) &
                                                                        !is.na(df_fam$todf4)) |
                                                                       (df_fam$todm4 >=  df_fam$todf8 & !is.na(df_fam$todf4) &
                                                                          df_fam$todf4==df_fam$todf8 & !is.na(df_fam$todf8) &
                                                                          !is.na(df_fam$todm4))])])
  if (n<length(kh_mom)){kh_mom <- sample(kh_mom, n)}
  return(kh_mom[order(kh_mom)])
}
