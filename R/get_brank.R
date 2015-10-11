#' Individual birth rank
#'
#' Returns an individuals birth rank (in case of twin births, it is not unique!).  
#' @param x ID of interest
#' @param df_ind The individual data frame
#' @keywords birth rank
#' @export
#' @examples
#' \dontrun{
#' get_brank(12)
#' }
get_brank <- function(x, df_ind = kh.full::kh_ind){
  df_ind <- df_ind[df_ind$momid == get_moms(x, df_ind) & !is.na(df_ind$bdate) & df_ind$momid > 0,]
  if(x %in% df_ind$id){
 y<-which(unique(df_ind$bdate == df_ind$bdate[df_ind$id == x]))
} else y = NA
return(y)
    }