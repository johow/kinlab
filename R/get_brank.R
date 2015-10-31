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
get_brank <- function(x=NULL,
                      df_ind = kh.full::kh_ind[kh.full::kh_ind$momid>0 & !is.na(kh.full::kh_ind$bdate),]){
df_ind <- df_ind[order(df_ind$momid, df_ind$bdate),]
 return(as.integer(lapply(mapply("==", 
tapply(df_ind$bdate, df_ind$momid, unique)[paste(get_moms(x, df_ind))],
         df_ind$bdate[df_ind$id%in% x]),  which)[order(x)]))
}