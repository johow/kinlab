#' Add iteratively IDs of mothers
#'
#' This function returns a vector of IDs for a given individual matrilineal ancestry.
#' @param x ID of individual for which a pedigree should be composed.
#' @param df_ind A dataframe for genealogical relations providing the variables 'id', 'momid' and 'dadid'
#' @keywords pedigree
#' @export
#' @examples
#' \dontrun{
#' grap_kin(1067, df_ped)
#' }


add_moms <- function(x, df_ind){
  tmp <- unique(c(x, get_moms(x, df_ind)))
  tmp <- na.omit(tmp)[na.omit(tmp)>0]
  while(length(tmp)<length(na.omit(unique(c(tmp, get_moms(tmp, df_ind))))[na.omit(unique(c(tmp, get_moms(tmp, df_ind))))>0])){
    tmp <- unique(c(tmp, get_moms(tmp, df_ind)))
    tmp <- na.omit(tmp)[na.omit(tmp)>0]
  }
  return(tmp)
}
