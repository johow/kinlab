#' Test for relatedness
#'
#' This function tests for relatedness by computing the relatedness coefficient between two given individuals (id1 and id2) in a given pedigree (ped). It returns either 1 for 'related' (i.e. r > 0) or 0 for 'unrelated' (r = 0).
#' @param id1 ID of first individual
#' @param id2 ID of second individual
#' @param ped A pedigree object containing both individuals
#' @param df_ped A dataframe
#' @param relate A realtionship matrix for twin zygosity
#' @keywords pedigree relatedness
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }
is_related <- function(id1, id2, ped=NA, df_ped, relate){
  if (all(is.na(ped))){
    ped <- grap_ped(id1, df_ped, relate)
  }
 ifelse(kinship2::kinship(ped)[paste(id1), paste(id2)]> 0, 1, 0)
}
