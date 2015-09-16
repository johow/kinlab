#' IDs of an individual's natal family members
#'
#' For a given individual, this function returns IDs of the parents and their (shared) offspring being ever born.
#' @param x An individual ID
#' @param df_ped A dataframe for genealogical relations providing the variables 'id', 'momid' and 'dadid'
#' @keywords kh pedigree
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }
get_natfam_id <- function(x, df_ped){
  return( c(df_ped[df_ped$id%in%x, "momid"], df_ped[df_ped$id%in%x, "dadid"],
          df_ped[df_ped$dadid==df_ped[df_ped$id%in%x & df_ped$dadid>0, "dadid"] |
                      df_ped$momid==df_ped[df_ped$id%in%x & df_ped$momid>0, "momid"], "id"]))
}
