#' IDs of an individual's founded family's members
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
get_foundfam_id <- function(x, df_ped){
  return(unique(c(x, get_spouses(x, df_ped), df_ped[df_ped$dadid %in%  c(x, get_spouses(x, df_ped)) | df_ped$dadid %in%  c(x, get_spouses(x, df_ped)), "id"])))
}
