#' Add IDs of family members
#'
#' This function returns a vector of family members IDs for a given individual by iteratively adding IDs of offspring and parents and vice versa for a given times.
#' @param x ID of individual for which a pedigree should be composed.
#' @param df_ped A dataframe for genealogical relations providing the variables 'id', 'momid' and 'dadid'
#' @keywords pedigree
#' @export
#' @examples
#' \dontrun{
#' grap_kin(1067, df_ped)
#' }

kin_crawler <- function(x, df_ped){
x <- unique(unlist(lapply(x, get_natfam_id, df_ped)))
x <- unique(c(x, get_kids(x, df_ped)))
x <- unique(unlist(lapply(x, get_natfam_id, df_ped)))
x <- x[x>0]
return(x)
}
