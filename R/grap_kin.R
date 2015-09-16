#' Add iteratively IDs of related family members
#'
#' This function returns a vector of family members IDs for a given individual by iteratively adding IDs of offspring and parents and vice versa for a given times.
#' @param x ID of individual for which a pedigree should be composed.
#' @param df_ind A dataframe for genealogical relations providing the variables 'id', 'momid' and 'dadid'
#' @param max_size Maximum size of pedigree of interest (Default is 300).
#' @param max_cycles Maximum number of iterations (adding further relatives).
#' @keywords pedigree
#' @export
#' @examples
#' \dontrun{
#' grap_kin(1067, df_ped)
#' }

grap_kin <- function(x, df_ind, max_size, max_cycles = 8){
  count_cycles <- 1
  while (length(kin) <= max_size & count_cycles<=max_cycles){
    kin <- kin_crawler(kin, df_ind)
    count_cycles <- count_cycles + 1
  }
  return(unique(kin[order(kin)]))
}
