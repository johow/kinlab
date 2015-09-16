#' Return an individual's sisters being ever born
#'
#' This function returns any female indiviual being born from the same mother -- excluding the ID for a given (possibly female) individual.
#' @param x ID of individual of interest
#' @param df_ped A dataframe
#' @keywords pedigree sisters
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }
get_sisters <- function(x, df_ped){
  return(ifelse(any(df_ped$sex==2 & 
         df_ped$momid %in% df_ped$momid[df_ped$id == x] & 
         df_ped$momid > 0 & df_ped$id != x), c(
 df_ped$id[df_ped$sex==2 & 
                       df_ped$momid %in% df_ped$momid[df_ped$id == x] & 
                       df_ped$momid > 0 & df_ped$id != x]), NA))
}
