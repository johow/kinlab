#' Fraternal IDs for a given individual
#'
#' Returns the IDs of (maternal) brothers - if any exists - for a given individual.
#' @param x ID of interest
#' @param df_ped A dataframe
#' @keywords kh ID brothers
#' @export
#' @examples
#' \dontrun{
#' get_brothers(1067, df_ped)
#' get_brothers(get_brothers(1067, df_ped)[1], df_ped)
#' }
get_brothers <- function(x, df_ped){
  if(get_moms(x, df_ped)!=0){
  return(unique(df_ped[df_ped$id != x & df_ped$momid == df_ped$momid[df_ped$id == x] & df_ped$sex==1, "id"]))
}
}
