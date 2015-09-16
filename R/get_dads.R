#' Paternal ID for a given individual
#'
#' This function returns the ID of an individual's father.
#' @param x An individual ID
#' @param df_ped A dataframe
#' @keywords kh father
#' @export
#' @examples
#' \dontrun{
#' get_dads(df_ped$id[2], df_ped)
#' }
get_dads <- function(x, df_ped){
  return(df_ped$dadid[df_ped$id %in% x])
}
