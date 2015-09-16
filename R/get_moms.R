#' Returns the ID of an individual's mother
#'
#' This function returns the ID of an individual's mother
#' @param x An individual ID
#' @param df_ped A dataframe
#' @keywords kh mother
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }
get_moms <- function(x, df_ped){
  return(df_ped$momid[df_ped$id %in% x])
}
