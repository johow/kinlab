#' Return the sex of a given ID
#'
#' This function returns the sex of a given individual.
#' @param x Ab individual ID
#' @param df_ped A dataframe
#' @keywords kh sex
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }
get_sex <- function(x,df_ped){
  return(df_ped$sex[df_ped$id %in% x])
}
