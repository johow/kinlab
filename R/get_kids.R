#' Returns the ID(s) of an individual's child(ren)
#'
#' This function returns the ID(s) of an individual's children
#' @param x An individual ID
#' @param df_ped A dataframe
#' @keywords kh children
#' @export
#' @examples
#' \dontrun{
#' get_kids(df_ped$id[2], df_ped)
#' detach(kh33::kh)
#' }
get_kids <- function(x, df_ped){
  x <- x[which(x > 0 & ( x %in% df_ped$momid | x %in% df_ped$dadid))]
    return(unique(df_ped$id[df_ped$momid %in% x | df_ped$dadid %in% x]))
}
