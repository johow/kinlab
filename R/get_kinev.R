#' Pedigree members with documented event data
#'
#' This function returns the IDs for those pedigree members of a given individual for which event data is available.
#'
#' @param x An individual ID
#' @param df_ped A dataframe
#' @param evmat A matrix providing event data
#' @param ped A pedigree object
#' @keywords kh pedigree events
#' @export
#' @examples
#' \dontrun{
#' ##
#' }
get_kinev <- function(x, df_ped, evmat, ped=NULL){
  if (is.null(ped)){return(grap_ped(x, df_ped)$id[which(grap_ped(x, df_ped)$id!=x & grap_ped(x, df_ped)$id %in% dimnames(evmat)$id)])}
  else return(ped$id[which(ped$id!=x & ped$id %in% dimnames(evmat)$id)])
}

