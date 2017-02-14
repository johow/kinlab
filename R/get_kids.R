#' Returns the ID(s) of an individual's child(ren)
#'
#' This function returns the ID(s) of an individual's children
#' @param x An individual ID
#' @param df_ped A dataframe
#' @param include_all Should offspring in subsequent generations (grandchildren etc.) be included?
#' @keywords kh children
#' @export
#' @examples
#' \dontrun{
#' get_kids(df_ped$id[2], df_ped)
#' detach(kh33::kh)
#' }
get_kids <- function(x, df_ped, include_all = FALSE){
  x <- x[which(x > 0 & ( x %in% df_ped$momid | x %in% df_ped$dadid))]
  if (include_all==TRUE){
   
    out_x <- unique(c(df_ped$id[df_ped$momid %in% x | df_ped$dadid %in% x], 
                      df_ped$id[df_ped$momid %in% df_ped$id[df_ped$momid %in% x | df_ped$dadid %in% x] |
                                df_ped$dadid %in% df_ped$id[df_ped$momid %in% x | df_ped$dadid %in% x]],
                      df_ped$id[df_ped$momid %in% df_ped$id[df_ped$momid %in% df_ped$id[df_ped$momid %in% x | df_ped$dadid %in% x] |
                                                              df_ped$dadid %in% df_ped$id[df_ped$momid %in% x | df_ped$dadid %in% x]] |
                                  df_ped$dadid %in% df_ped$id[df_ped$momid %in% df_ped$id[df_ped$momid %in% x | df_ped$dadid %in% x] |
                                                                df_ped$dadid %in% df_ped$id[df_ped$momid %in% x | df_ped$dadid %in% x]]]))
  } else {
  out_x <- unique(df_ped$id[df_ped$momid %in% x | df_ped$dadid %in% x])
  }
    return(out_x)
}
