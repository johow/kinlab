#' Children ever born
#'
#' This function returns the number of children ever born for a given set of individuals and a given event matrix.
#' @param id_sample A sample of IDs (will be sample from, if length(id_sample)> max_n)
#' @param evmat An event matrix
#' @keywords children ever born pedigree
#' @export
#' @examples
#' \dontrun{
#' df_ind <- get_exmpl_df()
#' df_fam <- data.frame(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
#' evmat <- get_evmat(df_ind, df_fam)
#' ms1 <- built_ms(evmat)
#' }
#'
get_ceb <- function(id, evmat){
  return(c(as.numeric(unlist(apply(evmat[paste(id), 2:19, 5], 1, sum) +
 as.numeric(unlist(apply(apply(apply(evmat[paste(id), 2:19, 4], 1, "==", 6), 1, as.numeric), 1, sum)))))))
}
