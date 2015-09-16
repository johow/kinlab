#' Get birth date of a mother's first born child
#'
#' This function returns the birth date of a mother's first born child
#' @param id Individual ID of parent(s)
#' @param kh_mat An event array (see  ?get_evmat )
#' @keywords pedigree
#' @export
#' @examples
#' \dontrun{
#' df_ind$bdate <- sample(seq(as.Date("1774-12-31"), as.Date("1874-12-31"), 100), nrow(df_ind))
#' df_fam <- data.frame(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
#' sample_kh(df_ind, df_fam)
#' }

first_bdate <-   function(id, kh_mat){
  stopifnot(id %in% dimnames(kh_mat)$id)
  return(as.Date(ifelse(kh_mat[paste(id),2,5]==0, NA,
                kh_mat[paste(id),2,1]), origin="1970-01-01"))
}

