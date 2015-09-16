#' Returns the relatedness coefficient between two individuals of a given pedigree
#'
#' This function returns the relatedness coefficient between two individuals of a given pedigree.
#' @param ped Pedigree object for individuals 1 and 2
#' @param x ID of individual 1
#' @param y ID of individual 2
#' @keywords kh pedigree relatedness
#' @export
#' @examples
#' exmpl_df <- get_exmpl_df()
#' ped <- with(exmpl_df, kinship2::pedigree(id, dadid, momid, sex, affected, status))
#' r_val(ped, exmpl_df$id[1], exmpl_df$id[2])
r_val <- function(ped=NULL,x=NULL,y=NULL){
  if (is.null(ped)){
    ped <- grap_ped(x)
  }
  return(as.numeric(kinship2::kinship(ped)[which(ped$id==x),which(ped$id %in% y)]))
}

r_valx <- function(ped=NULL,x=NULL,y=NULL){
  if (is.null(ped)){
    ped <- grap_ped(x)
  }
  return(as.numeric(kinship2::kinship(ped, chrtype = "X")[which(ped$id==x),which(ped$id %in% y[order(y)])]))
}
