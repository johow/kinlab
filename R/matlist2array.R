#' Turns a list of sparse matrices into an array (or vice versa)
#'
#' This function returns an array of a given list of sparse matrices (or vice versa a list of sparse matrices for a given 3-dimensional array).
#' @param object Either a list of sparse matrices or an array with three dimensions
#' @keywords kh spouse
#' @export
#' @examples
#' \dontrun{
#' matlist2array(1000)
#' }
matlist2array <- function(object){
  if (is.list(object)){
    outmat <- array(NA, dim=c(length(object), dim(object[[1]])[[1]], dim(object[[1]])[[2]]),
          dimnames=list(id = names(object), tt = dimnames(object[[1]])$tt, vv = dimnames(object[[1]])$vv))
  for (i in 1:length(object)){
    outmat[i,,] <- as.matrix(object[[i]])
  }} else {
    outmat <- list("")
  for (i in 1:dim(object)[[1]]){
    outmat[[i]] <- Matrix::Matrix(object[i,,], dimnames = list(tt = dimnames(object)$tt,
                                                              vv = dimnames(object)$vv), sparse = TRUE)
  }
    names(outmat) <- dimnames(object)$id
  }
   return(outmat)
}
