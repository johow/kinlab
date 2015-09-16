#' Returns the minimum age at birth
#' This function returns a mother's minimum age at birth
#' @param mother.id An individual mother ID
#' @param evmat_bak An event matrix
#' @keywords kh spouse
#' @export
#' @examples
#' \dontrun{
#' get_min_birthage(1067, kh33::kh$evmat_bak)
#' }
get_min_birthage <- function(mother.id=NULL, evmat_bak = NULL){
  .matAGE <- evmat_bak
  .matAGE[,,1] <- (.matAGE[,,1]- .matAGE[,1,1])/365.25
  return(min(.matAGE[paste(mother.id),2,1]))
}
