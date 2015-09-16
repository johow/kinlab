#' Returns a mother's minimum age at birth, considering only twin births
#'
#' This function returns a  mother's minimum age at birth, considering only twin births
#' @param twinmother.id Dates of interest in the Year-month-day format (e.g. "1850-01-01")
#' @param evmat_bak An event matrix
#' @keywords kh spouse
#' @export
#' @examples
#' \dontrun{
#' get_min_birthage(1067, kh33::kh$evmat_bak)
#' }
get_min_twinage <- function(twinmother.id=NULL, evmat_bak = NULL){
.matAGE <- evmat_bak
.matAGE[,,1] <- (.matAGE[,,1]- .matAGE[,1,1])/365.25
return(min(.matAGE[paste(twinmother.id),as.numeric(which(.matAGE[paste(twinmother.id),,4]==6)),1]))
}
