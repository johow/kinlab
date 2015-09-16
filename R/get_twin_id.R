#' Returns the IDs of twins of a twin mother
#'
#' This function returns the IDs of twins of a twin mother
#' @param twinmother.id Dates of interest in the Year-month-day format (e.g. "1850-01-01")
#' @param evmat_bak An event matrix
#' @param relate A twin relation matrix
#' @keywords kh spouse
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }


get_twin_id <- function(twinmother.id=NULL, evmat_bak=NULL, relate = NULL){
  evmat_bak[,,4] <- ifelse(evmat_bak[,,2] %in% relate[,1:2], 6,   evmat_bak[,,4])
    evmat_bak[,,2]
  return(as.numeric(evmat_bak[paste(twinmother.id),which(evmat_bak[paste(twinmother.id),,4]==6),2]))
}
