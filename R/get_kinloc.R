#' Returns the estimated locality of a given individual at a fixed date
#'
#' This function returns the estimated locality of a given individual at a given date. Locality is estimated from the individual's last record before or at the date of interest.
#' @param id An individual ID
#' @param evdat Date of interest in the Year-month-day format (e.g. "1850-01-01")
#' @param evmat Event matrix
#' @keywords kh locality
#' @export
#' @examples
#' get_min_birthage(1067, kh33::kh$evmat_bak)

get_kinloc <- function(id, evdat, evmat){
 if(!is.numeric(evdat)){  evdat <- as.numeric(as.Date(paste(evdat)))}
return(ifelse(any(evmat[paste(id),,1]<=evdat),
              unique(as.numeric(evmat[paste(id),,3][which(evmat[paste(id),,1] %in% max(evmat[paste(id),,1][which(evmat[paste(id),,1]<=evdat)]))])), NA))
}
