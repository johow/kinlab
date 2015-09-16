#' Valid dates
#'
#' For a given ID, this function returns a vector of all event dates for which 'status' given in `evmat`is 1.
#' @param id An ID included in 'dimnames(evmat)'
#' @param evmat An event array (see '?get_evmat')
#' @param names Should output include the names of events? Default is false
#' @keywords spatial distance pedigree
#' @export
#' @examples
#' \dontrun{
#' valid_dates(1570, kh_mat)
#' }
valid_dates <- function(id=NULL, evmat=NULL, names = FALSE){
  if (names==FALSE){outdates =
    as.numeric(evmat[paste(id),,1][which(evmat[paste(id),,5]==1)])} else{
  outdates = evmat[paste(id),,1][which(evmat[paste(id),,5]==1)]}
  return(as.Date(outdates, origin="1970-01-01"))
}
