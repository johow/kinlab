#' Spatial distance between individuals at a specific date
#'
#' This function returns the spatial distance in kilometres between individual x and pedigree members at a specific date
#' @param id ID of individual for which a pedigree should be composed.
#' @param id2 ID of individual for which distance should be computed
#' @param evdat A specific date in the format ``Year-month-day''
#' @param evmat Event matrix
#' @param dist_mat A distance matrix
#' @keywords spatial distance pedigree
#' @export
#' @examples
#' \dontrun{
#' attach(kh33::kh)
#' get_dist(40, grap_ped(40, df_ped), "1850-01-01", dist_mat = geo[["map_dist"]], evmat=evmat)
#' detach(kh33::kh)
#' }

get_dist <- function(id, id2, evdat, evmat, map_dist){
  return(map_dist[get_kinloc(id, evdat, evmat), get_kinloc(id2, evdat, evmat)])
}
