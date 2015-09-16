#' Add ID to maps
#'
#' Returns a list of map objects, showing estimated locations for all living pedigree members for a given individual
#' \code{ggmap} provided plot of a map showing the estimated locations of living pedigree members for a given ID at a given date. The size, shape and color of symbols represent for a specific individual's age, sex, and relatedness to an indiviudal x.
#' @param id_list List of individual IDs of interests
#' @param evdaz  Date for estimatiom
#' @param kh_mat The event array
#' @param list_geo A list of geo stuff.. with geopositional data for locations
#' @param kh_ind Individual data 
#' @keywords kh pedigree
#' @export
#' @examples
#' \dontrun{
#' plot_kinmap(40, "1860-01-01", df_ind, relate, evmat, geo$gmap, geo$df)
#' }

add_kinstat<- function(id_list, evdat, kh_mat, kh_geo, kh_ind){
add_id_2map <- function(id, evdat, kh_mat, kh_geo){
  return(list("lon" = kh_geo[["df"]][kh_geo[["df"]]$label %in% recode_evloc(get_kinloc(id, evdat, kh_mat)),"lon"],
              "lat" = kh_geo[["df"]][kh_geo[["df"]]$label %in% recode_evloc(get_kinloc(id, evdat, kh_mat)),"lat"]))
}
  
stopifnot(any(is_alive(id_list, evdat, kh_ind)==1))

outdf <- data.frame(id = id_list, 
                    lon = NA, 
                    lat = NA)

outdf <- outdf[outdf$id %in% outdf$id[is_alive(outdf$id, evdat, kh_ind)==1],]
for (i in 1:nrow(outdf)){
  outdf[i,2:3] <- unlist(add_id_2map( outdf[i,1], evdat, kh_mat, kh_geo))
}
return(outdf[outdf$lon!=0,]
)
}