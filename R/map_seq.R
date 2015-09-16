#' Map sequence
#'
#' Returns a list of map objects, showing estimated locations for all living pedigree members for a given individual
#' \code{ggmap} provided plot of a map showing the estimated locations of living pedigree members for a given ID at a given date. The size, shape and color of symbols represent for a specific individual's age, sex, and relatedness to an indiviudal x.
#' @param my_id The individual ID of interests
#' @param kin_list A nested list of dataframes providing the variables 'id', 'sex', 'relmom', 'reldad', 'age', and 'kinloc'
#' @param my_map A ggmap object
#' @param geo_df A dataframe with geopositional data for locations
#' @param return_gg A logical indicator whether the plots should be returned (if TRUE) or displayed (if  FALSE)
#' @param map_extent How much of the plot should the map take up? (default is "device")
#' @param scale_factor Ratio of scaling in relation to 1 (Default).
#' @keywords kh pedigree
#' @export
#' @examples
#' \dontrun{
#' plot_kinmap(40, "1860-01-01", df_ind, relate, evmat, geo$gmap, geo$df)
#' }

map_seq <- function(my_id=NULL, kin_list=NULL, my_map=NULL, geo_df=NULL,
                    return_gg=TRUE, map_extent="device",
                    scale_factor=1, limit_n =NULL){
  kin_list = kin_list[[paste(my_id)]]
  stopifnot(length(kin_list)>0)
map_list <- list("")

kin_stats <- list(subset(merge(kin_list[[1]],
      cbind(kinloc = recode_evloc(geo_df$label), lat = geo_df$lat, lon = geo_df$lon), by = "kinloc", all.x=TRUE), !is.na(dist) & !is.na(age)))
kin_stats[[1]]$rel_ratio <- ifelse( (kin_stats[[1]]$relmom - kin_stats[[1]]$reldad)>0, (kin_stats[[1]]$relmom - kin_stats[[1]]$reldad)^-3, 0)
kin_stats[[1]]$age <- ceiling(kin_stats[[1]]$age)

 if(is.null(limit_n)) limit_n <- 2:length(kin_stats)
for (i in limit_n){
  kin_stats[[i]] <-  subset(merge(kin_list[[i]],
                                       cbind(kinloc = recode_evloc(geo_df$label),
                                             lat = geo_df$lat, lon = geo_df$lon), by = "kinloc", all.x=TRUE), !is.na(dist) & !is.na(age))
  kin_stats[[i]]$rel_ratio <- ifelse((kin_stats[[i]]$relmom - kin_stats[[i]]$reldad)!=0,(kin_stats[[i]]$relmom - kin_stats[[i]]$reldad)^-3,0)
  kin_stats[[i]]$age <- ceiling(kin_stats[[i]]$age) * scale_factor
  }


  my_stats <- lapply(kin_stats, subset, id == my_id)[-length(kin_list)]

  kin_stats_full <- kin_stats
  kin_stats <- lapply(kin_stats, subset, !id %in% my_id)

  if(return_gg==TRUE){
  for (i in 1:length(my_stats)){

  map_list[[i]]  <- ggmap::ggmap(my_map, extent = map_extent) +
      ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, size = age , colour=
 rel_ratio, shape = sex),   position = "jitter",
                 data = kin_stats[[i]])+
    ggplot2::scale_shape_manual(values= c(16,15)) +
    ggplot2::scale_colour_gradient(low=scales::muted("red", c = 80, l = 50),
                                   high=scales::muted("green", c = 80, l = 50),
                                   na.value="black")  +
    ggplot2::geom_text(ggplot2::aes(x = lon, y = lat), label =  substr(names(kin_list),1,4)[[i]],
                       data = my_stats[[i]]) +
    ggplot2::ggtitle(paste("Kin network of ID", my_id, "on", kin_stats[[i]]$evdat[1]))
}
  map_list[[i+1]]  <- ggmap::ggmap(my_map, extent = map_extent) +
    ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, size = age , colour=
                                       rel_ratio, shape = sex),   position = "jitter",
                        data = kin_stats[[i+1]])+
    ggplot2::scale_shape_manual(values= c(16,15)) +
    ggplot2::scale_colour_gradient(low=scales::muted("red", c = 80, l = 50),
                                   high=scales::muted("green", c = 80, l = 50),
                                   na.value="black")  +
    ggplot2::geom_text(ggplot2::aes(x = lon, y = lat), label = substr(names(kin_list),1,4)[[i]],
                       data = my_stats[[i]]) +
    ggplot2::ggtitle(paste("Kin network of ID", my_id, "on", kin_stats[[i+1]]$evdat[1]))
  return(list("maps"=map_list, "kin_stats" = kin_stats_full))
  } else  {for (i in 1:length(my_stats)){print(ggmap::ggmap(my_map, extent = map_extent)) +
      ggplot2::geom_point(ggplot2::aes(x = lon, y = lat,
                                       colour=
                                         as.numeric(rel_ratio),
                                       shape = sex),
                          position = "jitter",
                          data = kin_stats[[i]]) +
      ggplot2::scale_shape_manual(values= c(16,15)) +
      ggplot2::scale_colour_gradient(low=scales::muted("red", c = 80, l = 50),
                                     high=scales::muted("green", c = 80, l = 50),
                                     na.value="black")  +
      ggplot2::geom_text(ggplot2::aes(x = lon, y = lat), label = substr(names(kin_list),1,4)[[i]],
                         data = my_stats[[i]]) +
      ggplot2::ggtitle(paste("Kin network of ID", my_id, "on", kin_stats[[i]]$evdat[1]))
  }}
  print(ggmap::ggmap(my_map, extent = map_extent)) +
    ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, size = age, colour=
                          relmom, shape = sex),   position = "jitter",
                        data = kin_stats[[i]])+
    ggplot2::scale_shape_manual(values= c(16,15)) +
    ggplot2::scale_colour_gradient(low=scales::muted("red", c = 80, l = 50),
                                   high=scales::muted("green", c = 80, l = 50),
                                   na.value="black")  +
    ggplot2::geom_text(ggplot2::aes(x = lon, y = lat), label = substr(names(kin_list),1,4)[[i]],
                       data = kin_stats[[i]]) +
    ggplot2::ggtitle(paste("Kin network of ID", my_id, "on", kin_stats[[i]]$evdat[1]))

}

