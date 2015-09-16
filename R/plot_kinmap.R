#' Plot a pedigree map
#'
#' \code{ggmap} provided plot of a map showing the estimated locations of living pedigree members for a given ID at a given date. The size, shape and color of symbols represent for a specific individual's age, sex, and relatedness to an indiviudal x.
#' @param id The individual ID of interests
#' @param evdat A specific date of interest
#' @param list_kin A nested list of dataframes providing the variables 'id', 'sex', 'relmom', 'reldad', 'age', and 'kinloc'
#' @param list_geo A list of geo stuff.. with geopositional data for locations
#' @param my_map A ggmap object
#' @param spit_results Spit it? (default is FALSE)
#' @param throw_plots Print'em? (default is TRUE)
#' @keywords kh pedigree
#' @export
#' @examples
#' \dontrun{
#' plot_kinmap(40, "1860-01-01", df_ind, relate, evmat, geo$gmap, geo$df)
#' }

plot_kinmap <- function(id=NULL,
                      evdat=NULL,
                      list_kin=NULL,
                      list_geo=NULL,
                      my_map=NULL,
                      spit_results=FALSE,
                      throw_plots=TRUE){
  outdf <-unlist_df(lapply(list_kin[[paste(id)]], merge,
                           cbind(kinloc = recode_evloc(list_geo[["df"]]$label), list_geo[["df"]][,!colnames(list_geo[["df"]]) %in% "label"]),
                           by = "kinloc"))
  outdf$relatedness <- na2zero(outdf$relmom)-na2zero(outdf$reldad)
  outdf$sex <- factor(outdf$sex)
  outlist  <- list("")
  dates <- paste(evdat)
  suppressWarnings(
  for (i in 1:length(dates)){
    outlist[[i]] <-  ggmap::ggmap(my_map) +
      ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, size = age,
                                       colour=relatedness, shape = sex),   position = "jitter",
                          data = outdf[outdf$dflist%in%dates,]) +
      ggplot2::scale_shape_manual(values= c(15,16)) +
      ggplot2::scale_colour_gradient(low=scales::muted("red", c = 80, l = 50),
                                     as.numeric( names(kh.data::kh_ped)[1]),
                                     high=scales::muted("green", c = 80, l = 50),
                                     na.value="black") +
      ggplot2::geom_text(ggplot2::aes(x = lon, y = lat+0.01, label = id,
                                      size = relatedness+0.2),
                         data =outdf[outdf$dflist%in%dates,]) +
      ggplot2::geom_density2d(data = outdf[outdf$relatedness > median(outdf$relatedness) & outdf$dflist%in%dates,],
                              ggplot2::aes(x = lon, y = lat,  fill = "high"),
                              size = 0.01) +
    ggplot2::stat_density2d(data = outdf[outdf$relatedness < median(outdf$relatedness) & outdf$dflist%in%dates,],
                                  ggplot2::aes(x = lon, y = lat,  fill = "low", alpha = ..level..),
                                           size = 0.01, bins = 16, geom = 'polygon') +
      ggplot2::scale_fill_manual(values=c("red", "black", "green"),
                        name="Mean Rel.",
                        breaks=c(-1, median(outdf$relatedness), 1),
                        labels=c("low", "average", "high")) +
         ggplot2::scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
      ggplot2::ggtitle(paste("Kin network of ID", id, "on", dates)) +
      ggplot2::guides(colour = ggplot2::guide_colourbar(title = "Rel.", order = 1, direction = "horizontal"),
             shape = ggplot2::guide_legend(title = "Sex", direction = "horizontal", order = 2),
             size = ggplot2::guide_legend(title = "Age", direction = "horizontal", order = 3),
             fill = ggplot2::guide_legend(title = "Mean Rel.", direction = "horizontal", order = 4))
  })

  if(throw_plots==TRUE)try(suppressWarnings(print(outlist[[i]])))
  if(spit_results==TRUE)return(outlist)
}
