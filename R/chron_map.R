#' Chronological maps
#'
#' Returns a list of map objects, showing estimated locations for all living pedigree members for a given individual
#' \code{ggmap} provided plot of a map showing the estimated locations of living pedigree members for a given ID at a given date. The size, shape and color of symbols represent for a specific individual's age, sex, and relatedness to an indiviudal x.
#' @param id The individual ID of interests
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

chron_map <- function(id=NULL,
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
dates <- unique(paste(outdf$dflist))
for (i in 1:length(dates)){
outlist[[i]] <-  ggmap::ggmap(my_map) +
ggplot2::geom_point(ggplot2::aes(x = lon, y = lat, size = age,
colour=relatedness, shape = sex),   position = "jitter",
data = outdf[outdf$dflist%in%dates[[i]],]) +
ggplot2::scale_shape_manual(values= c(15,16)) +
ggplot2::scale_colour_gradient(low=scales::muted("red", c = 80, l = 50),
high=scales::muted("green", c = 80, l = 50),
na.value="black")  +
ggplot2::geom_text(ggplot2::aes(x = lon, y = lat+0.01, colour = relatedness, label = id),
data =outdf[outdf$dflist%in%dates[[i]],]) +
ggplot2::ggtitle(paste("Kin network of ID", id, "on", dates[[i]]))
}
if(throw_plots==TRUE)for(i in outlist) try(print(i))
if(spit_results==TRUE)return(outlist)
}
