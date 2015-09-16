# chrono_map <- function(id=NULL,
# list_kin=NULL,
# list_geo=NULL,
# my_map=NULL){
# outdf <- unlist_df(lapply(list_kin[[paste(id)]], merge,
# cbind(kinloc = recode_evloc(list_geo[["df"]]$label), list_geo[["df"]][,!colnames(list_geo[["df"]]) %in% "label"]),
# by = "kinloc"))
# outdf$momdad <- na2zero(outdf$relmom)-na2zero(outdf$reldad)
# outlist  <- list("")
# dates <- unique(paste(outdf$dflist))
# for (i in 1:length(dates)){
# outlist[[i]] <-  ggmap::ggmap(my_map) +
# ggplot2::geom_point(ggplot2::aes(x = lon, y = lat,
# colour=momdad, shape = factor(sex)),   position = "jitter",
# data = outdf[outdf$dflist%in%dates[[i]],]) +
# ggplot2::scale_shape_manual(values= c(16,15)) +
# ggplot2::scale_colour_gradient(low=scales::muted("red", c = 80, l = 50),
# high=scales::muted("green", c = 80, l = 50),
# na.value="black")  +
# ggplot2::geom_text(ggplot2::aes(x = lon, y = lat, colour = momdad),
#                    label = substr(dates,1,4)[[i]],
# data =outdf[outdf$dflist%in%dates[[i]],]) +
# ggplot2::ggtitle(paste("Kin network of ID", my_id, "on", dates[[i]]))
# }
# return(outlist)
# }
