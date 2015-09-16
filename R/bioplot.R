#' Plot Biographies
#'
#' For a given ID sample, this function plots age intervals found in event array 'evmat'
#' @param id An ID included in 'dimnames(evmat)'
#' @param df_ind Individual data frama
#' @param evmat An event array (see '?get_evmat')
#' @keywords spatial distance pedigree
#' @export
#' @examples
#' \dontrun{
#' valid_dates(1570, kh_mat)
#' }
bio_plot <- function(id, df_ind, evmat){

my_dates <- lapply(id, valid_dates, evmat)

ages <-  lapply(
            lapply(
               mapply(
                 "difftime", my_dates, lapply(my_dates, "[", 1), units="days"
                 ),
               as.numeric),
            "*", 1/365.25)

years <- lapply(lapply(my_dates, format, "%Y"), as.integer)
sex <- get_sex(id, df_ind)

plot(c(0, max(unlist(ages)))~c(0, max(unlist(ages))), type="n", axes=FALSE, xlab = "", ylab="",
     ylim=c(0,length(ages)), xlim = c(0, max(unlist(ages))))
for (k in 1:length(ages)){
for (i in length(ages[[k]]):2){
  lines(c(0, ages[[k]][i]),c(k,k),  lwd=12, col = ifelse(i==length(ages[[k]]), "gray30", i-1))
}
}
}
