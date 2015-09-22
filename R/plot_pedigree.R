#' Plot a pedigree for a given individual at a given date
#'
#' This function plots a pedigree showing colored by relatedness and survival status with labels indivating age and locations of living pedigree members.
#' @param id Individual ID
#' @param evdat Dates of interest in the Year-month-day format (e.g. "1850-01-01")
#' @param df_ind A dataframe
#' @param ped An optional pedgree object related to x. Will be generated, if not provided.
#' @param evmat Event matrix
#' @param label optional vector of labels to add to pedigree members
#' @param cex size of text and symbols
#' @chrtype Chromosme type for relatedness calculation (either 'autosome' or 'X')
#' @keywords kh spouse
#' @export
#' @examples
#' \dontrun{
#' plot_pedigree(40, "1850-01-01", df_ind, evmat=evmat)
#' detach(kh33::kh)
#' }
plot_pedigree <- function(id, evdat, df_ind, ped=NULL, evmat, label = NULL, cex=0.6,chrtype="autosome"){
  if(is.null(ped)){
  ped <- grap_ped(id, df_ind)
  }
 if(is.null(label)) {  label <-  ifelse(is_alive(ped$id, paste(evdat), df_ind)==1,
    paste(ifelse(ped$id %in% as.numeric(dimnames(evmat)$id), paste0(
                round(get_age(ifelse(ped$id %in% as.numeric(dimnames(evmat)$id), ped$id, x), paste(evdat), evmat=evmat)), "y"), "?"),
          gsub("NA", "?", recode_evloc(na2zero(unlist(lapply(ped$id[which(ped$id %in% dimnames(evmat)$id)], get_kinloc, paste(evdat), evmat = evmat)), zero=34))),
          sep="\n"), "")}
  kinship2::plot.pedigree(ped, affected = is_alive(ped$id, paste(evdat), df_ind),
                          col = get_kincol(id, ped, X = ifelse(chrtype=="autosome", FALSE, TRUE)),
                          id = label, cex=cex)
  title(paste("Pedigree of ID", id, "on", evdat))
}
