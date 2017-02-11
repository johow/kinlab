#' Return spousal ID(s) for a married individual
#'
#' This function returns individual IDs of a given individual spouses (if possible, i.e. individual must be a parent).
#' @param x ID of married individual, whose spouses should be returned
#' @param df_ped A dataframe
#' @keywords spouses IDs
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }
# get_spouses <- function(x, df_ped){
#   df_ped <- df_ped[df_ped$momid %in% x | df_ped$dadid %in% x,c("momid", "dadid")]
#   df_ped$tmp <- as.numeric(factor(paste0(df_ped$momid, df_ped$dadid, sep="and")))
#   df_ped <- subset(df_ped, !duplicated(df_ped$tmp))
#   df_ped_tmp <- data.frame(id = unique(x), tmp = NA)
#   if (any(df_ped_tmp$id %in% df_ped$momid) & !any(df_ped_tmp$id %in% df_ped$dadid)){
#   df_ped_tmp <- merge(df_ped_tmp, data.frame(id = df_ped$momid, spouse = df_ped$dadid), by = "id", all.x=TRUE)
#   }
#   if (any(df_ped_tmp$id %in% df_ped$dadid) & !any(df_ped_tmp$id %in% df_ped$momid)){
#     df_ped_tmp <- merge(df_ped_tmp, data.frame(id = df_ped$dadid, spouse = df_ped$momid), by = "id", all.x=TRUE)
#   }
#
#   if (any(df_ped_tmp$id %in% df_ped$dadid) & any(df_ped_tmp$id %in% df_ped$momid)){
#     df_ped_tmp <- merge(df_ped_tmp, data.frame(id = df_ped$momid, spouse = df_ped$dadid), by = "id", all.x=TRUE)
#     df_ped_tmp <- merge(df_ped_tmp, data.frame(id = df_ped$dadid, spouse = df_ped$momid), by = "id", all.x=TRUE)
#   }
#   return(df_ped_tmp$spouse)
# }
get_spouses <- function(x, df_ped){
  outObj <- NA
  if (x>0 & x %in% df_ped$momid){
    outObj <- unique(df_ped$dadid[df_ped$momid%in%x])
  }
  if (x>0 & x %in% df_ped$dadid){
    outObj <- unique(df_ped$momid[df_ped$dadid%in%x])
}
return(outObj)
}
