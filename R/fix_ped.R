#' Prepare a pedigree dataframe by excluding 'half-parents'
#'
#' This function returns a pedigree dataframe, which is restricted to members of its largest pedigree.
#' @param x ID of individual for which a pedigree should be composed.
#' @param df_ind A dataframe for genealogical relations providing the variables 'id', 'momid' and 'dadid'
#' @param subsample_id A vector of IDs  (Set 'as.numeric(dimnames(evmat)$id') for IDs with events)
#' @keywords pedigree
#' @export
#' @examples
#' \dontrun{
#' fix_ped(grap_kin(1067, df_ped), df_ped)
#' }
fix_ped <- function(x, df_ind=NULL, subsample_id = NULL){
  if (is.data.frame(x)){df_tmp <- x} else {df_tmp <- df_ind[df_ind$id %in% x,]}
  df_tmp <- rbind(df_tmp, df_ind[!df_ind$id %in% df_ind$id &
                                   (df_ind$id %in% df_tmp$dadid |
                                      df_ind$id %in% df_tmp$momid),])
  df_tmp <- df_tmp[!duplicated(df_tmp$id),]
  if (!is.null(subsample_id)){df_tmp <- df_tmp[df_tmp$id %in% subsample_id,]}
  df_tmp$momid <- ifelse(df_tmp$momid %in% df_tmp$id, df_tmp$momid, 0)
  df_tmp$dadid <- ifelse(df_tmp$dadid %in% df_tmp$id, df_tmp$dadid, 0)
  while (any((df_tmp$momid>0 & df_tmp$dadid %in% 0) | (df_tmp$dadid>0 & df_tmp$momid %in% 0))){
    df_tmp$momid <- ifelse(df_tmp$dadid %in%  0 | !df_tmp$dadid %in% df_tmp$id | !df_tmp$momid %in% df_tmp$id, 0, df_tmp$momid)
    df_tmp$dadid <- ifelse(df_tmp$momid %in%  0 | !df_tmp$dadid %in% df_tmp$id | !df_tmp$momid %in% df_tmp$id, 0, df_tmp$dadid)
  }
df_tmp$famid <- with(df_tmp, kinship2::makefamid(id, dadid, momid))
df_tmp <- subset(df_tmp, famid == as.numeric(names(which(table(df_tmp$famid) == max(table(df_tmp$famid))))))

return(df_tmp[order(df_tmp$id),names(df_tmp)[names(df_tmp)!="famid"]])
}
