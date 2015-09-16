#' Return a compact, individual-centered pedigree by iteratively adding relatives until a given minimum pedigree size is reached.
#'
#' This function returns a pedigree object (as specified in the kinship2-package) for a given individual by iteratively adding spouses and first degree relatives until a given size is reached.
#' @param x ID of individual for which a pedigree should be composed.
#' @param df_ind A dataframe for genealogical relations providing the variables 'id', 'momid' and 'dadid'
#' @param relate A relationshi matrix specifying twin zygosity (1=MZ, 2=DZ, 3=unknown)
#' @param subsample_id A vector of IDs (Set 'as.numeric(dimnames(evmat)$id') for IDs with events)
#' @param min_size Minimum size of pedigree of interest (Default is 200)
#' @param max_count Maximum number of iterations in adding relatives to pedigree
#' @keywords pedigree
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }

grap_ped <- function(x=NULL, df_ind=NULL, relate=NULL, subsample_id = NULL, min_size = 300, max_count=9){
  df_tmp <- grap_df(x, df_ind, subsample_id, min_size, max_count)

  df_tmp$twin <- ifelse(df_tmp$momid > 0 & df_tmp$id %in% c(as.numeric(relate[,1]), as.numeric(relate[,2])), 1, 0)
  if (sum(df_tmp$twin)>2){
    .pedout <- with(df_tmp, kinship2::pedigree(id, dadid, momid, sex, affected = as.matrix(affected), status, relation =
                                      matrix(as.numeric(relate[relate[,1] %in% df_tmp$id[df_tmp$momid>0] & relate[,2] %in% df_tmp$id[df_tmp$momid>0],1:3]), ncol=3), missid=0))
  }
  if (sum(df_tmp$twin)==2){
    .pedout <- with(df_tmp, kinship2::pedigree(id, dadid, momid, sex, affected = as.matrix(affected), status, relation =
                                      matrix(as.numeric(relate[relate[,1] %in% df_tmp$id[df_tmp$momid>0] & relate[,2] %in% df_tmp$id[df_tmp$momid>0],1:3]), ncol=3), missid=0))
  }
  else {
    .pedout <- with(df_tmp, kinship2::pedigree(id, dadid, momid, sex, affected, status, missid = 0))
  }
 return(.pedout)
  }
