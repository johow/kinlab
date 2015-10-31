#' A snapshot of an individual's kinship structure at a fixed date
#'
#' This function returns a dataframe containing pedigree members of individual `x` being alive at date `evdat`.
#' @param x ID of individual for which a pedigree should be composed.
#' @param evdat A specific date in the format ``Year-month-day''
#' @param ped Pedigree of `x`
#' @param df_ind Dataframe for individuals
#' @param df_fam Dataframe for unions
#' @param evmat Event matrix
#' @param map_dist A distance matrix
#' @keywords spatial distance pedigree
#' @export
#' @examples
#' \dontrun{
#' attach(kh33::kh)
#' get_dist(40, grap_ped(40, df_ped), "1850-01-01", dist_mat = geo[["map_dist"]], evmat=evmat)
#' detach(kh33::kh)
#' }

get_kinset <- function(x, evdat, ped, df_ind, df_fam, evmat, map_dist){
  df_tmp <- kinship2::as.data.frame.pedigree(ped)
  df_tmp$index <- x
  df_tmp$evdat <- paste(evdat)
  df_tmp <- subset(df_tmp, is_alive(df_tmp$id, paste(evdat), df_ind)==1)
  df_tmp$myloc <- get_kinloc(x, paste(evdat), evmat)
  df_tmp$kinloc <- as.numeric(lapply(df_tmp$id, get_kinloc, evdat = paste(evdat), evmat))
  df_tmp$dist <- as.numeric(map_dist[df_tmp$myloc[1], df_tmp$kinloc])
  df_tmp$age <- as.numeric(lapply(df_tmp$id, get_age, paste(evdat), evmat=evmat))
  df_tmp$relmom <- as.numeric(kinship2::kinship(ped)[paste(x),paste(df_tmp$id)])
  if (kinlab::actual_spouse(x, paste(evdat), df_ind, df_fam) %in% df_tmp$id){
   df_tmp$reldad <- as.numeric(kinship2::kinship(ped)[paste(kinlab::actual_spouse(x, paste(evdat), df_ind, df_fam)),paste(df_tmp$id)])
  } else df_tmp$reldad <- NA
  df_tmp$relmomx <- as.numeric(kinship2::kinship(ped, chrtype="X")[paste(x),paste(df_tmp$id)])
  if (kinlab::actual_spouse(x, paste(evdat), df_ind, df_fam) %in% df_tmp$id){
    df_tmp$reldadx <- as.numeric(kinship2::kinship(ped, chrtype="X")[paste(kinlab::actual_spouse(x, paste(evdat), df_ind, df_fam)),paste(df_tmp$id)])
  } else df_tmp$reldad <- NA
  return(df_tmp)
}
