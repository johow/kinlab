#' Rainbow color palette for relatedness coefficients
#'
#' This function returns a vector containing color names based on the relatedness coefficients between a given individual's ID and all members of a given pedigree. The given individual is returned as 'black', while colors of other pedigre members depend on relatedness coefficients ranging from 'green' (closely related) to 'red' (relatively unrelated).
#'
#' @param id An individual ID
#' @param pedigree.name The individual's pedigree
#' @param df_ped A dataframe
#' @param my.col Color for ego of individual ID (default is 'black')
#' @param X TRUE if X-chromosomal relatedness instead of autosomal relatedness should be computed (default is FALSE)
#' @keywords kh pedigree events
#' @export
#' @examples
#' \dontrun{
#' df_ind <- get_exmpl_df()
#' df_ind$bdate <- sample(seq(as.Date("1774-12-31"), as.Date("1874-12-31"), 100), nrow(df_ind))
#' df_fam <- data.frame(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
#' my_id <- sample_kh(df_ind, df_fam)
#' get_kincol(1067, df_ped= df_ind)
#' }

get_kincol <- function(id, pedigree.name = NA, df_ped, my.col = "black", X=FALSE){
 if (X == TRUE){
   if (all(is.na(pedigree.name))){
     .ped <- grap_ped(id, df_ped)
   }
 else {
   .ped <- pedigree.name
 }
 .kin <- kinship2::kinship(.ped, chrtype="X")
 .col <- rainbow(length(table(as.numeric(.kin[paste(id), paste(.ped$id)]))), start = 0, end = 2/6, alpha = 1)[as.numeric(factor(as.numeric(.kin[paste(id), paste(.ped$id)])))]
 .col[which(.ped$id==id)] <- paste(my.col)
 return(.col)}
 else {
  if (all(is.na(pedigree.name))){
    .ped <- grap_ped(id, df_ped)
  }
  else {
    .ped <- pedigree.name
  }
  .kin <- kinship2::kinship(.ped)
  .col <- rainbow(length(table(as.numeric(.kin[paste(id), paste(.ped$id)]))), start = 0, end = 2/6, alpha = 1)[as.numeric(factor(as.numeric(.kin[paste(id), paste(.ped$id)])))]
  .col[which(.ped$id==id)] <- paste(my.col)
  return(.col)}
}
