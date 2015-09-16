#' A pedigree dataframe for subset of limited size
#'
#' For a given individual, this function returns a pedigree dataframe for close kin
#' @param x An individual ID
#' @param df_ind A dataframe for genealogical relations providing the variables 'id', 'momid' and 'dadid'
#' @param min_size Minimum size, i.e. number of family members
#' @param max_count Maximum number of iterations
#' @keywords kh pedigree
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }
grap_df <- function(x = NULL, df_ind = NULL, subsample_id = NULL, min_size=300, max_count = 9){
  count <- 1
  while(length(x) < min_size & count < max_count){
    x <-   kin_crawler(x, df_ind)
    count <- count + 1
    }
  return(fix_ped(df_ind[df_ind$id %in% x,], df_ind, subsample_id))
}

