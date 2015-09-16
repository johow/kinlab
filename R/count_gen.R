#' Count generation numbers
#' 
#' This function wraps the _pedigree::countGen()_ function to calculate generation number of each ID.
#' @param df_ind A dataframe with individual cases (id, dadid, momid).
#' @keywords kh import
#' @export
#' @examples
#' df_ind <- get_exmpl_df()
#' count_gen(df_ind)

count_gen <- function(df_ind=NULL){
  df_gen <- df_ind[,c("id", "dadid", "momid")]
  df_gen$dadid <- kinlab::zero2na(df_gen$dadid)
  df_gen$momid <- kinlab::zero2na(df_gen$momid)
  df_gen$reord <- 1:nrow(df_gen)
  df_gen <- df_gen[order(pedigree::orderPed(df_gen)),]
  merge(df_ind[,c("id", "bdate")], data.frame(id = df_gen$id, genr = pedigree::countGen(df_gen)), by = "id")[order(df_gen$reord),"genr"]  
}