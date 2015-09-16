#' Combine dataframes in a list by rows
#'
#' This function returns the elements of a list of dataframes as one single dataframe (combined by rows, i.e. all dataframes must have the same number of columns).
#' @param dflist A list of dataframes
#' @keywords kh33 spouse
#' @export
#' @examples
#' a_df <- data.frame(a=c(101,202), b=1:2, c=c("A", "B"))
#' b_df <- data.frame(a=c(303,404), b=3:4, c=c("C", "D"))
#' test <- list("a"=a_df, "b"=b_df)
#' unlist_df(test)
unlist_df <- function(dflist=NULL){
  df <- dflist[[1]]
  df$dflist <- names(dflist)[1]
  for (i in 2:length(dflist)){
  df <-  rbind(df, cbind(dflist[[i]], dflist=names(dflist)[i]))
  }
  return(df)
}
