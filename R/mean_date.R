#' Mean date in intervals
#'
#' This function returns a dataframe being required to contruct pedigrees via the _kinship2::pedigree()_ function
#' @param df A dataframe 1
#' @param ldate A date 1
#' @param rdate A date 2
#' @keywords kh import
#' @export
#' @examples
#' \dontrun{
#' #TBD
#' }
#'
mean_date <- function(df=NULL, ldate=NULL, rdate=NULL){
  return(as.Date(apply(cbind(
    as.Date(df[,paste(ldate)]),
    as.Date(df[,paste(rdate)])), 1, sum)/2, origin="1970-01-01"))
  }
