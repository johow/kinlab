#' Get IDs of twinmothers
#'
#' Returns a vector of twinmother IDs
#' @param df A dataframe containing birth dates of offspring and mother IDs
#' @param var_bdate Variable for offspring's birth date
#' @param var_mother Variable of mother ID
#' @param numeric_output Should index be numeric
#' @keywords kh localitiy
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }
get_twinmothers <- function(df = NULL,
                             var_bdate=NULL,
                             var_mother=NULL,
                             numeric_output = FALSE){
  index <- which(df[,paste(var_mother)]>0 &
                   !is.na(df[,paste(var_bdate)]))
    if (numeric_output == TRUE){
  return(
    as.numeric(
      names(
        unlist(
          lapply(
            lapply(
              lapply(
                tapply(df[index,paste(var_bdate)],
                       df[index,paste(var_mother)],
                       table),
                ">", 1),
              any),
            which)))))
  } else
    return(
      names(
        unlist(
          lapply(
            lapply(
              lapply(
                tapply(df[index,paste(var_bdate)],
                       df[index,paste(var_mother)],
                       table),
                ">", 1),
              any),
            which))))
}
