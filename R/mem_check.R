#' Check for memory limits
#'
#' This function tests whether the currently used memory
#' is above a given limit (Default to 999 MB) and either
#' gives out an error or the value of it's second argument.
#'
#' @param limit Numerical value for maximum memory usage allowed (Default is 999 MB)
#' @param error_message Error message to be returned, in case of exceeded memory limit.
#' @param only_warn Should instead only a warning be given? (Default is FALSE)
#' @keywords memory
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }

mem_check <- function(limit=999, error_message = NULL, only_warn = FALSE){
  if (only_warn==TRUE){
    if(gc()[2,2]>limit) warning(paste0("Currently used memory (", gc()[2,2], " MB) is exceeding your limit (", limit, " MB)!"))
  } else if (is.null(error_message)){
    stopifnot(gc()[2,2]<=limit)
  } else if(gc()[2,2]>limit){stop(error_message)}
}
