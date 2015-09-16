#' Run time since t_start 
#' 
#' This function returns the time difference from `Sys.time()` to `t_start`, either as printed message (Default) or as `difftime`.
#' @param t_start Time, for which time difference should be computed
#' @param print_message Set to FALSE, if to turn off messages and return a `difftime` instead. 
#' @keywords pedigree
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }

run_time <- function(t_start=
                        as.POSIXct(1, origin = "1981-12-13"), print_message = TRUE){
  if(print_message==TRUE){
  message(paste("## ...Running", round(difftime(Sys.time(), t_start), 2), 
                attr(difftime(Sys.time(), t_start), "units"), "(start on", format(t_start, "%a %X)")))
  } else return(difftime(Sys.time(), t_start))
}