#' Number of living children being younger than 8 years of a given individual at a given date
#'
#' This function returns  the number of living children being younger than 8 years of a given individual at a given date
#' @param id The ID of the individual parent (if any children exist) of interest (will be zero if no living children under age 8 alive)
#' @param evdat Date of interest in the Year-month-day format (e.g. "1850-01-01")
#' @param df_ped A dataframe
#' @keywords kh count children
#' @export
#' @examples
#' \dontrun{
#' count_kids8(1067, paste(as.Date(evmat["1067", 4, 1], origin = "1970-01-01")), df_ped)
#' }

count_kids8 <- function(id, evdat, df_ped){
  if(length(id) == 1){
    return(count_kids8_int(id, evdat,df_ped))} else
  return(mapply("count_kids8_int", id, evdat, rep(df_ped, length(id))))
}

count_kids8_int <- function(id, evdat, df_ped){
  return(sum(
    as.numeric(
      as.numeric(difftime(as.Date(evdat),df_ped$bdate[df_ped$momid == id | df_ped$dadid == id],  units = "days"))/365.25 < 8 &
                          as.numeric(difftime(as.Date(evdat),df_ped$bdate[df_ped$momid == id | df_ped$dadid == id], units = "days")) >=0 &
                          (is.na(df_ped$ddate[df_ped$momid == id | df_ped$dadid == id]) | df_ped$ddate[df_ped$momid == id | df_ped$dadid == id]>as.Date(paste(evdat)))), na.rm=TRUE))

}
