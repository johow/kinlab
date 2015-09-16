#' A datatable for individual events
#'
#' This function returns a data.table::data.table object (in "long format", i.e. an individual's ID spanning multiple rows) for event data (date, location, and type of event).
#'
#' @param df_ind A dataframe containing as columns "id", "bdate", "bplace", "bstatus", "ddate", "dplace", and "dstatus"
#' @param df_fam A dataframe containing as columns "idm","idf", "dat4", "cort", and "fall"
#' @keywords kh events
#' @export
#' @examples
#' \dontrun{
#' df_ind <- get_exmpl_df()
#' df_fam <- data.table::data.table(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
#' df_ev <- get_df_ev(df_ind, df_fam)
#' }

get_dt_ev <- function(df_ind=NULL, df_fam=NULL){
  #  df_ind <- merge(df_ind, df_fam[,c("doc", "idf", "idm")], by ="doc", all.x=TRUE)
  df_ind$idf <- zero2na(df_ind$momid)
  df_ind$idm <- zero2na(df_ind$dadid)
  .tmp <- df_ind[!is.na(df_ind$bdate),c("id", "bdate", "bplace", "bstatus")]
  df_ev <- data.table::data.table(id = .tmp$id, evdat = .tmp$bdate, evtyp = "*", evloc = .tmp$bplace, evid = .tmp$id, evspc = paste(.tmp$bstatus))
  .tmp <- df_ind[!is.na(df_ind$ddate),c("id", "ddate", "dplace", "dstatus")]
  df_ev <- rbind(df_ev, data.table::data.table(id = .tmp$id, evdat = .tmp$ddate, evtyp = "+", evloc = .tmp$dplace, evid = .tmp$id, evspc = paste(.tmp$dstatus)))
  .tmp <- df_ind[!is.na(df_ind$bdate) & !is.na(df_ind$idf),c("idf", "bdate", "bplace", "id", "bstatus")]
  df_ev <- rbind(df_ev, data.table::data.table(id = .tmp$idf, evdat = .tmp$bdate, evtyp = "#", evloc = .tmp$bplace, evid = .tmp$id, evspc = paste(.tmp$bstatus)))
  .tmp <- df_ind[!is.na(df_ind$bdate) & !is.na(df_ind$idm),c("idm", "bdate", "bplace", "id", "bstatus")]
  df_ev <- rbind(df_ev, data.table::data.table(id = .tmp$idm, evdat = .tmp$bdate, evtyp = "#", evloc = .tmp$bplace, evid = .tmp$id, evspc = paste(.tmp$bstatus)))
  if (is.null(df_fam)==FALSE){
    .tmp <- df_fam[!is.na(df_fam$dat4) & !is.na(df_fam$idm),c("idm","idf", "dat4", "cort", "fall")]
    levels(.tmp$cort) <- unlist(lapply(levels(.tmp$cort), kinlab::get_kirchspiele))
    df_ev <- rbind(df_ev, data.table::data.table(id = .tmp$idm, evdat = as.Date(as.POSIXct(.tmp$dat4, origin="1582-10-14")), evtyp = "oo",
                                     evloc = .tmp$cort, evid = .tmp$idf, evspc = .tmp$fall))
    .tmp <- df_fam[!is.na(df_fam$dat4) & !is.na(df_fam$idf),c("idm","idf", "dat4", "cort", "fall")]
    levels(.tmp$cort) <- unlist(lapply(levels(.tmp$cort), kinlab::get_kirchspiele))
    df_ev <- rbind(df_ev, data.table::data.table(id = .tmp$idf, evdat = as.Date(as.POSIXct(.tmp$dat4, origin="1582-10-14")), evtyp = "oo",
                                     evloc = .tmp$cort, evid = .tmp$idm, evspc = .tmp$fall))
  }
  df_ev <- df_ev[order(df_ev$id, df_ev$evdat),]
  df_ev$tmp2 <- paste(df_ev$id, df_ev$evdat, df_ev$evtyp, df_ev$evid, sep="_")
  df_ev$tmp <- duplicated(df_ev$tmp2)
  df_ev <- df_ev[df_ev$id > 0 & df_ev$tmp == FALSE,1:6]

  df_ev$evspc <- paste(df_ev$evspc)
  df_ev$evspc[df_ev$evspc=="NA"] <- ""
  df_ev$evspc <- factor(df_ev$evspc)
  df_ev <- subset(df_ev, !(evtyp == "+" & evspc == "E"))
  df_ev <- df_ev[order(df_ev$id, df_ev$evdat),]
  return(df_ev)
}
