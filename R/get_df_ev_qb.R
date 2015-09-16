#' A dataframe for individual events
#'
#' This function returns a dataframe (in "long format", i.e. an individual's ID spanning multiple rows) for event data (date, location, and type of event).
#'
#' @param df_ped A dataframe containing as columns "id", "bdate", "bplace", "gebkk", "ddate", "dplace", and "todkk"
#' @param df_fam A dataframe containing as columns "idm","idf", "dat4", "cort", and "fall"
#' @keywords kh events
#' @export
#' @examples
#' get_dads(kh[["df_ped"]]$id[2])
#'  get_dads(get_kids(get_dads(kh[["df_ped"]]$id[2])))
get_df_ev_qb <- function(df_ped = qb[{"df_ped"}]){
df_ped$affected <- ifelse(df_ped$id %in% df_ped$id[!is.na(df_ped$idf)] & df_ped$idf %in% df_ped$id[!is.na(df_ped$idf)], 1, 0)
ped1 <- with(df_ped, kinship2::pedigree(id, dadid, momid, sex,  affected, status, relation=relate))

#       k <- 1
#       for (i in relate[,1]){
#       relate[k,4] <- as.numeric(paste(df_twins$famid[df_twins$id == i]))
#       attributes(relate)$dimnames[[1]][k] <- paste(df_twins$famid[df_twins$id == i])
#       k <- k+1
#       }

df_ped <- df_ped[order(df_ped$momid, df_ped$bdate),]

df_ped$bnum <- as.numeric(factor(df_ped$bplace))
df_ped$dnum <- as.numeric(factor(df_ped$dplace))

df_ped$gebkk <- paste(df_ped$xdateb)
df_ped$todkk <- paste(df_ped$xdated)
.tmp <- df_ped[!is.na(df_ped$bdate),c("id", "bdate", "bplace", "gebkk")]
df_ev <- data.frame(id = .tmp$id, evdat = .tmp$bdate, evtyp = "*", evloc = .tmp$bplace, evid = .tmp$id, evspc = paste(.tmp$gebkk))
.tmp <- df_ped[!is.na(df_ped$ddate),c("id", "ddate", "dplace", "todkk")]
df_ev <- rbind(df_ev, data.frame(id = .tmp$id, evdat = .tmp$ddate, evtyp = "+", evloc = .tmp$dplace, evid = .tmp$id, evspc = paste(.tmp$todkk)))
.tmp <- df_ped[!is.na(df_ped$bdate) & df_ped$momid>0,c("momid", "bdate", "bplace", "id", "gebkk")]
df_ev <- rbind(df_ev, data.frame(id = .tmp$momid, evdat = .tmp$bdate, evtyp = "#", evloc = .tmp$bplace, evid = .tmp$id, evspc = paste(.tmp$gebkk)))
.tmp <- df_ped[!is.na(df_ped$bdate) & df_ped$dadid>0,c("dadid", "bdate", "bplace", "id", "gebkk")]
df_ev <- rbind(df_ev, data.frame(id = .tmp$dadid, evdat = .tmp$bdate, evtyp = "#", evloc = .tmp$bplace, evid = .tmp$id, evspc = paste(.tmp$gebkk)))
#   .tmp <- df_fam[!is.na(df_fam$dat4) & !is.na(df_fam$idm),c("idm", "dat4", "cort")]
#   df_ev <- rbind(df_ev, data.frame(id = .tmp$idm, evdat = as.Date(as.POSIXct(.tmp$dat4, origin="1582-10-14")), evtyp = "oo", evloc = get.kirchspiele(.tmp$cort)))
#   .tmp <- df_fam[!is.na(df_fam$dat4) & !is.na(df_fam$idf),c("idf", "dat4", "cort")]
#   df_ev <- rbind(df_ev, data.frame(id = .tmp$idf, evdat = as.Date(as.POSIXct(.tmp$dat4, origin="1582-10-14")), evtyp = "oo", evloc = get.kirchspiele(.tmp$cort)))

df_ev <- df_ev[order(df_ev$id, df_ev$evdat),]
#df_ev$test <- factor(paste(df_ev$evtyp, df_ev$evdat, df_ev$evid, sep="."))
# df_ev[df_ev$id==55255,]


# max(table(df_ev$id))
#df_ev[df_ev$id==55255,]
#tapply(df_ev$evspc, df_ev$evtyp, table)
df_ev$evspc <- paste(df_ev$evspc)
df_ev$evspc[df_ev$evspc=="NA"] <- ""
df_ev$evspc <- factor(df_ev$evspc)

###  here we go
# load("/home/johow/Dropbox/cobreeding/data/geo.Rdata")

df_ev$evloc <- as.integer(df_ev$evloc)
# levels(df_ev$evloc) <- unlist(lapply(levels(df_ev$evloc), get_kirchspiele))
# df_ev <- subset(df_ev, evtyp == "+" & !grepl("is duly recorded", paste(df_ev$evspc)))
# tapply(df_ev$evspc, df_ev$evtyp, table)
#
df_ped <- df_ped[order(df_ped$id),]
df_ped$bspc <- as.numeric(factor(df_ped$gebk))
df_ped$dspc <- as.numeric(factor(df_ped$todk)) + max(df_ped$bspc)
#

df_ped$idf <- na2zero( df_ped$idf)
df_ped$idm <- na2zero( df_ped$dadid)

df_ped <- df_ped[order(df_ped$idf, df_ped$bdate),]
df_ped$mat_parity <- 1
df_ped$mat_parity[df_ped$idf>0] <- as.numeric(unlist(mapply("seq", from=rep(1,length(as.numeric(table(df_ped$idf[df_ped$idf>0])))),
                                                            to=c(as.numeric(table(df_ped$idf[df_ped$idf>0]))),by= 1)))

df_ped <- df_ped[order(df_ped$idm, df_ped$bdate),]
df_ped$pat_parity <- 1
df_ped$pat_parity[df_ped$idm>0] <- as.numeric(unlist(mapply("seq", from=rep(1,length(as.numeric(table(df_ped$idm[df_ped$idm>0])))),
                                                            to =c(as.numeric(table(df_ped$idm[df_ped$idm>0]))),by= 1)))
max_parity <- max(df_ped$pat_parity)
gc()

return(df_ev[order(df_ev$id),])
}
