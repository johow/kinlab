#' Wrapper function to import and format Qu\enc{é}{e}bec data for children and families from SPSS files
#'
#' This function returns a list containing three data.frames:
#' \enumerate{
#'   \item Imported 'Individus' SPSS file as read from source.
#'   \item Dataframe holding individual IDs and those of the parents.
#'   \item Matrix describing event data in the form date, id, location, special, status. Here, gemini births count as one single event (see get_evmat_twins() for including the birth of twins as multiple events).
#'   \item Matrix describing event data in the form date, id, location, special, status. Here, gemini births count as one single event (see get_evmat_twins() for including the birth of twins as multiple events).
#'   \item Matrix describing twin relations in the form 'id1-id2-code', where code stands for 1=monozygotous, 2=dizygotous, 3=unknown. Here, zygosity is soley based on sex and therefore only twins of different sex are given code=2 (i.e. dizygotous), while same-sex twins are classified as unknown (code=3). In the case of triplets (and twins of higher order), each individual will have relations to each of his co-twins.
#'   }
#' @param Individus File path to  the 'Individus.sav' SPSS file.
#' @keywords qb import
#' @export
#' @examples
#' notrun{
#' twinmat <- get_twinmat()
#' # str(twinmat)
#' }
get_qb <- function(Individus = "/home/johow/Dropbox/qb/RPQA.MarcKlemp.individus.2012-01-27.sav"){
  ###############################
  #
  #  DATA IMPORT AND PREPARATION
  #
  ###############################

  #
  # IMPORT DATA
  #

  qb.ind <- suppressMessages(suppressWarnings(foreign::read.spss(Individus, to.data.frame=TRUE)))

qb.ind <- qb.ind[qb.ind$idIndividu>0,]
qb.ind$bdate.l <- as.Date(paste(qb.ind$dateNaissAnnee, ifelse(qb.ind$dateNaissMois>0, qb.ind$dateNaissMois, 1),
                                ifelse(qb.ind$dateNaissJour>0,qb.ind$dateNaissJour, 1), sep="-"))
qb.ind$bdate.r <- as.Date(paste(qb.ind$dateNaissAnnee, ifelse(qb.ind$dateNaissMois>0, qb.ind$dateNaissMois, 12),
                                ifelse(qb.ind$dateNaissJour>0,qb.ind$dateNaissJour,
                                       ifelse(qb.ind$dateNaissMois %in% c(1,3,5,7,8,10,12), 31,
                                              ifelse(qb.ind$dateNaissMois == 2, 28, 30))), sep="-"))
qb.ind$ddate.l <- paste(qb.ind$dateDecesAnnee, ifelse(qb.ind$dateDecesMois>0, qb.ind$dateDecesMois, 1),
                        ifelse(qb.ind$dateDecesJour>0,qb.ind$dateDecesJour, 1), sep="-")

qb.ind$ddate.l <- as.Date(ifelse(qb.ind$ddate.l == "NA-NA-NA", NA, paste(qb.ind$ddate.l)))

qb.ind$ddate.r <- paste(qb.ind$dateDecesAnnee, ifelse(qb.ind$dateDecesMois>0, qb.ind$dateDecesMois, 12),
                        ifelse(qb.ind$dateDecesJour>0,qb.ind$dateDecesJour,
                               ifelse(qb.ind$dateDecesMois %in% c(1,3,5,7,8,10,12), 31,
                                      ifelse(qb.ind$dateDecesMois == 2, 28, 30))), sep="-")

qb.ind$ddate.r <- as.Date(ifelse(qb.ind$ddate.r == "NA-NA-NA", NA, paste(qb.ind$ddate.r)))

qb.ind$ind <- qb.ind$idIndividu
qb.ind$pere <- qb.ind$idPere
qb.ind$mere <- qb.ind$idMere
qb.ind$sex <- qb.ind$sexe
qb.ind$bdate <- ifelse(qb.ind$bdate.l == qb.ind$bdate.r & !is.na(qb.ind$bdate.l) & !is.na(qb.ind$bdate.r), qb.ind$bdate.l, NA)
qb.ind$xdateb <- qb.ind$qualiteDateNaiss
qb.ind$parishb <- qb.ind$codeLieuNaiss
qb.ind$ddate <- ifelse(qb.ind$ddate.l == qb.ind$ddate.r & !is.na(qb.ind$ddate.l) & !is.na(qb.ind$ddate.r), qb.ind$ddate.l, NA)
qb.ind$xdated <- qb.ind$qualiteDateDeces
qb.ind$parishd <- qb.ind$codeLieuDeces

df_full <- subset(qb.ind[,c("ind", "pere", "mere", "sex", "bdate", "xdateb", "parishb", "ddate", "xdated", "parishd")],
                  apply(qb.ind[,c("amerindien", "immigrant", "emigrant")], 1, sum)==0 &
                    !is.na(mere) & !is.na(pere))
names(df_full) <- c("id", "dadid", "momid", "sex", "bdate", "xdateb", "bplace", "ddate", "xdated", "dplace")
df_full$sex_old <- df_full$sex; df_full$sex <- NULL
df_full$sex <- ifelse(df_full$sex_old == "m", 1, ifelse(df_full$sex_old == "f", 2, 3))
df_full$sex_old <-NULL

df_full2 <- rbind(df_full,
                  data.frame(id = c(unique(df_full$dadid[!df_full$dadid %in% df_full$id]),
                                    unique(df_full$momid[!df_full$momid %in% df_full$id])),
                             dadid = 0, momid = 0, bdate = NA, xdateb =0, bplace = NA,
                             ddate = NA, xdated = 0, dplace = NA, sex = c(rep(1, length(unique(df_full$dadid[!df_full$dadid %in% df_full$id]))),
                                                                          rep(2, length(unique(df_full$momid[!df_full$momid %in% df_full$id]))))))

df_full <- df_full2[order(df_full2$id),]

df_full$famid <- with(df_full, kinship2::makefamid(id, dadid, momid))
df_full$proband <- ifelse(df_full$id %in% df_full$momid, 1, 0)

for (i in c("id", "momid", "dadid")){
  df_full[,paste(i)] <- as.numeric(paste(df_full[,paste(i)]))
}

bpeds <- with(df_full,
              kinship2::pedigree(id, dadid, momid, sex, affected=proband, famid=famid))

df_full$proband <- ifelse(df_full$momid > 0 & df_full$dadid > 0 & df_full$id %in% df_full$momid, 1, 0)
df_full$status <- ifelse(is.na(df_full$ddate), 0, 1)
#   summary(df_full$ddate)
df_ped <- subset(df_full, famid==as.numeric(names(which(table(df_full$famid)==max(table(df_full$famid))))))
df_ped$idf <- zero2na(df_ped$momid)
df_ped <- df_ped[order(df_ped$idf, df_ped$bdate),]
df_ped$ibi <- ifelse(df_ped$idf == c(df_ped$idf[2:nrow(df_ped)], NA) & !is.na(df_ped$bdate),
                     as.numeric(difftime(c(as.Date(df_ped$bdate[2:nrow(df_ped)], origin="1970-01-01"), NA), as.Date(df_ped$bdate, origin="1970-01-01"), unit="days"))/365.25, NA)

df_ped$preibi <- c(NA, df_ped$ibi[1:(nrow(df_ped)-1)])
return(list("df_ind" = qb.ind, "df_ped" = df_ped))
}
