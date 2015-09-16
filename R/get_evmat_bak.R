#' A matrix for event data
#'
#' This function returns a matrix describing event data in the form date, id, location, special, status. Here, gemini births are included as multiple events (with each cotwin being born at the same date).
#' @param df_ped A dataframe containing genealogical information
#' @param df_fam A dataframe of the Krummh\enc{ö}{oe}rn family data
#' @keywords kh kin events
#' @export
get_evmat_bak <- function(df_ped = kh[["df_ped"]], df_fam = kh[["df_fam"]]){
  .tmp <- df_ped[!is.na(df_ped$bdate),c("id", "bdate", "bplace", "gebkk")]
  df_ev <- data.frame(id = .tmp$id, evdat = .tmp$bdate, evtyp = "*", evloc = .tmp$bplace, evid = .tmp$id, evspc = paste(.tmp$gebkk))
  .tmp <- df_ped[!is.na(df_ped$ddate),c("id", "ddate", "dplace", "todkk")]
  df_ev <- rbind(df_ev, data.frame(id = .tmp$id, evdat = .tmp$ddate, evtyp = "+", evloc = .tmp$dplace, evid = .tmp$id, evspc = paste(.tmp$todkk)))
  .tmp <- df_ped[!is.na(df_ped$bdate) & !is.na(df_ped$idf),c("idf", "bdate", "bplace", "id", "gebkk")]
  df_ev <- rbind(df_ev, data.frame(id = .tmp$idf, evdat = .tmp$bdate, evtyp = "#", evloc = .tmp$bplace, evid = .tmp$id, evspc = paste(.tmp$gebkk)))
  .tmp <- df_ped[!is.na(df_ped$bdate) & !is.na(df_ped$idm),c("idm", "bdate", "bplace", "id", "gebkk")]
  df_ev <- rbind(df_ev, data.frame(id = .tmp$idm, evdat = .tmp$bdate, evtyp = "#", evloc = .tmp$bplace, evid = .tmp$id, evspc = paste(.tmp$gebkk)))
  .tmp <- df_fam[!is.na(df_fam$dat4) & !is.na(df_fam$idm),c("idm","idf", "dat4", "cort", "fall")]
  levels(.tmp$cort) <- unlist(lapply(levels(.tmp$cort), get_kirchspiele))
  df_ev <- rbind(df_ev, data.frame(id = .tmp$idm, evdat = as.Date(as.POSIXct(.tmp$dat4, origin="1582-10-14")), evtyp = "oo",
                                   evloc = .tmp$cort, evid = .tmp$idf, evspc = .tmp$fall))
  .tmp <- df_fam[!is.na(df_fam$dat4) & !is.na(df_fam$idf),c("idm","idf", "dat4", "cort", "fall")]
  levels(.tmp$cort) <- unlist(lapply(levels(.tmp$cort), get_kirchspiele))
  df_ev <- rbind(df_ev, data.frame(id = .tmp$idf, evdat = as.Date(as.POSIXct(.tmp$dat4, origin="1582-10-14")), evtyp = "oo",
                                   evloc = .tmp$cort, evid = .tmp$idm, evspc = .tmp$fall))

  df_ev <- df_ev[order(df_ev$id, df_ev$evdat),]
  #df_ev$test <- factor(paste(df_ev$evtyp, df_ev$evdat, df_ev$evid, sep="."))
  df_ev[df_ev$id==df_ped$momid[df_ped$momid>0][1],]
  df_ev$tmp2 <- paste(df_ev$id, df_ev$evdat, df_ev$evtyp, df_ev$evid, sep="_")
  df_ev$tmp <- duplicated(df_ev$tmp2)
  df_ev <- df_ev[df_ev$id > 0 & df_ev$tmp == FALSE,1:6]

  #df_ev[df_ev$id==55255,]
  #tapply(df_ev$evspc, df_ev$evtyp, table)
  df_ev$evspc <- paste(df_ev$evspc)
  df_ev$evspc[df_ev$evspc=="NA"] <- ""
  df_ev$evspc <- factor(df_ev$evspc)
  df_ev <- subset(df_ev, !(evtyp == "+" & evspc == "E"))
  # tapply(df_ev$evspc, df_ev$evtyp, table)

  df_ped <- df_ped[order(df_ped$id),]

  evmat <- array(as.numeric(rep(NA, (20*5*length(df_ped$id)))), dim = c(length(df_ped$id), 20, 5),
                 dimnames=list(id = paste(df_ped$id), tt = c("*", paste(ifelse(c(1:18)>9, paste0("#",c(1:18)), paste0("#0",c(1:18)))), "+"), vv = c("evdat", "evid", "evloc", "evspc", "status")))
  # str(evmat)


  df_ev <- df_ev[order(df_ev$id),]

  evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")]),1,1] <- df_ev$evdat[which(df_ev$evtyp == "*")]
  evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")]),20,1] <- df_ev$evdat[which(df_ev$evtyp == "+")]

  evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")]),1,3] <- df_ev$evloc[which(df_ev$evtyp == "*")]
  evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")]),20,3] <- df_ev$evloc[which(df_ev$evtyp == "+")]

  evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")]),1,4] <- df_ev$evspc[which(df_ev$evtyp == "*")]
  evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")]),20,4] <- df_ev$evspc[which(df_ev$evtyp == "+")]


  evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")]),1,2] <- as.numeric(paste(unlist(attr(evmat, "dimnames")[1])))[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")])]
  evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")]),20,2] <- as.numeric(paste(unlist(attr(evmat, "dimnames")[1])))[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")])]

  df_ped$gebk[df_ped$gebk=="NA"]<- ""
  df_ped$todk[df_ped$todk=="NA"]<- ""

  df_ped$bspc <- as.numeric(factor(df_ped$gebk))
  df_ped$dspc <- as.numeric(factor(df_ped$todk)) + max(df_ped$bspc)
  df_ped$dspc[df_ped$dspc==7]<-2
  df_ped$dspc[df_ped$dspc==5]<-1
  df_ped$dspc[df_ped$dspc==6]<-5
  evspc.list <- list(unique(c(levels(factor(df_ped$gebk)), levels(factor(df_ped$todk)))), 1:max(df_ped$dspc))

  df_ped <- df_ped[order(df_ped$idf, df_ped$bdate),]
  bdates_old <-    tapply(as.numeric(df_ped$bdate), df_ped$idf, list)[-1]

  index_log <- lapply(lapply(bdates_old, duplicated),"!")
  index_bdates <- lapply(lapply(lapply(bdates_old, duplicated),"!"), which)
  bplaces_old <- tapply(df_ped$bnum, df_ped$idf, list)[-1]


  id_old <- tapply(df_ped$id, df_ped$idf, list)[-1]

  bspc_old <- tapply(df_ped$bspc, df_ped$idf, list)[-1]


  df_ped <- df_ped[order(df_ped$id),]

evmat <- array(as.numeric(rep(NA, (20*5*length(df_ped$id)))), dim = c(length(df_ped$id), 20, 5),
               dimnames=list(id = paste(df_ped$id), tt = c("*", paste(ifelse(c(1:18)>9, paste0("#",c(1:18)), paste0("#0",c(1:18)))), "+"), vv = c("evdat", "evid", "evloc", "evspc", "status")))
# str(evmat)


df_ev <- df_ev[order(df_ev$id),]

evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")]),1,1] <- df_ev$evdat[which(df_ev$evtyp == "*")]
evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")]),20,1] <- df_ev$evdat[which(df_ev$evtyp == "+")]

evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")]),1,3] <- df_ev$evloc[which(df_ev$evtyp == "*")]
evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")]),20,3] <- df_ev$evloc[which(df_ev$evtyp == "+")]

evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")]),1,4] <- df_ev$evspc[which(df_ev$evtyp == "*")]
evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")]),20,4] <- df_ev$evspc[which(df_ev$evtyp == "+")]


evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")]),1,2] <- as.numeric(paste(unlist(attr(evmat, "dimnames")[1])))[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")])]
evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")]),20,2] <- as.numeric(paste(unlist(attr(evmat, "dimnames")[1])))[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")])]



for (i in 1:length(bdates_old)){
  evmat[names(bdates_old)[i],1+c(1:length(bdates_old[[i]])),1] <- bdates_old[[i]]
}
for (i in 1:length(id_old)){
  evmat[names(id_old)[i],1+c(1:length(id_old[[i]])),2] <- id_old[[i]]
}

for (i in 1:length(bplaces_old)){
  evmat[names(bplaces_old)[i],1+c(1:length(bplaces_old[[i]])),3] <- bplaces_old[[i]]
}
for (i in 1:length(bspc_old)){
  evmat[names(bspc_old)[i],1+c(1:length(bspc_old[[i]])),4] <- bspc_old[[i]]
}
evmat[,,5] <- ifelse(is.na(evmat[,,1]), 0, 1)


evmat <- evmat[as.numeric(which(!is.na(evmat[,1,1]))),,]
evmat <- evmat[as.numeric(which(apply(evmat[,,5], 1, sum, na.rm=TRUE)>1)),,]

for (i in 2:20){
if (any(is.na(evmat[,i,1]))){
  evmat[which(is.na(evmat[,i,1])),i,1] <- as.numeric(apply(evmat[which(is.na(evmat[,i,1])),,1], 1, max, na.rm=TRUE))
}
}

evmat[which(as.numeric(apply(evmat[,,1], 1, max, na.rm=TRUE)) > evmat[,20,1]),,]
evmat[which(as.numeric(apply(evmat[,,1], 1, max, na.rm=TRUE)) > evmat[,20,1]),20,5] <- 0

evmat[which(as.numeric(apply(evmat[,,1], 1, max, na.rm=TRUE)) > evmat[,20,1]),20,1] <-
  as.numeric(apply(evmat[which(as.numeric(apply(evmat[,,1], 1, max, na.rm=TRUE)) > evmat[,20,1]),,1], 1, max, na.rm=TRUE))
evmat[which(as.numeric(apply(evmat[,1:19,1], 1, max, na.rm=TRUE)) >= evmat[,20,1]),20,1] <-
  as.numeric(apply(evmat[which(as.numeric(apply(evmat[,1:19,1], 1, max, na.rm=TRUE)) >= evmat[,20,1]),1:19,1], 1, max, na.rm=TRUE))+1


evmat[is.na(evmat)] <- 0
for (i in 2:19){
  evmat[,i,3][evmat[,i,3]==0]  <- ifelse(evmat[,i,1][evmat[,i,3]==0] == evmat[,20,1][evmat[,i,3]==0], evmat[,20,3],
                                         evmat[,i-1,3][evmat[,i,3]==0])
}
return(evmat)
}
