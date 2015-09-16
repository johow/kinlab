#' A matrix for event data in Qu\enc{é}{e}bec (including each twin as single event)
#'
#' This function returns a matrix describing event data in the form date, id, location, special, status. Here, gemini births count as one single event (see get_evmat_twins() for including the birth of twins as multiple events).
#' @param df_ped A dataframe containing genealogical information
#' @keywords qb kin events
#' @export
get_evmat_bak_qb <- function(df_ped = qb[["df_ped"]]){

  evmat <- array(as.numeric(rep(NA, (2+max_parity)*5*length(df_ped$id))),
               dim = c(length(df_ped$id),sum(max_parity,2), 5),
               dimnames=list(id = paste(df_ped$id), tt = c("*", paste(ifelse(c(1:(max_parity))>9,
                                                                             paste0("#",c(1:( max_parity))),
                                                                             paste0("#0",c(1:( max_parity))))), "+"), vv = c("evdat", "evid", "evloc", "evspc", "status")))
# str(evmat)

evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")]),1,1] <- df_ev$evdat[which(df_ev$evtyp == "*")]
evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")]), max(table(df_ev$id)),1] <- df_ev$evdat[which(df_ev$evtyp == "+")]
# str(evmat)

evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")]),1,3] <- df_ev$evloc[which(df_ev$evtyp == "*")]
evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")]), max(table(df_ev$id)),3] <- df_ev$evloc[which(df_ev$evtyp == "+")]
# str(evmat)

evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")]),1,4] <- df_ev$evspc[which(df_ev$evtyp == "*")]
evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")]), max(table(df_ev$id)),4] <- df_ev$evspc[which(df_ev$evtyp == "+")]


evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")]),1,2] <- as.numeric(paste(unlist(attr(evmat, "dimnames")[1])))[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "*")])]
evmat[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")]), max(table(df_ev$id)),2] <- as.numeric(paste(unlist(attr(evmat, "dimnames")[1])))[which(as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) %in% df_ev$id[which(df_ev$evtyp == "+")])]

tmp <- c("bdate", "id", "bplace", "xdateb")

for (k in 1:4){
  for (i in 1:max(df_ped$pat_parity)){
    evmat[, sum(i,1),k] <-  ifelse(as.numeric(paste(unlist(attr(evmat, "dimnames")[1])))  %in%
                                     df_ped[df_ped$dadid %in% as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) & df_ped$pat_parity==i,"dadid"],
                                   as.numeric(df_ped[df_ped$dadid %in% as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) & df_ped$pat_parity==i, paste(tmp[k])]),     evmat[, sum(i,1),k])
    if (max(df_ped$mat_parity) >= i){
      evmat[, sum(i,1),k] <-
        ifelse(as.numeric(paste(unlist(attr(evmat, "dimnames")[1])))  %in%
                 df_ped[df_ped$momid %in% as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) & df_ped$mat_parity==i,"momid"],
               as.numeric(df_ped[df_ped$momid %in% as.numeric(paste(unlist(attr(evmat, "dimnames")[1]))) & df_ped$mat_parity==i, paste(tmp[k])]),     evmat[, sum(i,1),k])
    }
  }
}

df_ped <- df_ped[order(df_ped$id),]
evmat[,,5] <- ifelse(is.na(evmat[,,1]), 0, 1)
# save(evmat, file="evmat.Rdata")
#
evmat <- evmat[which(!is.na(evmat[,1,1])),,]
evmat <- evmat[which(apply(evmat[,,5], 1, sum, na.rm=TRUE)>1),,]
#
# any dates after death?
length(which(as.numeric(apply(evmat[,,1], 1, max, na.rm=TRUE)) > evmat[,dim(evmat)[2],1]))
# evmat[which(as.numeric(apply(evmat[,,1], 1, max, na.rm=TRUE)) > evmat[,dim(evmat)[2],1]),,]
# evmat[which(as.numeric(apply(evmat[,,1], 1, max, na.rm=TRUE)) > evmat[,dim(evmat)[2],1]),dim(evmat)[2],5] <- 0
# evmat[which(as.numeric(apply(evmat[,,1], 1, max, na.rm=TRUE)) > evmat[,dim(evmat)[2],1]),dim(evmat)[2],1] <-
#   as.numeric(apply(evmat[which(as.numeric(apply(evmat[,,1], 1, max, na.rm=TRUE)) > evmat[, dim(evmat)[2],1]),,1], 1, max, na.rm=TRUE))
# evmat[which(as.numeric(apply(evmat[,1:(dim(evmat)[2]-1),1], 1, max, na.rm=TRUE)) >= evmat[,dim(evmat)[2],1]),dim(evmat)[2],1] <-
#  as.numeric(apply(evmat[which(as.numeric(apply(evmat[,1:19,1], 1, max, na.rm=TRUE)) >= evmat[,dim(evmat)[2],1]),1:(dim(evmat)[2]-1),1], 1, max, na.rm=TRUE))+1


evmat_bak <- evmat

evmat_bak[,,4] <- ifelse(evmat_bak[,,2] %in% relate[,1:2], 6, 99)
return(evmat_bak)
}
