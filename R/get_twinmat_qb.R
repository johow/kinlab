#' Wrapper function to return a matrix for twin relations
#'
#' This function returns a matrix describing twin relations in the form 'id1-id2-code', where code stands for 1=monozygotous, 2=dizygotous, 3=unknown. Here, zygosity is soley based on sex and therefore only twins of different sex are given code=2 (i.e. dizygotous), while same-sex twins are classified as unknown (code=3). In the case of triplets (and twins of higher order), each individual will have relations to each of his co-twins.
#' @param df_ped A dataframe containing the children's birth dates and and the ID of their parents
#' @param df_kinder A dataframe containing information on the type of certificate (required to exclude siblings having their baptism on the same day from being classified as twins)
#' @keywords pedigree
#' @export
#' @examples
#' twinmat <- get_twinmat()
#' str(twinmat)
get_twinmat_qb <- function(df_ped = qb[["df_ped"]]){
# df_ped[df_ped$idf %in% df_ped$idf[df_ped$ibi>20 & !is.na(df_ped$ibi)],c("id", "idf", "ibi", "bdate")]
df_twins <- df_ped[!is.na(df_ped$idf) & !is.na(df_ped$bdate) &
                     ((!is.na(df_ped$ibi) & df_ped$ibi %in% 0) |
                        (df_ped$preibi %in% 0 & !is.na(df_ped$preibi))),]

df_twins <- df_twins[order(df_twins$idf, df_twins$bdate),]
df_twins$id_twin <- as.numeric(factor(paste(df_twins$idf,  df_twins$bdate, sep="-")))
df_twins <- df_twins[order(df_twins$id_twin),]
#
# table(as.numeric(lapply(lapply(tapply(df_twins$idf, df_twins$id_twin, list), unique), length)))
# table(as.numeric(lapply(lapply(tapply(zero2na(df_twins$dadid), df_twins$id_twin, list), unique), length)))
# table(as.numeric(lapply(lapply(tapply(zero2na(df_twins$dadid), df_twins$id_twin, list), unique), length)))
# df_twins[which(lapply(lapply(tapply(zero2na(df_twins$dadid), df_twins$id_twin, list), unique), length)>1),]


twinlist <- tapply(df_twins$id, df_twins$id_twin, list)
table(as.numeric(lapply(twinlist, length)))

twintypelist  <-  ifelse(unlist(lapply(tapply(df_twins$sex, df_twins$id_twin, unique), length))==1, 3, 2)


twinlist2 <- twinlist[which( as.numeric(lapply(twinlist, length)) == 2)]
twinlist3 <- twinlist[which( as.numeric(lapply(twinlist, length)) == 3)]
twinlist4 <- twinlist[which( as.numeric(lapply(twinlist, length)) == 4)]



twinmat2 <- matrix(unlist(twinlist2), nrow= length(twinlist2),  byrow=TRUE,  dimnames =
                     list(1:length(twinlist2), c("id1", "id2")))

twinmat2 <- cbind(twinmat2, twintypelist[which( as.numeric(lapply(twinlist, length)) == 2)])
dimnames(twinmat2)[[2]][3] <- "code"


twinmat3 <- matrix(c(unlist(twinlist3)[rep(c(TRUE, TRUE, FALSE))], unlist(twinlist3)[rep(c(TRUE, FALSE, TRUE))],
                     unlist(twinlist3)[rep(c(FALSE, TRUE, TRUE))]),
                   nrow= length(twinlist3)*3,  byrow=TRUE,  dimnames =
                     list(1:(length(twinlist3)*3)+length(twinlist2), c("id1", "id2")))


twinmat3 <- cbind(twinmat3, "code" = NA)
twinmat3[rep(c(TRUE, FALSE, FALSE)), 3] <- twintypelist[which( as.numeric(lapply(twinlist, length)) == 3)]
twinmat3[rep(c(FALSE, TRUE, FALSE)), 3] <- twintypelist[which( as.numeric(lapply(twinlist, length)) == 3)]
twinmat3[rep(c(FALSE, FALSE, TRUE)), 3] <- twintypelist[which( as.numeric(lapply(twinlist, length)) == 3)]

twinmat <- rbind(twinmat2, twinmat3)


if (length(twinlist4)>0){
  twinmat4 <- matrix(c(unlist(twinlist4)[rep(c(TRUE, TRUE, FALSE, FALSE))],
                       unlist(twinlist4)[rep(c(TRUE, FALSE, TRUE, FALSE))],
                       unlist(twinlist4)[rep(c(TRUE, FALSE, FALSE, TRUE))],
                       unlist(twinlist4)[rep(c(FALSE, TRUE, TRUE, FALSE))],
                       unlist(twinlist4)[rep(c(FALSE, TRUE, FALSE, TRUE))],
                       unlist(twinlist4)[rep(c(FALSE, FALSE, TRUE, TRUE))]),
                     nrow= length(twinlist4)*6,  byrow=TRUE,  dimnames =
                       list(1:(length(twinlist4)*6)+(length(twinlist2)+length(twinlist3)), c("id1", "id2")))


  twinmat4 <- cbind(twinmat4, "code" = NA)
  twinmat4[rep(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)), 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 4)]
  twinmat4[rep(c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)), 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 4)]
  twinmat4[rep(c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)), 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 4)]
  twinmat4[rep(c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)), 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 4)]
  twinmat4[rep(c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)), 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 4)]
  twinmat4[rep(c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)), 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 4)]

  twinmat <- rbind(twinmat, twinmat4)
}

return(twinmat[twinmat[,1] %in% df_ped$id & twinmat[,2] %in% df_ped$id,])
}
