#' A dataframe describing the kin network for a given ID at a given date.
#'
#' This function returns a dataframe describing kin networks at specific dates documented for one or more given IDs
#' @param id ID of individual for which specific kin network should be compiled
#' @param evdat A specific date of interest
#' @param df_ped A dataframe for genealogical relations providing the variables 'id', 'momid' and 'dadid'
#' @param map_dist A distance matrix, which contains locations of individuals
#' @param kin.only If FALSE, also kin being genetically unrelated both to the given ID and his or her spouse(s) will be included (Default is TRUE)
#' @keywords kh kin events
#' @export
#' @examples
#' get_kindata(1067, "1801-01-01")
get_kindata <- function(id, evdat, df_ped = kh[["df_ped"]], map_dist = kh[["geo"]][["map_dist"]], kin.only = TRUE){
 if (!id %in% as.numeric(names(kh[["ped"]]))){
   ped <- grap_ped(id)} else {ped <- kh[["ped"]][[paste(id)]]}
  df_out <- subset(merge(data.frame(id = id, kin.id = get_kinev(id, ped), evdat = as.Date(paste(evdat))), data.frame(kin.id = df_ped$id,
                                                                                                    kin.bdate=df_ped$bdate,
                                                                                                    kin.ddate = df_ped$ddate,
                                                                                                    kin.sex = df_ped$sex), by = "kin.id"),
                   difftime(evdat, kin.bdate, units="days")/365.25>=8 &
                   ((is.na(kin.ddate) & difftime(evdat, kin.bdate, units="days")/365.25<15)| kin.ddate > evdat))
 df_out$relmom <- kinship2::kinship(ped)[paste(id), paste(df_out$kin.id)]
 spouse_tmp <- get_spouses(id)
 spouse_tmp <- spouse_tmp[spouse_tmp %in% ped$id]
 if(length(spouse_tmp)>1){
   reltmp <- as.data.frame(list(kinship2::kinship(ped)[
     paste(spouse_tmp[1]),
     paste(df_out$kin.id)]))
   for (i in 2:length(spouse_tmp)){
     reltmp <- cbind(reltmp, as.data.frame(list(kinship2::kinship(ped)[
       paste(spouse_tmp[i]),
       paste(df_out$kin.id)])))
   }
   df_out$reldad <- as.numeric(apply(reltmp, 1, max))
 } else df_out$reldad <- kinship2::kinship(ped)[
   paste(spouse_tmp),
   paste(df_out$kin.id)]
 df_out$relmomx <- kinship2::kinship(ped, chrtype="X")[paste(id), paste(df_out$kin.id)]
 spouse_tmp <- get_spouses(id)
 spouse_tmp <- spouse_tmp[spouse_tmp %in% ped$id]
 if(length(spouse_tmp)>1){
   reltmp <- as.data.frame(list(kinship2::kinship(ped, chrtype="X")[
     paste(spouse_tmp[1]),
     paste(df_out$kin.id)]))
   for (i in 2:length(spouse_tmp)){
     reltmp <- cbind(reltmp, as.data.frame(list(kinship2::kinship(ped, chrtype="X")[
       paste(spouse_tmp[i]),
       paste(df_out$kin.id)])))
   }
   df_out$reldadx <- as.numeric(apply(reltmp, 1, max))
 } else df_out$reldadx <- kinship2::kinship(ped, chrtype="X")[
   paste(spouse_tmp),
   paste(df_out$kin.id)]
 if (kin.only == TRUE){
   df_out <- subset(df_out, relmom > 0 | reldad >0)
 }
 df_out$kin.age <- round(as.numeric(difftime(df_out$evdat, df_out$kin.bdate, units="days")/365.25), 2)
 df_out$kin.loc <- unlist(mapply("get_kinloc", df_out$kin.id, paste(df_out$evdat)))
 df_out$loc <-get_kinloc(df_out$id[1], paste(df_out$evdat[1]))
 df_out$kin.dist <- round(as.numeric(map_dist[df_out$kin.loc,df_out$loc[1]]), 2)
 df_out$kin.loc <- recode_evloc(df_out$kin.loc)
 df_out$loc <- recode_evloc(df_out$loc)
 df_out$kin.kids8 <- 0
 df_out[which(df_out$kin.id %in% c(df_ped$momid, df_ped$dadid)), "kin.kids8"] <- count_kids8(df_out[which(df_out$kin.id %in% c(df_ped$momid, df_ped$dadid)), "kin.id"],
                                                                                        df_out[which(df_out$kin.id %in% c(df_ped$momid, df_ped$dadid)), "evdat"])
 for (i in c(paste0(c("relmom", "reldad"), c("","", "x", "x")))){
  df_out[,paste(i)]<- round(df_out[,paste(i)], 3)
}
 df_out <- df_out[order(df_out$kin.dist, df_out$kin.age),]
return(df_out[!is.na(df_out$kin.dist),c("id", "evdat", "loc", "kin.id", "relmom", "reldad", "relmomx", "reldadx", "kin.loc", "kin.sex", "kin.age", "kin.dist", "kin.kids8")])
}
