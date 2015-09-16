#' Average age-specific residual reproductive value
#'
#' This function computes residual reproductive values based on estimated survival probabilites and age-specific fertiliy rates (see \href{http://en.wikipedia.org/wiki/Life_history_theory#Reproductive_value_and_costs_of_reproduction}{Wikipedia, 2015}).
#' @param id The individual IDs of females of interests
#' @param age Age for which the residual reproductive value should be computed.
#' @param  df_ped A dataframe for genealogical relations providing the variables 'id', 'momid' and 'dadid'
#' @param  evmat Event matrix
#' @param  evmat_bak An event matrix
#' @keywords kh pedigree
#' @return The average number of offspring, which can be expected for individuals in \code{id} at age \code{age}.
#' @export
#' @examples
#' \dontrun{
#' get_resid_rv(sample(df_ped$momid[df_ped$momid>0], 100), df_ped, age=30)
#' }
get_resid_rv <- function(id, df_ped, evmat, evmat_bak, age = NULL){
  if (length(id)>0){
  cox_survival_f_sample <- survival::coxph(survival::Surv(
    ifelse(!is.na(as.numeric(difftime(df_ped$ddate[df_ped$id %in% id],
                                      df_ped$bdate[df_ped$id %in% id], unit="days"))/365.25),
           as.numeric(difftime(df_ped$ddate[df_ped$id %in% id],
                               df_ped$bdate[df_ped$id %in% id], unit="days"))/365.25, 15),
    ifelse(!is.na(as.numeric(difftime(df_ped$ddate[df_ped$id %in% id],
                                      df_ped$bdate[df_ped$id %in% id], unit="days"))/365.25),
           1, 0)) ~ 1)

  tmp_sisters <- na.omit(unlist(lapply(id, get_sisters, df_ped)))
  cox_survival_f <- survival::coxph(survival::Surv(
    ifelse(!is.na(as.numeric(difftime(df_ped$ddate[df_ped$id %in% tmp_sisters],
                                      df_ped$bdate[df_ped$id %in% tmp_sisters], unit="days"))/365.25),
           as.numeric(difftime(df_ped$ddate[df_ped$id %in% tmp_sisters],
                               df_ped$bdate[df_ped$id %in% tmp_sisters], unit="days"))/365.25, 15),
    ifelse(!is.na(as.numeric(difftime(df_ped$ddate[df_ped$id %in% tmp_sisters],
                                      df_ped$bdate[df_ped$id %in% tmp_sisters], unit="days"))/365.25),
           1, 0)) ~ 1)
  df_asfr <- get_asfr(id, df_ped, evmat, evmat_bak)
  df_life <- df_asfr
  df_life$age1 <- as.numeric(substr(df_life$age, 1,2))
  df_life$age2 <- df_life$age1 + 4
  for (i in nrow(df_life)){
    df_life$life.exp1[i] <- survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time>df_life$age1[i]][1]
    df_life$life.exp2[i] <- survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time>df_life$age2[i]][1]
  }
  df_life$life.exp1[1] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time>15][1]
  df_life$life.exp2[1] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time ==
                                                               max(survival::basehaz(cox_survival_f)$time[survival::basehaz(cox_survival_f)$time<20])][1]

  df_life$life.exp1[2] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time>20][1]
  df_life$life.exp2[2] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time ==
                                                               max(survival::basehaz(cox_survival_f)$time[survival::basehaz(cox_survival_f)$time<25])][1]


  df_life$life.exp1[3] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time>25][1]
  df_life$life.exp2[3] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time ==
                                                               max(survival::basehaz(cox_survival_f)$time[survival::basehaz(cox_survival_f)$time<30])][1]

  df_life$life.exp1[4] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time>30][1]
  df_life$life.exp2[4] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time ==
                                                               max(survival::basehaz(cox_survival_f)$time[survival::basehaz(cox_survival_f)$time<35])][1]

  df_life$life.exp1[5] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time>35][1]
  df_life$life.exp2[5] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time ==
                                                               max(survival::basehaz(cox_survival_f)$time[survival::basehaz(cox_survival_f)$time<40])][1]

  df_life$life.exp1[6] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time>40][1]
  df_life$life.exp2[6] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time ==
                                                               max(survival::basehaz(cox_survival_f)$time[survival::basehaz(cox_survival_f)$time<45])][1]

  df_life$life.exp1[7] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time>45][1]
  df_life$life.exp2[7] <- 1 - survival::basehaz(cox_survival_f)$hazard[survival::basehaz(cox_survival_f)$time ==
                                                               max(survival::basehaz(cox_survival_f)$time[survival::basehaz(cox_survival_f)$time<50])][1]


  df_asfr$life.exp <- (df_life$life.exp1 + df_life$life.exp2)/2


  df_asfr$cum.births <- cumsum(df_asfr$nbirth/df_asfr$nwomen)

  basehaz_f <- data.frame(time=survival::survfit(cox_survival_f)$time, surv=survival::survfit(cox_survival_f)$surv)
  basehaz_f_sample <- data.frame(time=survival::survfit(cox_survival_f_sample)$time, surv=survival::survfit(cox_survival_f_sample)$surv)
  basehaz_f_sample$surv <- basehaz_f_sample$surv*min(basehaz_f[basehaz_f$time<15,"surv"])
  basehaz_f <- rbind(basehaz_f[basehaz_f$time<min(survival::survfit(cox_survival_f_sample)$time),],basehaz_f_sample)
  basehaz_f$rest.births[basehaz_f$time <15] <- sum(df_asfr$nbirth/df_asfr$nwomen)
  basehaz_f$rest.births[basehaz_f$time>=15 & basehaz_f$time <20] <- sum(df_asfr$nbirth/df_asfr$nwomen)-df_asfr$nbirth[1]/df_asfr$nwomen[1]
  basehaz_f$rest.births[basehaz_f$time>=20 & basehaz_f$time <25] <- sum(df_asfr$nbirth/df_asfr$nwomen)-df_asfr$nbirth[2]/df_asfr$nwomen[2]
  basehaz_f$rest.births[basehaz_f$time>=25 & basehaz_f$time <30] <- sum(df_asfr$nbirth/df_asfr$nwomen)-df_asfr$nbirth[3]/df_asfr$nwomen[3]
  basehaz_f$rest.births[basehaz_f$time>=30 & basehaz_f$time <35] <- sum(df_asfr$nbirth/df_asfr$nwomen)-df_asfr$nbirth[4]/df_asfr$nwomen[4]
  basehaz_f$rest.births[basehaz_f$time>=35 & basehaz_f$time <40] <- sum(df_asfr$nbirth/df_asfr$nwomen)-df_asfr$nbirth[5]/df_asfr$nwomen[5]
  basehaz_f$rest.births[basehaz_f$time>=30 & basehaz_f$time <45] <- sum(df_asfr$nbirth/df_asfr$nwomen)-df_asfr$nbirth[6]/df_asfr$nwomen[6]
  basehaz_f$rest.births[basehaz_f$time>=45 & basehaz_f$time <50] <- sum(df_asfr$nbirth/df_asfr$nwomen)-df_asfr$nbirth[7]/df_asfr$nwomen[7]
  basehaz_f$rest.births[basehaz_f$time >=50] <-0

  basehaz_f$surv15 <-
    ifelse(basehaz_f$time < 15, basehaz_f$surv[basehaz_f$time>15][1] +
             1 - basehaz_f$surv, 1)


  basehaz_f$surv20<-
    ifelse(basehaz_f$time < 20, basehaz_f$surv[basehaz_f$time>20][1] +
             1 - basehaz_f$surv, 1)


  basehaz_f$surv25 <-
    ifelse(basehaz_f$time < 25, basehaz_f$surv[basehaz_f$time>25][1] +
             1 - basehaz_f$surv, 1)


  basehaz_f$surv30 <-
    ifelse(basehaz_f$time < 30, basehaz_f$surv[basehaz_f$time>30][1] +
             1 - basehaz_f$surv, 1)


  basehaz_f$surv35 <-
    ifelse(basehaz_f$time < 35, basehaz_f$surv[basehaz_f$time>35][1] +
             1 - basehaz_f$surv, 1)


  basehaz_f$surv40 <-
    ifelse(basehaz_f$time < 40, basehaz_f$surv[basehaz_f$time>40][1] +
             1 - basehaz_f$surv, 1)


  basehaz_f$surv45 <-
    ifelse(basehaz_f$time < 45, basehaz_f$surv[basehaz_f$time>45][1] +
             1 - basehaz_f$surv, 1)


  basehaz_f$surv50 <-
    ifelse(basehaz_f$time < 50, basehaz_f$surv[basehaz_f$time>50][1] +
             1 - basehaz_f$surv, 1)

  basehaz_f$surv[basehaz_f$time>15][1]

  basehaz_f$nbirths.15 <- ifelse(basehaz_f$time < 20,
                                 (ifelse(basehaz_f$time>15, 20-basehaz_f$time, 5))/5 * df_asfr$nbirth[1]/df_asfr$nwomen[1] *
                                   basehaz_f$surv15, 0)

  basehaz_f$nbirths.20 <- ifelse(basehaz_f$time < 25,
                                 (ifelse(basehaz_f$time>20, 25-basehaz_f$time, 5))/5 * df_asfr$nbirth[2]/df_asfr$nwomen[2] *
                                   basehaz_f$surv15, 0)


  basehaz_f$nbirths.25 <- ifelse(basehaz_f$time < 30,
                                 (ifelse(basehaz_f$time>25, 30-basehaz_f$time, 5))/5 * df_asfr$nbirth[3]/df_asfr$nwomen[3] *
                                   basehaz_f$surv15, 0)


  basehaz_f$nbirths.30 <- ifelse(basehaz_f$time < 35,
                                 (ifelse(basehaz_f$time>30, 35-basehaz_f$time, 5))/5 * df_asfr$nbirth[4]/df_asfr$nwomen[4] *
                                   basehaz_f$surv15, 0)


  basehaz_f$nbirths.35 <- ifelse(basehaz_f$time < 40,
                                 (ifelse(basehaz_f$time>35, 40-basehaz_f$time, 5))/5 * df_asfr$nbirth[5]/df_asfr$nwomen[5] *
                                   basehaz_f$surv15, 0)

  basehaz_f$nbirths.40 <- ifelse(basehaz_f$time < 45,
                                 (ifelse(basehaz_f$time>40, 45-basehaz_f$time, 5))/5 * df_asfr$nbirth[6]/df_asfr$nwomen[6] *
                                   basehaz_f$surv15, 0)


  basehaz_f$nbirths.45 <- ifelse(basehaz_f$time < 50,
                                 (ifelse(basehaz_f$time>45, 50-basehaz_f$time, 5))/5 * df_asfr$nbirth[7]/df_asfr$nwomen[7] *
                                   basehaz_f$surv15, 0)

  basehaz_f$resid.rv <-apply(basehaz_f[,c("nbirths.15", "nbirths.20",  "nbirths.25",  "nbirths.30",  "nbirths.35",  "nbirths.40",  "nbirths.45")], 1, sum)
  basehaz_f$n.surv <-apply(basehaz_f[,c("surv15", "surv20",  "surv25",  "surv30",  "surv35",  "surv40",  "surv45")], 1, sum)
  }else{basehaz_f <- 0}
  if (is.null(age)){
  return(basehaz_f)} else return(basehaz_f$resid.rv[basehaz_f$time==max(basehaz_f$time[basehaz_f$time<=age])])
}


