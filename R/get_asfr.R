#' Age-specific fertility rates
#'
#' This function computes age-specific fertility rates for a female, population being exposed (i.e. living years between age 15 and 45, see \href{http://www.un.org/esa/population/publications/WFD 2008/Metadata/ASFR.html}{United Nations, 2008}).
#' @param x The individual IDs of females of interests
#' @param df_ped A dataframe
#' @param evmat Event matrix (twins count as one)
#' @param evmat_bak Event matrix (including all twins)
#' @param include_twins Should twins count as multiple births? Default to FALSE
#' @param treshold_date Maximum date to include mothers with completed fertility. Default is "1830-01-01" for the Krummh\enc{ö}{oe}rn data.
#' @keywords kh pedigree
#' @export
#' @return A dataframe with 'age', 'years' (i.e. number of years, where females have been exposed), 'nbirth' (number of births, or children being born see \code{include_twins}), 'nwomen' (number of women being ever exposed), 'asfr' (resulting age-specific fertility rate in births/1000 women/year).
#' @examples
#' \dontrun{
#' x <- which(attr(evmat, "dimnames")$id %in% df_ped$id[!is.na(df_ped$bdate) &
#'            df_ped$sex==2] &  (evmat[,20,1]- evmat[,1,1])/365.25 >= 15)
#' get_asfr(x, df_ped, evmat, evmat_bak)
#' }
get_asfr <- function(x, df_ped, evmat, evmat_bak, include_twins=FALSE, treshold_date = as.Date("1830-01-01")) {
  .listFROM <- c(15, 20, 25, 30, 35, 40, 45)
  .listTO <- c(19, 24, 29, 34, 39, 44, 49)
  x <- df_ped$id[which(df_ped$id %in% x & df_ped$bdate < treshold_date)]
  if(include_twins==FALSE){
  .matAGE <- evmat[which(attr(evmat, "dimnames")$id %in% x),,]
  .matAGE[,,1] <- (.matAGE[,,1]- .matAGE[,1,1])/365.25
  }
  if(include_twins==TRUE){
    .matAGE <- evmat_bak[which(attr(evmat_bak, "dimnames")$id %in% x),,]
    .matAGE[,,1] <- (.matAGE[,,1]- .matAGE[,1,1])/365.25
  }
  .dfAFR <- data.frame(age = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
                       years = 0, nbirth = 0, nwomen = 0)
  for (i in 1:nrow(.dfAFR)){
    .dfAFR[i, "years"]    <- sum(as.numeric(ifelse(.matAGE[,20,1]>=(.listTO[i]+1), 5,
                                                   ifelse(.matAGE[,20,1]-.listFROM[i] >0,
                                                          (.matAGE[,20,1]-.listFROM[i]), 0))))

    .dfAFR[i, "nwomen"]    <- sum(as.numeric(ifelse(.matAGE[,20,1]>=(.listTO[i]+1), 1,
                                                    ifelse(.matAGE[,20,1]-.listFROM[i] >0,
                                                           1, 0))))
    for (k in 2:19){
      .dfAFR[i, "nbirth"] <- .dfAFR[i, "nbirth"] + sum(as.numeric(.matAGE[,k,1]<(.listTO[i]+1) &
                                                                    .matAGE[,k,1]>=.listFROM[i] &
                                                                    .matAGE[,k,5]==1))
    }
  }
  .dfAFR$asfr <- 1000*(.dfAFR$nbirth)/.dfAFR$years
  return(.dfAFR)
}
