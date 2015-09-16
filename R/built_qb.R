#' Prepare data for pedigrees in Qu\enc{é}{e}bec
#'
#' This function returns a list of 10 elements:
#' \enumerate{
#'   \item{df_ind}{A dataframe with 80489 rows and 58 columns}
#'   \item{df_fam}{A dataframe with 34711 rows and 178 columns}
#'  \item{df_ped}{TBD}
#'  \item{relate}{TBD}
#'  \item{evmat}{TBD}
#'  \item{df_ev}{TBD}
#'  \item{evmat_bak}{TBD}
#'  \item{geo}{TBD}
#'  \item{evloc_list}{TBD}
#'  \item{evspc_list}{TBD}
#' }
#' @param Individus File path to  the 'Individus.sav' file (SPSS).
#' @param geo File path to  the 'geo.Rdata' file (R) containing geo data.
#' @param location_list File path to  the 'qb_places.txt' file (txt).
#' @keywords kh import
#' @export
#' @examples
#' twinmat <- get_twinmat()
#' str(twinmat)
built_qb <- function(Individus = "/home/johow/Dropbox/qb/RPQA.MarcKlemp.individus.2012-01-27.sav",
                     geo = "/home/johow/Dropbox/qb/geo.Rdata",
                     location_list = "/home/johow/Dropbox/qb/qb_places.txt"){
  t_start <- Sys.time()

  qb <- get_qb(Individus=Individus)
  qb[["relate"]] <- get_twinmat()
  qb[["df_ev"]] <- get_df_ev()
  qb[["evmat"]] <- get_evmat()
  qb[["evmat_bak"]] <- get_evmat_bak()

  load(geo)
  qb[["geo"]] <- geo[["qb"]]
  qb[["geo"]][["map_dist"]] <- geosphere::distm( qb[["geo"]][["df"]][,3:4],  qb[["geo"]][["df"]][,3:4], fun=geosphere::distHaversine)/1000
  qb[["geo"]][["locations"]] <- read.table(location_list)


  #
  #  extract list components
  #

  #
  #  lists to store labels for 'Kirchspiele' or 'special cases' (e.g. "G", "T", "B", "E")
  #
#
#   qb[["evloc_list"]] <- pairlist(label = kh[["df_ped"]]$bnum[which(!duplicated(kh[["df_ped"]]$bplace))][order(kh[["df_ped"]]$bnum[which(!duplicated(kh[["df_ped"]]$bplace))])],
#                                  name = paste(kh[["df_ped"]]$bplace[which(!duplicated(kh[["df_ped"]]$bplace))][order(kh[["df_ped"]]$bnum[which(!duplicated(kh[["df_ped"]]$bplace))])]))
#
#   qb[["evspc_list"]] <- pairlist(label = c(kh[["df_ped"]]$bspc[which(!duplicated(kh[["df_ped"]]$bspc))][order(kh[["df_ped"]]$bspc[which(!duplicated(kh[["df_ped"]]$bspc))])],
#                                            kh[["df_ped"]]$dspc[which(!duplicated(kh[["df_ped"]]$dspc))][order(kh[["df_ped"]]$dspc[which(!duplicated(kh[["df_ped"]]$dspc))])]),
#                                  name = paste(c(kh[["df_ped"]]$gebk[which(!duplicated(kh[["df_ped"]]$bspc))][order(kh[["df_ped"]]$bspc[which(!duplicated(kh[["df_ped"]]$bspc))])],
#                                                 kh[["df_ped"]]$todk[which(!duplicated(kh[["df_ped"]]$dspc))][order(kh[["df_ped"]]$dspc[which(!duplicated(kh[["df_ped"]]$dspc))])])))


  message(paste("successfully created", length(qb), "objects:\n   -"), paste(names(qb)[-length(names(qb))], "\n   -"), paste(names(qb)[length(names(qb))]))
  message("Run time:\n    ", round(difftime(Sys.time(), t_start), 2), attr(difftime(Sys.time(), t_start), "unit"))
  return(qb)
}




