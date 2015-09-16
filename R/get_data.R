#' Prepare data for pedigrees
#'
#' This function returns a list of 10 elements:
#' \enumerate{
#'   \item{df_kinder}{A dataframe with 80489 rows and 58 columns}
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
#' @param kindermc File path to  the 'kindermc.sav' file (SPSS).
#' @param familienmc File path to  the 'familienmc.sav' file  (SPSS).
#' @param geo File path to  the 'kh-geo.Rdata' file (R).
#' @keywords kh import
#' @export
#' @examples
#' twinmat <- get_twinmat()
#' str(twinmat)
get_data <- function(kindermc = "/home/johow/Desktop/KH33_Final/kindermc.sav",
                     familienmc = "/home/johow/Desktop/KH33_Final/familienmc.sav",
                     geo ="/home/johow/Dropbox/variance/data/kh-geo.Rdata"){
  t_start <- Sys.time()
kh <- get_kh(kindermc=kindermc, familienmc=familienmc)
kh[["relate"]] <- get_twinmat(kh[["df_ped"]], kh[["df_kinder"]])
kh[["evmat"]] <- get_evmat(kh[["df_ped"]], df_fam = kh[["df_fam"]])
kh[["df_ev"]] <- get_df_ev( kh[["df_ped"]], kh[["df_fam"]])
kh[["evmat_bak"]] <- get_evmat_bak(kh[["df_ped"]], df_fam = kh[["df_fam"]])

load(geo)
kh[["geo"]] <- geo

#
#  extract list components
#

#
#  lists to store labels for 'Kirchspiele' or 'special cases' (e.g. "G", "T", "B", "E")
#

kh[["evloc_list"]] <- pairlist(label = kh[["df_ped"]]$bnum[which(!duplicated(kh[["df_ped"]]$bplace))][order(kh[["df_ped"]]$bnum[which(!duplicated(kh[["df_ped"]]$bplace))])],
                                    name = paste(kh[["df_ped"]]$bplace[which(!duplicated(kh[["df_ped"]]$bplace))][order(kh[["df_ped"]]$bnum[which(!duplicated(kh[["df_ped"]]$bplace))])]))

kh[["evspc_list"]] <- pairlist(label = c(kh[["df_ped"]]$bspc[which(!duplicated(kh[["df_ped"]]$bspc))][order(kh[["df_ped"]]$bspc[which(!duplicated(kh[["df_ped"]]$bspc))])],
                                              kh[["df_ped"]]$dspc[which(!duplicated(kh[["df_ped"]]$dspc))][order(kh[["df_ped"]]$dspc[which(!duplicated(kh[["df_ped"]]$dspc))])]),
                                    name = paste(c(kh[["df_ped"]]$gebk[which(!duplicated(kh[["df_ped"]]$bspc))][order(kh[["df_ped"]]$bspc[which(!duplicated(kh[["df_ped"]]$bspc))])],
                                                   kh[["df_ped"]]$todk[which(!duplicated(kh[["df_ped"]]$dspc))][order(kh[["df_ped"]]$dspc[which(!duplicated(kh[["df_ped"]]$dspc))])])))


#
# define sample
#
cat(paste(length(kh), "objects:"), names(kh), sep="\n   -")
cat("Run time:\n    ", round(difftime(Sys.time(), t_start), 2), attr(difftime(Sys.time(), t_start), "unit"))
return(kh)
}
