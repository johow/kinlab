#' Generate random biographies of reproductive event
#'
#' This function returns a generated event matrix
#' @param n Number of desired cases
#' @param evmat_bak Should twin births appear as multiple events at one date (Default FALSE)
#' @param twinrate Default to p = 0.05 (i.e. 5 percent twins)
#' @keywords pedigree
#' @export
#' @examples
#' random_bio(5)
random_bio <- function(n, evmat_bak=FALSE, twinrate=0.05){
  nu_id <- 1
  pool_bdates <- as.numeric(seq(as.Date("1720-01-01"), as.Date("1830-01-01"),1))
  pool_nbirth <- c(rep(1,8),rep(2,4), rep(3, 9), rep(4, 11), rep(5, 14), rep(5, 17),  rep(6, 15),  rep(6, 14),
                   rep(7, 12), rep(8, 10), rep(9, 8), rep(10, 6), rep(11, 4), rep(12,  3), rep(13, 2), rep(14, 2), rep(15,  2), 16:18)
  if (evmat_bak==FALSE) {pool_ibidays <- c(seq(500, 1800, 10), seq(700, 1400, 5))}
  else {pool_ibidays <- c(seq(500, 1800, 10), seq(700, 1400, 5), rep(0, length(ceiling(twinrate*c(seq(500, 1800, 10), seq(700, 1400, 5))))))}
  pool_survdays <- c(seq(1, 20009, 1))
  pool_places <- c(1:33)
  evmat <- array(as.numeric(rep(NA, (20*5*n))), dim = c(n, 20, 5),
                 dimnames=list(id = 1:n, tt = c("*", paste(ifelse(c(1:18)>9, paste0("#",c(1:18)), paste0("#0",c(1:18)))), "+"), vv = c("evdat", "evid", "evloc", "evspc", "status")))

  evmat[,1,1] <- as.numeric(sample(pool_bdates, n)) + 15*366
  my_nbirths <- sample(pool_nbirth, n, replace=TRUE)
  for (i in 1:n){
    evmat[i,1,2] <- nu_id; nu_id <- nu_id+1
    evmat[i,1,3] <- sample(pool_places, 1)
    evmat[i,1,4] <- 0
    evmat[i,1,5] <- 1
  }
  for (i in 1:n){
    for (k in 2:(1+my_nbirths[i])){
      evmat[i,k,1] <- as.numeric(evmat[i,k-1,1]) + sample(pool_ibidays, 1)* my_nbirths[i]/(my_nbirths[i]*2)
      evmat[i,k,2] <- nu_id; nu_id <- nu_id+1
      evmat[i,k,3] <- sample(pool_places, 1)
      evmat[i,k,4] <- sample(c(0, 6), 1, prob=c(.95, .05))
      evmat[i,k,5] <- 1
    }
  }
  for (i in 1:n){
    for (k in (2+my_nbirths[i]):19){
      evmat[i,k,1] <- evmat[i,my_nbirths[i]+1,1]
      evmat[i,k,2] <- 0
      evmat[i,k,3] <- evmat[i,my_nbirths[i]+1,3]
      evmat[i,k,4] <- 0
      evmat[i,k,5] <- 0
    }
    evmat[i,20,1] <- as.numeric(evmat[i,19,1]) + sample(pool_survdays, 1)
    evmat[i,20,2] <- evmat[i,1,2]
    evmat[i,20,3] <- sample(pool_places, 1)
    evmat[i,20,4] <- 0
    evmat[i,20,5] <- 1
  }
  evmat[,1,1] <-  evmat[,1,1] -  15*366
  return(evmat)
}
