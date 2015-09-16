#' Prepare multi-state data
#' 
#' This function calls 'mstate::msprep()' with the appropriate argument for the transition matrix on an array of events (as returned by 'get_evmat()') by calling 'as.data.frame()'   
#' returns a 'msdata' object (consisting of a table in long format and the coresponding transistion matrix)     
#' @param id_sample A sample of IDs (will be sample from, if length(id_sample)> max_n)
#' @param evmat An event matrix
#' @param df_vars Optional dataframe containing predictor variables
#' @param min_n Minimum number of cases per transtition to state under observation (Default to 16)
#' @param max_n Maximum number of total cases (Default to 1000)
#' @keywords spatial distance pedigree
#' @export
#' @examples
#' \dontrun{
#' df_ind <- get_exmpl_df()
#' df_fam <- data.frame(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
#' evmat <- get_evmat(df_ind, df_fam)
#' ms1 <- built_ms(evmat)
#' }
built_ms <- function(id_sample = NULL, evmat = NULL, df_vars = NULL, min_n = 16, max_n = 1000){
  if (length(id_sample) > max_n) {id_sample <- sample(id_sample,  max_n)}
  df_t <- as.data.frame(evmat[paste(id_sample),,1])
  s_max <- ncol(df_t)
  names(df_t) <- paste0("date.", c("birth", 1:(ncol(df_t)-2), "death"))
  for (i in 2:s_max){
    df_t$tmp <- (df_t[,i]-df_t[,1])/365.25
    names(df_t)[names(df_t)=="tmp"] <- paste0("age.b", i-1)
  }
  names(df_t)[names(df_t)==paste0("age.b", i-1)] <- "age.death"
  tmp <-  as.data.frame(evmat[paste(id_sample),,5])
  colnames(tmp) <- paste0("status.", c("birth", 1:(s_max-2), "death"))
  df_t <-   cbind(df_t, tmp)

  b_max <- max(which(as.numeric(apply(df_t[,paste0("status.", 1:(s_max-2))], 2, sum))>min_n))


  df_t$id <-rownames(df_t)

  tmat <- matrix(rep(NA, (b_max+2)*(b_max+2)), nrow = b_max+2, ncol = b_max+2, dimnames = list(
    "from" = c(NA, paste0("#",
                          ifelse(1:b_max>9, "", "0"), 1:b_max), "+"),
    "to" = c(NA, paste0("#",
                        ifelse(1:b_max>9, "", "0"), 1:b_max), "+")))

  k <- 1
  for (i in 1:(b_max+1)){
    if (i < (b_max+1)){
      tmat[i,i+1] <- k; k <- k+1
    }
    tmat[i,b_max+2] <- k; k <- k+1
  }

if (!is.null(df_vars)){
  df_t <- merge(df_t, df_vars, by = "id")
  keep_vars <- names(df_vars)[names(df_vars)!="id"]
  df_t$id_old <- df_t$id

  ms_kh <- suppressWarnings(mstate::msprep(
    time=c(NA, paste0("age.b", 1:b_max), "age.death"),
    status =c(NA, paste0("status.", 1:b_max), "status.death"),
    df_t,
    tmat,
    keep = c("id_old", keep_vars, "date.birth")))
} else {
  df_t$id_old <- df_t$id

  ms_kh <- suppressWarnings(mstate::msprep(
    time=c(NA, paste0("age.b", 1:b_max), "age.death"),
    status =c(NA, paste0("status.", 1:b_max), "status.death"),
    df_t,
    tmat,
    keep = c("id_old", "date.birth")))
}
  return(ms_kh)
}
