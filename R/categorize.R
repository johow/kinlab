#' Return categorical values for a numerical variable
#'
#' This function returns categories for a numerical variable, divided into either 2 or 4 or 5 or 10 categories.
#' @param x The numerical variable, which should be split up into categories.
#' @param y The desired number of categories with possible values 2, 4, 5, or 10.
#' @keywords numeric categories
#' @export
#' @examples
#' categorize(1:100, y =  5)
#'
categorize <- function(x, y=4){
  if (y==2){
    return(factor(
      ifelse(x == 0, "none", "at least one"), levels =
        c("none", "at least one"), ordered=TRUE))}
  if (y==4){
    return(factor(
      ifelse(x == 0, "none",
             ifelse(x == 1, "one",
                    ifelse(x == 2, "two", "more"))), levels =
        c("none", "one", "two", "more"), ordered=TRUE))}
  if (y==5){
    if (quantile(x, .25) == 0 & quantile(x, .5) > 1){
      return(factor(
        ifelse(x == 0, "Q1:\nnone",
               ifelse(x >= 1 & x < quantile(x, .5), paste0("Q2:\n", 1,"-",quantile(x, .5)),
                      ifelse(x >= quantile(x, .5) & x < quantile(x, .75),
                             paste0("Q3:\n",quantile(x, .5),"-",quantile(x, .75)),
                             paste0("Q4:\n", quantile(x, .75),"-",max(x))))), levels =
          c("Q1:\nnone",
            paste0("Q2:\n", 1,"-",quantile(x, .5)),
            paste0("Q3:\n", quantile(x, .5),"-",quantile(x, .75)),
            paste0("Q4:\n", quantile(x, .75),"-",max(x))), ordered=TRUE))
    }
    if (quantile(x, .25) == 0 & quantile(x, .5) == 1 & quantile(x, .75) > 2){
      return(factor(
        ifelse(x == 0, "Q1:\nnone",
               ifelse(x == 1, "Q2:\n1",
                      ifelse(x > quantile(x, .5) & x < quantile(x, .75),
                             paste0("Q3:\n", 1+quantile(x, .5),"-",quantile(x, .75)),
                             paste0("Q4:\n", 1+quantile(x, .75),"-",max(x))))), levels =
          c("Q1:\nnone",
            "Q2:\n1",
            paste0("Q3:\n", 1+quantile(x, .5),"-",quantile(x, .75)),
            paste0("Q4:\n", 1+quantile(x, .75),"-",max(x))), ordered=TRUE))
    }
    if (quantile(x, .25) == 0 & quantile(x, .5) == 1 & quantile(x, .75) == 2){
      return(factor(
        ifelse(x == 0, "Q1:\nnone",
               ifelse(x == 1, "Q2:\n1",
                      ifelse(x == 2, "Q3:\n2",
                             paste0("Q4:\n",quantile(x, .75),"-",max(x))))), levels =
          c("Q1:\nnone",
            "Q2:\n1",
            "Q3:\n2",
            paste0("Q4:\n",quantile(x, .75),"-",max(x))), ordered=TRUE))
    }
    if (quantile(x, .25) == 1 & quantile(x, .5) > 1){
      return(factor(
        ifelse(x == 0, "Q1:\nnone",
               ifelse(x >= 1 & x < quantile(x, .5), paste0("Q2:\n",1,"-",quantile(x, .5)),
                      ifelse(x >= quantile(x, .5) & x < quantile(x, .75),
                             paste0("Q3:\n",quantile(x, .5),"-",quantile(x, .75)),
                             paste0("Q4:\n",quantile(x, .75),"-",max(x))))), levels =
          c("Q1:\nnone",
            paste0("Q2:\n",1,"-",quantile(x, .5)),
            paste0("Q3:\n",quantile(x, .5),"-",quantile(x, .75)),
            paste0("Q4:\n",quantile(x, .75),"-",max(x))), ordered=TRUE))
    }
    if (quantile(x, .5) == 1 & quantile(x, .75) == 2){
      return(factor(
        ifelse(x == 0, "Q1:\nnone",
               ifelse(x == 1, "Q2:\n1",
                      ifelse(x == 2, "Q3:\n2",
                             paste0("Q4:\n", 1+quantile(x, .75),"-",max(x))))), levels =
          c("Q1:\nnone",
            "Q2:\n1", "Q3:\n1",
            paste0("Q4:\n",1+quantile(x, .75),"-",max(x))), ordered=TRUE))
    } else
      return(factor(
        ifelse(x == 0, "Q1:\nnone",
               ifelse(x >= 1 & x < quantile(x, .25), paste0("Q2:\n",1,"-",quantile(x, .25)),
                      ifelse(x >= quantile(x, .25) & x < quantile(x, .75),
                             paste0("Q3:\n",quantile(x, .25),"-",quantile(x, .75)),
                             paste0("Q4:\n",quantile(x, .75),"-",max(x))))), levels =
          c("Q1:\nnone",
            paste0("Q2:\n",1,"-",quantile(x, .25)),
            paste0("Q3:\n",quantile(x, .25),"-",quantile(x, .75)),
            paste0("Q4:\n",quantile(x, .75),"-",max(x))), ordered=TRUE))}
  if (y==10) {
    return(factor(
      ifelse(x == 0, "0", ifelse(x == 1, "1", ifelse(x == 2, "2", ifelse(x == 3, "3", ifelse(x == 4, "4", ifelse(x == 5, "5",  ifelse(x == 6, "6",ifelse(x == 7, "7", ifelse(x == 8, "8", ifelse(x == 9, "9",  "10 or more")))))))))), levels =
        c("0","1", "2","3", "4","5","6","7", "8","9","10 or more"), ordered=TRUE))
  }
}
