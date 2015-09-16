#' A Barplots for Differences in Bionomial Probabilities
#'
#' This function plots estimated probabilities with 95 percent confidence intervals.
#' @param df A dataframe containing status and factor variables
#' @param var Variable of interest
#' @param status Variable for status
#' @param new_size Change text size in labels
#' @keywords barplots confidence intervals
#' @export
#' @examples
#' plot_categories(data.frame(status = sample(c(0,1), 1000, replace=TRUE),
#'          group = factor(sample(c("A", "B", "C", "D"), 1000, replace=TRUE))), var="group")
#'
plot_categories <- function(df, var="var", status="status", new_size= NA){
  df <- df[order(as.integer(factor(df[,paste(var)]))),]
  .df_2 <- data.frame(label = paste(levels(factor(df[,paste(var)]))),
                      ci_lo = as.numeric(unlist(lapply(lapply(apply(data.frame(x = as.numeric(unlist(tapply(df[,paste(status)], as.integer(factor(df[,paste(var)])), sum))),
                                                                               n = as.numeric(unlist(tapply(df[,paste(status)], as.integer(factor(df[,paste(var)])), length)))), 1, binom.test), "[[", 4), "[[", 1))),
                      ci_hi =as.numeric(unlist(lapply(lapply(apply(data.frame(x = as.numeric(unlist(tapply(df[,paste(status)], as.integer(factor(df[,paste(var)])), sum))),
                                                                              n = as.numeric(unlist(tapply(df[,paste(status)], as.integer(factor(df[,paste(var)])), length)))), 1, binom.test), "[[", 4), "[[", 2))),
                      est = as.numeric(unlist(lapply(apply(data.frame(x = as.numeric(unlist(tapply(df[,paste(status)], as.integer(factor(df[,paste(var)])), sum))),
                                                                      n = as.numeric(unlist(tapply(df[,paste(status)], as.integer(factor(df[,paste(var)])), length)))), 1, binom.test), "[[", 5))))

  return(ggplot2::ggplot(.df_2,  ggplot2::aes(x=label , y=est, fill = 1)) + ggplot2::geom_bar(position=ggplot2::position_dodge(), stat="identity",fill = rgb(138,147,120, maxColorValue=255))   +
           ggplot2::geom_errorbar(ggplot2::aes(ymin=ci_lo, ymax=ci_hi), width=.2, position=ggplot2::position_dodge(.9)) +
           ggplot2::xlab("") + ggplot2::ylab("")  +
           ggplot2::theme(plot.title = ggplot2::element_text(hjust=0, size = ifelse(is.na(new_size), 10, new_size)+4),
                          legend.position = "none", axis.text.x  = ggplot2::element_text( size=ifelse(is.na(new_size), 10, new_size))))

}
