#' A wrapper function to plot age-specific residual reproductive values
#'
#' This function wraps the \code{get_resid_rv} function and \code{ggplot2::qplot} to plot the residual reproductive value against age for different groups of interest.
#' @param df A dataframe containing the variables being set for 'id' and 'var'.
#' @param momid The name of the variable for the ID of an individual's mother
#' @param var The variabke by which groups should divided and compared
#' @param df_ped A dataframe
#' @param evmat Event matrix (twins count as one)
#' @param evmat_bak Event matrix (including all twins)
#' @param categories Number of categories
#' @param action Default is "make.plot" to produce a plot on the current device, alternative options are "return.df" for returning a data.frame object containing relevant values or "return.plot" for returning a ggplot2 object.
#' @param y.lim The limits Upppr limit of the y axis
#' @keywords kh pedigree
#' @export
#' @examples
#' \dontrun{
#' # TBD
#' }
plot_rrv <- function(df=df_xs,
                          momid = "momid",
                          var,
                          df_ped, evmat, evmat_bak,
                          categories=NULL,
                          action = "make.plot",
                          y.lim=NULL){
  if(is.null(categories)){
    dfresid <- unlist_df(tapply( df[,paste(momid)], factor(df[,paste(var)]), get_resid_rv, df_ped, evmat, evmat_bak))
  } else dfresid <- unlist_df(tapply( df[,paste(momid)], categorize(df[,paste(var)],
                                                        categories), get_resid_rv, df_ped, evmat, evmat_bak))
  dfresid <- subset(dfresid, time < 50)
  if(action=="make.plot"){
  myplot <- ggplot2::qplot(time, resid.rv, data = dfresid, geom="smooth", method = "loess", span = 0.25, level = 0.99, colour = dflist,
                  ylim = c(ifelse(is.null(y.lim), 0, y.lim[1]), ifelse(is.null(y.lim), 1+ceiling(max(dfresid$resid.rv)) , y.lim[2]))) +
    ggplot2::xlab("Age (years)") + ggplot2::ylab("Residual reproductive value") + ggplot2::geom_point() + ggplot2::theme(legend.position="right",
                                                #             legend.direction="horizontal",
                                                             legend.title =  ggplot2::element_blank())
  print(myplot)
  }
  if(action=="return.df"){return(dfresid)}
  if(action=="return.plot"){
    return(
    ggplot2::qplot(time, resid.rv, data = dfresid, geom="smooth", method = "loess",span = 0.25,  level = 0.99, colour = dflist,
          ylim = c(ifelse(is.null(y.lim), 0, y.lim[1]), ifelse(is.null(y.lim), 1+ ceiling(max(dfresid$resid.rv)) , y.lim[2]))) +
      ggplot2::ylab("Residual reproductive value") + ggplot2::geom_point() + ggplot2::theme(legend.position="right",
                                                                 #                                                  legend.direction="horizontal",
                                                 legend.title = ggplot2::element_blank()))}
}
