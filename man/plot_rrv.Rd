% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/plot_rrv.R
\name{plot_rrv}
\alias{plot_rrv}
\title{A wrapper function to plot age-specific residual reproductive values}
\usage{
plot_rrv(df = df_xs, momid = "momid", var, df_ped, evmat, evmat_bak,
  categories = NULL, action = "make.plot", y.lim = NULL)
}
\arguments{
\item{df}{A dataframe containing the variables being set for 'id' and 'var'.}

\item{momid}{The name of the variable for the ID of an individual's mother}

\item{var}{The variabke by which groups should divided and compared}

\item{df_ped}{A dataframe}

\item{evmat}{Event matrix (twins count as one)}

\item{evmat_bak}{Event matrix (including all twins)}

\item{categories}{Number of categories}

\item{action}{Default is "make.plot" to produce a plot on the current device, alternative options are "return.df" for returning a data.frame object containing relevant values or "return.plot" for returning a ggplot2 object.}

\item{y.lim}{The limits Upppr limit of the y axis}
}
\description{
This function wraps the \code{get_resid_rv} function and \code{ggplot2::qplot} to plot the residual reproductive value against age for different groups of interest.
}
\examples{
\dontrun{
# TBD
}
}
\keyword{kh}
\keyword{pedigree}

