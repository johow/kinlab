% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/get_sisters.R
\name{get_sisters}
\alias{get_sisters}
\title{Return an individual's sisters being ever born}
\usage{
get_sisters(x, df_ped)
}
\arguments{
\item{x}{ID of individual of interest}

\item{df_ped}{A dataframe}
}
\description{
This function returns any female indiviual being born from the same mother -- excluding the ID for a given (possibly female) individual.
}
\examples{
\dontrun{
grap_ped(1067, df_ped)
}
}
\keyword{pedigree}
\keyword{sisters}

