% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/get_twinmothers.R
\name{get_twinmothers}
\alias{get_twinmothers}
\title{Get IDs of twinmothers}
\usage{
get_twinmothers(df = NULL, var_bdate = NULL, var_mother = NULL,
  numeric_output = FALSE)
}
\arguments{
\item{df}{A dataframe containing birth dates of offspring and mother IDs}

\item{var_bdate}{Variable for offspring's birth date}

\item{var_mother}{Variable of mother ID}

\item{numeric_output}{Should index be numeric}
}
\description{
Returns a vector of twinmother IDs
}
\examples{
\dontrun{
grap_ped(1067, df_ped)
}
}
\keyword{kh}
\keyword{localitiy}

