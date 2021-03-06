% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/get_kirchspiele.R
\name{get_kirchspiele}
\alias{get_kirchspiele}
\title{Recode locality labels in KH33 database (internal function)}
\usage{
get_kirchspiele(x, valid_entries = c("CA", "CH", "CI", "CN", "EI", "FR", "GH",
  "GI", "GM", "GS", "HI", "HW", "JE", "LA", "LO", "LP", "LV", "MA", "MW", "NE",
  "PE", "PI", "RY", "SU", "TW", "UP", "UT", "VI", "WE", "WI", "WO", "WY", "WZ"))
}
\arguments{
\item{x}{A character vector containing locality labels which should be standardized.}

\item{valid_entries}{A character vector containing all valid standard locality labels.}
}
\description{
This function standardizes labels of localities in the KH33 database.
}
\examples{
get_kirchspiele("CA")
get_kirchspiele("WY")
}
\keyword{KH33}
\keyword{labels}

