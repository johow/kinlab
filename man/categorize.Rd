% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/categorize.R
\name{categorize}
\alias{categorize}
\title{Return categorical values for a numerical variable}
\usage{
categorize(x, y = 4)
}
\arguments{
\item{x}{The numerical variable, which should be split up into categories.}

\item{y}{The desired number of categories with possible values 2, 4, 5, or 10.}
}
\description{
This function returns categories for a numerical variable, divided into either 2 or 4 or 5 or 10 categories.
}
\examples{
categorize(1:100, y =  5)
}
\keyword{categories}
\keyword{numeric}

