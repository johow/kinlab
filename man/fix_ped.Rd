% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/fix_ped.R
\name{fix_ped}
\alias{fix_ped}
\title{Prepare a pedigree dataframe by excluding 'half-parents'}
\usage{
fix_ped(x, df_ind = NULL, subsample_id = NULL)
}
\arguments{
\item{x}{ID of individual for which a pedigree should be composed.}

\item{df_ind}{A dataframe for genealogical relations providing the variables 'id', 'momid' and 'dadid'}

\item{subsample_id}{A vector of IDs  (Set 'as.numeric(dimnames(evmat)$id') for IDs with events)}
}
\description{
This function returns a pedigree dataframe, which is restricted to members of its largest pedigree.
}
\examples{
\dontrun{
fix_ped(grap_kin(1067, df_ped), df_ped)
}
}
\keyword{pedigree}

