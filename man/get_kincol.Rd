% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/get_kincol.R
\name{get_kincol}
\alias{get_kincol}
\title{Rainbow color palette for relatedness coefficients}
\usage{
get_kincol(id, pedigree.name = NA, df_ped, my.col = "black", X = FALSE)
}
\arguments{
\item{id}{An individual ID}

\item{pedigree.name}{The individual's pedigree}

\item{df_ped}{A dataframe}

\item{my.col}{Color for ego of individual ID (default is 'black')}

\item{X}{TRUE if X-chromosomal relatedness instead of autosomal relatedness should be computed (default is FALSE)}
}
\description{
This function returns a vector containing color names based on the relatedness coefficients between a given individual's ID and all members of a given pedigree. The given individual is returned as 'black', while colors of other pedigre members depend on relatedness coefficients ranging from 'green' (closely related) to 'red' (relatively unrelated).
}
\examples{
\dontrun{
df_ind <- get_exmpl_df()
df_ind$bdate <- sample(seq(as.Date("1774-12-31"), as.Date("1874-12-31"), 100), nrow(df_ind))
df_fam <- data.frame(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
my_id <- sample_kh(df_ind, df_fam)
get_kincol(1067, df_ped= df_ind)
}
}
\keyword{events}
\keyword{kh}
\keyword{pedigree}

