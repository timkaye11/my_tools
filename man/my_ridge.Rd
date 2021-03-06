% Generated by roxygen2 (4.1.0.9000): do not edit by hand
% Please edit documentation in R/lm_tools.R
\name{my_ridge}
\alias{my_ridge}
\title{A wrapper for ridge regression}
\usage{
my_ridge(lin_mod, lam = seq(0, 100, 0.01))
}
\arguments{
\item{lin_mod}{A linear model of class 'lm'}

\item{lam}{The lambda values to consider for ridge regression}
}
\description{
Ridge regression made easy
}
\examples{
my_ridge(lm(resp ~ predictor))
}
\keyword{ridge}

