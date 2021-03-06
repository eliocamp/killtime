% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transforms.R
\docType{data}
\name{trans_probit}
\alias{trans_probit}
\alias{trans_identity}
\alias{trans_logit}
\alias{trans_cll}
\title{Transformations}
\format{
An object of class \code{list} of length 3.

An object of class \code{list} of length 3.

An object of class \code{list} of length 3.

An object of class \code{list} of length 3.
}
\usage{
trans_probit

trans_identity

trans_logit

trans_cll
}
\arguments{
\item{p}{cummulative proportion of dead animals}

\item{z}{transformed p}

\item{covp}{covariance matrix of p}

\item{n}{number of total experimental animales.}
}
\description{
Transformations
}
\keyword{datasets}
\keyword{internal}
