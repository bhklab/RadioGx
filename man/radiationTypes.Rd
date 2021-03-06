% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/radiationTypes-methods.R
\name{radiationTypes}
\alias{radiationTypes}
\title{radiationTypes Generic}
\usage{
radiationTypes(object)
}
\arguments{
\item{object}{A \code{RadioSet}}
}
\value{
A vector of the radiation names used in the RadioSet
}
\description{
A generic for the radiationTypes method
}
\examples{
data(clevelandSmall)
radTypes <- radiationTypes(clevelandSmall)

}
