% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeSF2.R
\name{computeSF2}
\alias{computeSF2}
\title{Compute SF2}
\usage{
computeSF2(pars)
}
\arguments{
\item{pars}{parameters (alpha, beta) in equation
y = exp(-alpha * x - beta * x ^ 2)}
}
\value{
\code{numeric} The survival fraction
}
\description{
This function computes the survival fraction after administering
  2 units of radiation, given alpha and beta in the equation
  SF = exp(-alpha * D - beta * D ^ 2).
}
\examples{
computeSF2(c(0.2, 0.1))

}
