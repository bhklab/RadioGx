% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linearQuadraticModel.R
\name{linearQuadraticModel}
\alias{linearQuadraticModel}
\title{Fit linear-quadratic curves to dose-response data}
\usage{
linearQuadraticModel(
  D,
  SF,
  lower_bounds = c(0, 0),
  upper_bounds = c(1, 1),
  scale = 5,
  family = c("normal", "Cauchy"),
  median_n = 1,
  trunc = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{D}{vector of radiation doses}

\item{SF}{vector of survival fractions corresponding to the doses}

\item{lower_bounds}{vector of length 2 containing minimum allowed values of
fitted alpha and beta, respectively}

\item{upper_bounds}{vector of length 2 containing maximum allowed values of
fitted alpha and beta, respectively}

\item{scale}{parameter of the assumed error distribution of the data; see
sdetails}

\item{family}{family of distributions of the error terms in the data;
currently supported options are "normal" and "cauchy"}

\item{median_n}{see details}

\item{trunc}{should survival fractions be truncated downward to 1? Defaults
to FALSE.}

\item{verbose}{'verbose' outputs warnings that are otherwised suppressed when
the function sanity-checks user inputs. 'median_n' denotes the number of
distributions from family 'family' that are medianned. (Note that setting
n = 1 (the default) is equivalent to using a simple normal or cauchy
distribution without taking any medians.)}
}
\value{
\code{numeric} The estimated alpha and beta values
}
\description{
This function fits a linear-quadratic curve to dose-response data.
}
\examples{
linearQuadraticModel(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
 c(1.1, 0.8, 0.7, 0.45, 0.15, -0.1, -0.1, -0.4, -0.65, -0.75, -1.1))

}
