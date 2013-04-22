\docType{methods}
\name{stdz}
\alias{stdz}
\title{stdz, Standardise a set of data}
\usage{
  stdz(x, na.rm = TRUE)
}
\arguments{
  \item{x,}{An R object. Currently there is only a method
  for numeric vectors.}

  \item{na.rm}{a logical value indicating whether NA values
  should be stripped before the computation proceeds.}
}
\value{
  A vector.
}
\description{
  Function takes a vector and scales it so the mean is 0
  and the standard deviation is 1. It is intended to be
  used with the \code{ply} package that implement the
  split-apply-combine pattern in R
}
\note{
  nn
}
\examples{
x = rlnorm(100,.2,.3)
x = stdz(x)
mean(x)
se(x)
}
\author{
  Laurence Kell
}
\references{
  \url{}
}
\seealso{
  \code{\link{minMax}} (scale so min=0 and max=1)
}

