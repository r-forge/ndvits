\name{meanNA}
\alias{meanNA}
\title{
Mean function discarding NA values
}
\description{
Compute the mean of a vector, discarding NA values.
}
\usage{
meanNA(x)
}
\arguments{
  \item{x}{numerical vector}
}
\value{Return the mean of \verb{x}}
\author{Romain Frelat, Bruno Gerard}
\seealso{
\code{\link{minNA}}
}
\examples{
a=c(15,35,NA)
meanNA(a)
}
