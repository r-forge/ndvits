\name{sdNA}
\alias{sdNA}
\title{
sd function discarding NA values
}
\description{
Compute the standard deviation of a vector, discarding NA values.
}
\usage{
sdNA(x)
}
\arguments{
  \item{x}{vector}
}
\value{Return the standard deviation of \verb{x}}
\author{Romain Frelat, Bruno Gerard}
\seealso{
\code{\link{minNA}}
}
\examples{
a=c(15,35,NA)
sdNA(a)
}
