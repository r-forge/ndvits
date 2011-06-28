\name{minNA2}
\alias{minNA2}
\title{
min function discarding NA values(2)
}
\description{
Compute the minimum of a two dimension vector, discarding NA values.
}
\usage{
minNA2(x)
}
\arguments{
  \item{x}{a two dimension vector}
}
\value{Return the minimum of \verb{x}}
\author{Romain Frelat, Bruno Gerard}
\seealso{
\code{\link{minNA}}
}
\examples{
a=c(15,NA)
minNA(a)
}
