\name{minNA}
\alias{minNA}
\title{
min function discarding NA values
}
\description{
Compute the minimum of a vector, discarding NA values.
}
\usage{
minNA(x)
}
\arguments{
 \item{x}{vector}
}
\value{Return the minimum of \verb{x}}
\author{Romain Frelat, Bruno Gerard}
\seealso{
\code{\link{minNA}}
}
\examples{
a=c(15,35,NA)
minNA(a)
}
