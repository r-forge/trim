\name{distribution}
\alias{distribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a \code{Distribution} object.
}
\description{
The \code{distribution} function is a generic method for creating a \code{Distribution} object.
}
\usage{
distribution(
  x,
  weights = rep(1, length(x)),
  precision = 0.001, 
  p.estimator = NA
)
}
\arguments{
  \item{x}{
A R object, see \code{\link{distribution-methods}}.
}
  \item{weights}{
A \code{numeric} vector for weighting, for example non-response weights.
}
  \item{precision}{
A single numeric value. To be valid a \code{Distribution} object has to satisfy sum(object) == 1 +- precision.
}
  \item{p.estimator}{
Used only when \code{x} is a \code{character} or a \code{factor}. In this case \code{p.estimator} has to be function able to estimate probabilities, see for example \code{\link{p.estimator.laplace}}. A such function takes two arguments: \code{y} and \code{weights}, with \code{y} a \code{character} or a \code{factor}, and return a \code{numeric}. 
}
}
\value{
A \code{Distribution} object.
}
\author{
Emmanuel Rousseaux
}

\seealso{
\code{\link{Distribution-class}}, \code{\link{distribution.comparable}}, \code{\link{distribution.reorder}}.
}
\examples{
distribution(c(0.2,0.8))
data(iris)
distribution(iris$Species)

distribution(
  iris$Species,
  weights=c(rep(1,50), rep(0,100)),
  p.estimator = NA
)
distribution(
  iris$Species,
  weights=c(rep(1,50), rep(0,100)),
  p.estimator = p.estimator.laplace
)
}

\keyword{ distribution }
