\name{p.estimator.laplace}
\alias{p.estimator.laplace}
\title{
Laplace's estimator of probability
}
\description{
Compute the estimated probabilities of a given variable with the Laplace's estimator of probability.
}
\usage{
p.estimator.laplace(
  y,
  weights = rep(1, length(y))
)
}
\arguments{
  \item{y}{
A \code{character} of a \code{factor}.
}
  \item{weights}{
A \code{numeric} vector for weighting, for example non-response weights.
}
}
\value{
A \code{numeric} vector corresponding to the estimated probabilities.
}
\references{
FIXME
}
\author{
Emmanuel Rousseaux
}
\seealso{
\code{\link{distribution}}.
}
\examples{
data(iris)
p.estimator.laplace(iris$Species)
}
\keyword{ estimator }
\keyword{ Laplace }