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
a \code{character} or a \code{factor}.
}
  \item{weights}{
a \code{numeric} vector for weighting, for example non-response weights.
}
}
\value{
a \code{numeric} vector corresponding to the estimated probabilities.
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
p.estimator.laplace(
  y = iris$Species,
  weights = c(rep(0,50),rep(1,100))
)
}
\keyword{ estimator }
\keyword{ Laplace }