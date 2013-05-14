\name{wvc}
\alias{wvc}
\alias{wvc-methods}
\title{
Create a \code{WeightedVariable.categorical} object.
}
\description{
The \code{wvc} function is a generic S4 method for creating a \code{WeightedVariable.categorical} object.
}
\usage{
wvc(
  variable,
  weights = rep(1, length(variable)),
  infos = list()
)
}
\arguments{
  \item{variable}{
a categorical variable, i.e. either a \code{factor} or a \code{character}
}
  \item{weights}{
a \code{numeric} vector for weighting, for example non-response weights.
}
  \item{infos}{
a \code{list} for storing other information if needed.
}
}
\value{
A \code{WeightedVariable.categorical} object.
}
\author{
Emmanuel Rousseaux
}
\seealso{
\code{\link[=WeightedVariable.categorical-class]{WeightedVariable.categorical}}
}
\examples{
data(iris)
a <- wvc(iris$Species)
a
wv.variable(a)
wv.weights(a)
wv.infos(a)

a <- wvc(as.character(iris$Species))

a <- wvc(
  iris$Species,
  weights = c(rep(1,50), rep(0,100))
)
a

a[1:10]
length(a)
}

\keyword{ categorical }
\keyword{ categorical }
