\name{implication.index}
\alias{implication.index}
\title{
Compute the implication index of Gras (YEAR).
}
\description{
The \code{implication.index} function computes the implication index statistics of Gras (YEAR).
}
\usage{
implication.index(
  y,
  target = decision.rule.majority(y, quiet = quiet),
  h0 = y,
  quiet = T,
  ...
)
}
\arguments{
  \item{y}{
a \code{WeithedVariable.categorical}, a \code{factor} or a \code{character}.
}
  \item{target}{
a character.
}
  \item{h0}{
a \code{WeithedVariable.categorical}, a \code{factor}, a \code{character} or a a \code{Distribution}.
}
  \item{quiet}{
a \code{logical}.
}
  \item{\dots}{
optional arguments (not used).
}
}
\details{
FIXME: write the formula.
}
\value{
A \code{numeric}, the value of the statistics.
}
\references{
FIXME
}
\author{
Emmanuel Rousseaux
}
\seealso{
\code{\link{implication.index.gain}}
}
\examples{
ritschard.array <- array(
  data = c(50,5,5,40,5,8,6,12,10,0,50,6,14,30,2,10,18,2),
  dim = c(3,3,2),
  dimnames = list(
    'civil status' = c('married', "single", 'divorced/widowed'),
    'working sector' = c('primary', 'secondary', 'tertiary'), 
    'sex' = c('man', 'woman')
  )
)
ritschard.data <- expand.array(ritschard.array)
ritschard.data
names(ritschard.data)

distribution(ritschard.data[ritschard.data$sex == 'man','civil.status'])
distribution(ritschard.data[ritschard.data$sex == 'woman','civil.status'])

imp.m <- implication.index(
  y = ritschard.data[ritschard.data$sex == 'man','civil.status'],
  h0 = distribution(ritschard.data$civil.status),
  quiet = FALSE
)

}
\keyword{ implication }
\keyword{ index }% __ONLY ONE__ keyword per line
