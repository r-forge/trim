\name{implication.index.gain}
\alias{implication.index.gain}
\title{
Compute the gain of a split by the implication index of Gras
}
\description{
The \code{implication.index.gain} function computes the implication index statistics of Gras (YEAR).
}
\usage{
implication.index.gain(
  y,
  kidsids,
  decision.rule = decision.rule.majority,
  h0 = y,
  aggregation.rule = aggregation.rule.weighted.mean,
  quiet = T,
  rquiet = T
)
}
\arguments{
  \item{y}{
a \code{WeithedVariable.categorical}, a \code{factor} or a \code{character}.
}
  \item{kidsids}{
a \code{integer} specifying how to split the variable.
}
  \item{decision.rule}{
a \code{function} specifying how to choose the class of interest. See for example \code{\link{decision.rule.majority}}.
}
  \item{h0}{
a \code{WeithedVariable.categorical}, a \code{factor}, a \code{character} or a a \code{Distribution}.
}
  \item{aggregation.rule}{
a \code{function} specifying how to aggregate utility between parent and children. See for example \code{\link{aggregation.rule.weighted.mean}}.
}
  \item{quiet}{
a \code{logical}.
}
  \item{rquiet}{
a \code{logical}, if \code{TRUE} the quiet order is passed in each sub-call.
}
}
\value{
A \code{numeric}, the value of the gain.
}
\references{
FIXME
}
\author{
Emmanuel Rousseaux
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{implication.index}}
}
\examples{
gilbert.array <- array(
  data = c(50,5,5,40,5,8,6,12,10,0,50,6,14,30,2,10,18,2),
  dim = c(3,3,2),
  dimnames = list(
    'civil status' = c('married', "single", 'divorced/widowed'),
    'working sector' = c('primary', 'secondary', 'tertiary'), 
    'sex' = c('man', 'woman')
  )
)
gilbert.data <- expand.array(gilbert.array)
gilbert.data
names(gilbert.data)

implication.index.gain(
  y = gilbert.data$civil.status,
  kidsids = c(rep(1, 141), rep(2, 132)),
  h0 = gilbert.data$civil.status
)
implication.index.gain(
  y = gilbert.data$civil.status,
  kidsids = c(rep(1, 141), rep(2, 132))
)

implication.index.gain(
  y = gilbert.data$civil.status,
  kidsids = c(rep(1, 141), rep(2, 132)),
  aggregation.rule = aggregation.rule.maximum
)
implication.index.gain(
  y = gilbert.data$civil.status,
  kidsids = c(rep(1, 141), rep(2, 132)),
  aggregation.rule = aggregation.rule.minimum
)
implication.index.gain(
  y = gilbert.data$civil.status,
  kidsids = c(rep(1, 141), rep(2, 132))
)
kidsids.sector <- rep(0, nrow(gilbert.data))
kidsids.sector[gilbert.data$working.sector == 'primary'] <- 1
kidsids.sector[gilbert.data$working.sector == 'secondary'] <- 2
kidsids.sector[gilbert.data$working.sector == 'tertiary'] <- 3
implication.index.gain(
  y = gilbert.data$civil.status,
  kidsids = kidsids.sector,
  h0 = distribution(gilbert.data$civil.status)
)
implication.index.gain(
  y = gilbert.data$civil.status,
  kidsids = c(rep(1, 141), rep(2, 132)),
  h0 = distribution(gilbert.data$civil.status),
  decision.rule = decision.rule.married
)
implication.index(
  y = gilbert.data$civil.status,
  h0 = distribution(gilbert.data$civil.status),
  quiet = FALSE
)
implication.index.gain(
  y = gilbert.data$civil.status,
  kidsids = c(rep(1, 141), rep(2, 132)),
  h0 = distribution(gilbert.data$civil.status),
  aggregation.rule = aggregation.rule.information.gain
)
}
\keyword{ implication }
\keyword{ index }
