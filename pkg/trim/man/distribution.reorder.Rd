\name{distribution.reorder}
\alias{distribution.reorder}
\title{
Reorder a \code{Distribution} object according to another \code{Distribution} object
}
\description{
The \code{distribution.reorder} function reorder a \code{Distribution} object to make its names be in the same order than a another \code{Distribution} object. Both objects have to be \link[=distribution.comparable]{comparable}.
}
\usage{
distribution.reorder(
  x,
  model,
  quiet = T
)
}

\arguments{
  \item{x}{
A \code{Distribution} object.
}
  \item{model}{
A \code{Distribution} object which is the model for name ordering.
}
  \item{quiet}{
A logical.
}
}

\value{
The \code{x} \code{Distribution} object with names reordered according to names of \code{model}.
}
\author{
Emmanuel Rousseaux
}
\seealso{
\link{distribution.comparable}
}
\examples{
x <- distribution(c(0.5,0.5))
y <- distribution(c(0.1,0.9))
distribution.reorder(x,y)

x <- distribution(c("well" = 0.5, "poor" = 0.5))
y <- distribution(c("poor" = 0.1, "well" = 0.9))
distribution.reorder(x,y)
distribution.reorder(x,y, quiet = FALSE)
}

\keyword{ distribution }
\keyword{ order }
