\name{distribution.comparable}
\alias{distribution.comparable}
\title{
Check if two \code{Distribution} objects are comparable
}
\description{
The \code{distribution.comparable} function check if \code{Distribution} objects are comparable. Two \code{Distribution} objects are comparable if and only if they have same length and share same value names. Precision can differ.
}
\usage{
distribution.comparable(
  x,
  y, 
  quiet = T
)
}
\arguments{
  \item{x}{
A \code{Distribution} object.
}
  \item{y}{
A \code{Distribution} object.
}
  \item{quiet}{
A logical. If \code{FALSE} informative messages can be print.
}
}
\value{
A logical.
}
\author{
Emmanuel Rousseaux
}

\seealso{
\code{\link{Distribution-class}}, \code{\link{distribution}}, \code{\link{distribution.reorder}}.
}
\examples{
x <- distribution(c(0.5,0.5))
y <- distribution(c(0.1,0.9))
distribution.comparable(x,y)

x <- distribution(c("well" = 0.5, "bad" = 0.5))
y <- distribution(c("well" = 0.1, "poor" = 0.9))
distribution.comparable(x,y)
distribution.comparable(x,y, quiet = FALSE)
}
\keyword{ distribution }
\keyword{ comparison }
