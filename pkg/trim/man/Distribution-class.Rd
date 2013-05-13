\name{Distribution-class}
\Rdversion{1.1}
\docType{class}
\alias{Distribution-class}
\alias{print,Distribution-method}
\alias{show,Distribution-method}

\title{Class \code{"Distribution"}}
\description{
The \code{Distribution} class if a formal representation of a probability distribution of a categorical variable.
}
\section{Objects from the Class}{
Objects can be created by calls to the \code{\link{distribution}} methods.
}
\section{Slots}{
  \describe{
    \item{\code{vector}:}{Object of class \code{"numeric"}}
    \item{\code{precision}:}{Object of class \code{"numeric"}}
  }
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "Distribution")}: print a \code{Distribution} object (S3 method) }
    \item{show}{\code{signature(object = "Distribution")}:  print a \code{Distribution} object (S4 method) }
	 }
}

\author{
Emmanuel Rousseaux
}
\details{
A \code{Distribution} object perform the following checks:
- ...
}

\seealso{
\code{\link{distribution}}, \code{\link{distribution.comparable}}, \code{\link{distribution.reorder}}.
}
\examples{
distribution(c(0.2,0.8))
}
\keyword{classes}
\keyword{distribution}
