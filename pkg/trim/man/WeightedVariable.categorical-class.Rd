\name{WeightedVariable.categorical-class}
\Rdversion{1.1}
\docType{class}
\alias{WeightedVariable.categorical-class}
\alias{print,WeightedVariable.categorical-method}
\alias{show,WeightedVariable.categorical-method}

\title{Class \code{"WeightedVariable.categorical"}}
\description{
The \code{WeightedVariable.categorical} class is a formal representation of a weighted categorical variable.
}
\section{Objects from the Class}{
Objects can be created by calls to the \code{\link{wvc}} methods.
}
\section{Slots}{
  \describe{
    \item{\code{variable}:}{Object of class \code{"character"}}
    \item{\code{weights}:}{Object of class \code{"numeric"}}
    \item{\code{infos}:}{Object of class \code{"list"}}
  }
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "WeightedVariable.categorical")}: print a \code{WeightedVariable.categorica} object (S3 method) }
    \item{show}{\code{signature(object = "WeightedVariable.categorical")}: print a \code{WeightedVariable.categorica} object (S4 method) }
	 }
}
\author{
Emmanuel Rousseaux
}
\seealso{
\code{\link{wvc}}, \code{\link{distribution}}.
}
\examples{
data(iris)
a <- wvc(iris$Species)
a
class(a)
}
\keyword{classes}
