\name{distribution-methods}
\docType{methods}
\alias{distribution-methods}
\alias{distribution,character-method}
\alias{distribution,factor-method}
\alias{distribution,numeric-method}
\title{Standard builders for \code{Distribution} objects}
\description{
The \code{distribution} method is a generic method for creating \code{Distribution} objects.
}
\section{Methods}{
\describe{

\item{\code{signature(x = "character")}}{
Create a \code{Distribution} object from a \code{character}. The distribution is compute on all values appearing in the \code{character} object.
}

\item{\code{signature(x = "factor")}}{
Create a \code{Distribution} object from a \code{factor}. The distribution is compute on all levels of the \code{factor} object.
}

\item{\code{signature(x = "numeric")}}{
Create a \code{Distribution} object from a \code{numeric} object.
}
}}
\keyword{methods}
\keyword{distribution}
