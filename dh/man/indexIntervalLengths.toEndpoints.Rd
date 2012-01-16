\name{indexIntervalLengths.toEndpoints}

\alias{indexIntervalLengths.toEndpoints}

\title{Transform index intervals given by their lengths into an endpoint description.}

\description{Transform index intervals given by their lengths into an endpoint description.}

\usage{indexIntervalLengths.toEndpoints(lengths, return.list = FALSE)}

\arguments{
   \item{lengths}{A numeric vector giving the lengths of the index intervals. All entries must be \eqn{\ge 1} and will be converted to integers.}
   \item{return.list}{A logical defining whether to return the result as a list or a matrix.}
}

\value{If \code{return.list = FALSE} (default), a matrix with one row per interval and 2 columns is returned. Otherwise, the result is a list with one element per interval, which in turn is a list with the elements \code{from} and\code{to}.}

\examples{
indexIntervalLengths.toEndpoints(c(10, 4, 8, 3))
indexIntervalLengths.toEndpoints(c(10, 4, 8, 3), return.list = TRUE)
}