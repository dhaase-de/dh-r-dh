\name{vector.ortho2D}

\alias{vector.ortho2D}

\title{Calculate Orthogonal Vectors of 2D Input Vectors}

\description{Calculate the orthogonal and normalized vectors for one or more given two-dimensional input vectors.}

\usage{vector.ortho2D(P)}

\arguments{
   \item{P}{either a N-by-2 matrix or a vector of length 2 holding the x and y coordinates of the input vectors.}
}

\value{The returned object has the same dimensions as the input \code{P}. If \code{P} is a matrix, the rownames of the returned matrix are those of \code{P}. All output vectors are normalized to have a length of one.}

\examples{
   vector.ortho2D(c(2,0))
   vector.ortho2D(matrix(runif(10), nrow = 5, ncol = 2))
}
