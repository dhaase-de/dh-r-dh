% basename
\name{function-recycle}

% topic(s) documented
\alias{recycle}

% title of the topic(s)
\title{Recycle Vector to a Given Length}

% short description
\description{Recycle the values of a vector to a given length and give a warning if it was partially repeated}

% function synopsis
\usage{
   recycle(x, length.out)
}

% function arguments
\arguments{
   \item{x}{Vector whose values are to be recycled.}
   \item{length.out}{Desired length of the recycled vector.}
}

% detailed description of the functionality
%\details{}

% description of the return value(s)
\value{A vector containing the recycled values of \code{x} with a length as specified in argument \code{length.out}.}

% literature references
%\references{}

% special note(s)
%\note{}

% author of this file
\author{Daniel Haase.}

% links to related R objects
\seealso{\code{\link{rep}}.}

% code examples
\examples{
   recycle(1:5, 10)
   recycle(1:5, 11)
}

% R documentation keywords
%\keyword{}
