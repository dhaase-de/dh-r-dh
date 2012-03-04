% basename
\name{function-clip}

% topic(s) documented
\alias{clip}

% title of the topic(s)
\title{Clip Values of an Object}

% short description
\description{Clip Ordinal Values of an Object}

% function synopsis
\usage{
   clip(x, low = 0, high = 1)
}

% function arguments
\arguments{
   \item{x}{Object whose values are to be clipped.}
   \item{low}{Lower limit of the clipping range.}
   \item{high}{Upper limit of the clipping range.}
}

% detailed description of the functionality
\details{All values in \code{x} which are smaller than \code{low} (larger than \code{high}) are replaced by \code{low} (\code{high}).}

% description of the return value(s)
\value{Object \code{x} with clipped values.}

% literature references
%\references{}

% special note(s)
\note{This function is not only valid for numerics but for all ordered values, such as strings or ordered factors.}

% author of this file
\author{Daniel Haase.}

% links to related R objects
%\seealso{}

% code examples
\examples{
   clip(1:10, 3, 8)
   clip(letters[1:10], "d", "f")
}

% R documentation keywords
%\keyword{}
