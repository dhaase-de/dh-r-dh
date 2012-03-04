% basename
\name{function-finite}

% topic(s) documented
\alias{finite}

% title of the topic(s)
\title{Return Finite Values}

% short description
\description{Return all finite values of an R object.}

% function synopsis
\usage{
   finite(x)
}

% function arguments
\arguments{
   \item{x}{Object for which all finite values are to be returned.}
}

% detailed description of the functionality
\details{Wrapper for \code{x[is.finite(x)]}.}

% description of the return value(s)
\value{A vector containing all finite values of \code{x}.}

% literature references
%\references{}

% special note(s)
%\note{}

% author of this file
\author{Daniel Haase.}

% links to related R objects
\seealso{\code{\link{is.finite}}.}

% code examples
\examples{
   finite(log(-1:3))
}

% R documentation keywords
%\keyword{}
