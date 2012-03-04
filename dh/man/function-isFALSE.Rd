% basename
\name{function-isFALSE}

% topic(s) documented
\alias{isFALSE}

% title of the topic(s)
\title{Alias for \code{!isTRUE}}

% short description
\description{This function is an alias for \code{!isTRUE}.}

% function synopsis
\usage{
   isFALSE(x)
}

% function arguments
\arguments{
   \item{x}{Object to be tested.}
}

% detailed description of the functionality
%\details{}

% description of the return value(s)
\value{This function is an alias for \code{!isTRUE} and therefore returns \code{FALSE} if and only if \code{x} is identical to \code{TRUE}.}

% literature references
%\references{}

% special note(s)
\note{\code{isTRUE(x)} returns \code{TRUE} if \code{x} is identical to \code{TRUE}. Be aware that \code{isFALSE} does not check for \code{x} to be identical to \code{FALSE} but checks if \code{x} is not identical to \code{TRUE}.}

% author of this file
\author{Daniel Haase.}

% links to related R objects
\seealso{\code{\link{isTRUE}}.}

% code examples
\examples{
   isFALSE(TRUE)
   isFALSE(FALSE)
   isFALSE(0)
   isFALSE(1)
   isFALSE(1:10)
   isFALSE(NULL)
}

% R documentation keywords
%\keyword{}
