% basename
\name{function-wait}

% topic(s) documented
\alias{wait}

% title of the topic(s)
\title{Wait for Press of Return}

% short description
\description{Waits for the user to press return.}

% function synopsis
\usage{
   wait()
}

% function arguments
%\arguments{
%   \item{}{}
%}

% detailed description of the functionality
\details{
   This function is a wrapper for \code{readline("Press [RETURN] to continue...")}.
}

% description of the return value(s)
\value{A character vector of length one, see \code{\link{readline}}.}

% literature references
%\references{}

% special note(s)
%\note{}

% author of this file
\author{Daniel Haase.}

% links to related R objects
\seealso{\code{\link{readline}}.}

% code examples
\examples{
   wait()
}

% R documentation keywords
%\keyword{}
