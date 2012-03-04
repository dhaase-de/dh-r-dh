% basename
\name{function-is.singleton}

% topic(s) documented
\alias{is.singleton}

% title of the topic(s)
\title{Checks for Vector of Length One}

% short description
\description{Checks if argument is a vector of length one.}

% function synopsis
\usage{
   is.singleton(x)
}

% function arguments
\arguments{
   \item{x}{Object to be tested.}
}

% detailed description of the functionality
%\details{}

% description of the return value(s)
\value{Returns \code{TRUE} if the object is a vector and has a length of one and \code{FALSE} otherwise.}

% literature references
%\references{}

% special note(s)
%\note{}

% author of this file
\author{Daniel Haase.}

% links to related R objects
%\seealso{}

% code examples
\examples{
   is.singleton(10)
   is.singleton(NULL)
   is.singleton(list(1))
   is.singleton(1:5)
}

% R documentation keywords
%\keyword{}
