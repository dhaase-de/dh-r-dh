% basename
\name{group-strings}

% topic(s) documented
\alias{strfirst}
\alias{strlast}
\alias{strlen}

% title of the topic(s)
\title{String Functions}

% short description
\description{Additional (Vectorized) Functions for Strings}

% function synopsis
\usage{
   strfirst(v, characters = 1)
   strlast(v, characters = 1)
   strlen(v)
}

% function arguments
\arguments{
   \item{v}{Character vector containing the strings to be processed.}
   \item{characters}{Number of characters to extract from each string.}
}

% detailed description of the functionality
%\details{
%   
%}

% description of the return value(s)
\value{
   \code{strfirst} returns the first \code{characters} letters from each string in the character vector \code{v}.
   Similarly, \code{strlast} returns the last \code{characters} letters.
   
   \code{strlen} returns the length of each string in the character vector \code{v}.
}

% literature references
%\references{}

% special note(s)
\note{Note that all functions are vectorized, that is \code{v} may be a character vector of length greater than one.}

% author of this file
\author{Daniel Haase.}

% links to related R objects
\seealso{\code{\link{substr}}.}

% code examples
\examples{
   v <- c("Foobar", "Test", "Hi there")
   strlen(v)
   strfirst(v, 2)
   strlast(v, 3)
}

% R documentation keywords
%\keyword{}
