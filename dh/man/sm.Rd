\name{sm}

\alias{sm}

\title{Set new working directory and source the file \code{main.R}}

\description{Set new working directory (unchanged by default) and source the file \code{main.R}.}

\usage{sm(dir = getwd())}

\arguments{
   \item{dir}{The directory to be set as new working directory and where to look for the file \code{main.R}.}
}

\details{The function checks if the file \code{main.R} exists in the given directory. If this is the case, the given directory is set as new working directory and the file \code{main.R} is sourced. Otherwise, an error is triggered.}

\value{The return value of the function \code{\link{source}} is returned.}

\examples{
   \dontrun{sm()}
   \dontrun{sm("/path/to/my/project")}
}