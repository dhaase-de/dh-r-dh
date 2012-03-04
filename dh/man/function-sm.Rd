\name{sm}

\alias{sm}

\title{Set new working directory and source the file \code{main.R}}

\description{Set new working directory (unchanged by default) and source the file \code{main.R}.}

\usage{sm(dir = getwd(), return = FALSE)}

\arguments{
   \item{dir}{The directory to be set as new working directory and where to look for the file \code{main.R}.}
   \item{return}{If \code{TRUE}, the function returns to the original working directory after sourcing.}
}

\details{The function checks if the file \code{main.R} exists in the given directory. If this is the case, the given directory is set as new working directory and the file \code{main.R} is sourced. Otherwise, an error is triggered. If argument \code{return} is \code{TRUE}, then the function returns to the original working directory after sourcing the files.}

\value{The return value of the function \code{\link{source}} is returned.}

\examples{
   \dontrun{sm()}
   \dontrun{sm("/path/to/my/project", return = TRUE)}
}
