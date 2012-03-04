\name{file.extension}

\alias{file.extension}

\title{Return the extension of filenames}

\description{Return the extensions of given filenames.}

\usage{file.extension(filenames)}

\arguments{
   \item{filenames}{Character vector giving the filenames to be checked.}
}

\details{The function returns the extension of each filename given in the arguent \code{filenames}. For filenames without extension, the value \code{""} is returned.}

\note{For filenames starting with \code{.}, the part until the next \code{.} is not considered a extension by this function (see the examples below). Currently, this function was only tested under GNU/Linux and probably has some flaws with whacky filenames (e.g. filenames having line breaks).}

\value{A character vector with a same length as the argument \code{filenames}}

\seealso{\code{\link{file.exists}}, \code{\link{dir}}.}

\examples{
   file.extension(c(".RData", "x.RData"))
   file.extension(dir(all.files = TRUE))
}