\name{Packages}

\alias{is.installed}
\alias{is.loadedPackage}

\title{Package functions}

\description{Check if a package is installed or loaded.}

\usage{
   is.installed(packages)
   is.loadedPackage(packages)
}

\arguments{
   \item{packages}{Character vector of package names.}
}

\details{
   \code{is.installed} checks whether a package is installed.
   
   \code{is.loadedPackage} checks, if an installed package loaded (via \code{\link{library}}) in the current R session.
}

\value{
   A logical vector indicating installed/loaded (\code{TRUE}) and not installed/loaded (\code{FALSE}) packages.
}

\seealso{\code{\link{install.packages}}, \code{\link{library}}.}

\examples{
   is.installed(c("base", "tcltk", "dh", "foobar"))
   is.loadedPackage(c("base", "tcltk", "dh", "foobar"))
}
