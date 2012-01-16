\name{rotation2D}

\alias{rot}
\alias{rot2angle}

\title{2D rotation matrices}

\description{Functions for 2D rotation matrices.}

\usage{
   rot(rad, deg, drop = TRUE)
   rot2angle(rot, only.positive = FALSE)
}

\arguments{
   \item{rad}{A numeric object that can be coerced to a vector giving the angles in radian.}
   \item{deg}{A numeric object that can be coerced to a vector giving the angles in degrees.}
   \item{drop}{A logical indicating whether for only one angle (specified via \code{rad} or \code{deg}) the corresponding matrix should be returned within a list (\code{FALSE}) or as a pure matrix (\code{TRUE}).}
   \item{rot}{A 2D rotation matrix or a list of such matrices to be converted to rotation angles.}
   \item{only.positive}{A logical indicating the interval for the returned angles. If \code{FALSE} (default), the results are from the interval \eqn{[-\pi,\pi]}{[-pi,pi]}, otherwise from \eqn{[0,2\pi]}{[0,2*pi]}.}
}

\details{
   \code{rot} returns rotation matrices for given 2D rotation angles.
   
   \code{rot2angle} calculates the 2D rotation angles for given rotation matrices.
}

\note{For the function \code{rot} exactly one argument of \code{rad} and \code{deg} must be specified.}

\value{
   The function \code{rot} returns a list of \eqn{2\times2}{2x2} matrices, one for each angle specified by the argument \code{rad} or \code{deg}.
   Note that if only one angle is given and \code{drop = TRUE} (default), a pure matrix (not a list of matrices) is returned.
   
   For \code{rot2angle}, a numeric vector giving the angles for each of the specified rotation matrices.
}

\examples{
   rot(deg = c(45, 90))
   R <- rot(rad = pi / 2)
   rot2angle(R)
}
