\name{in.triangle}

\alias{in.triangle}

\title{Check if a 2D point is inside a given triangle}

\description{Check if a 2D point is inside a given 2D triangle.}

\usage{in.triangle(coordinates)}

\arguments{
   \item{coordinates}{a numeric vector of length 8. The first 6 elements are the x any y coordinates for the triangle (ordered xyxyxy), and the last two elements are the x and y coordinates of the point to be checked.}
}

\value{A logical value indicating if the point is inside (\code{TRUE}) or outside (\code{FALSE}) the triangle.}
