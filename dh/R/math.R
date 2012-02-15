##
## Miscellaneous
##

# transform a point x in a given interval into another interval
"interval.transform" <- function(x, interval.from, interval.to = c(0, 1)) {
   if (!is.numeric(x)) {
      stop("Argument \'x\' must be numeric")
   }
   if (!is.numeric(interval.from) || !is.vector(interval.from) || length(interval.from) != 2) {
      stop("Argument \'interval.from\' must be a numeric vector of length 2")
   }
   if (!is.numeric(interval.to) || !is.vector(interval.to) || length(interval.to) != 2) {
      stop("Argument \'interval.to\' must be a numeric vector of length 2")
   }
   
   (x - interval.from[1]) / (interval.from[2] - interval.from[1]) * (interval.to[2] - interval.to[1]) + interval.to[1]
}

# check if two numeric values are equal (but, unlike 'identical', with numeric fuzz)
"equal" <- function(x, y) {
   isTRUE(all.equal(x, y))
}

##
## Geometry
##

# checks whether a given point is inside a given triangle
"in.triangle" <- function(coordinates) {
   # checks whether a point (p.x,p.y) is inside the triangle (a.x,a.y),
   # (b.x,b.y), (c.x,c.y), whereas coordinates must be
   # c(a.x, a.y, b.x, b.y, c.x, c.y, p.x, p.y)
   
   # checks
   stopifnot(is.numeric(coordinates))
   stopifnot(is.vector(coordinates))
   stopifnot(length(coordinates) == 8)
   
   a.x <- coordinates[1]
   a.y <- coordinates[2]
   b.x <- coordinates[3]
   b.y <- coordinates[4]
   c.x <- coordinates[5]
   c.y <- coordinates[6]
   p.x <- coordinates[7]
   p.y <- coordinates[8]
   
   # reduce problem to check whether the origin (0,0) is inside the triangle
   a.x <- a.x - p.x
   b.x <- b.x - p.x
   c.x <- c.x - p.x
   a.y <- a.y - p.y
   b.y <- b.y - p.y
   c.y <- c.y - p.y
   
   OA <- c(a.x, a.y)
   AB <- c(b.x, b.y) - OA
   AC <- c(c.x, c.y) - OA
   
   M <- matrix(c(AB, AC), ncol = 2, byrow = FALSE)
   x <- solve(M, -OA)
   
   all(x >= 0) && all(x <= 1) && sum(x) <= 1
}

"triangle.area" <- function(triangle) {
   triangle <- t(apply(triangle, 1, function(v) v - triangle[1,]))
   abs(triangle[2, 1] * triangle[3, 2] - triangle[2, 2] * triangle[3, 1]) / 2
}

# gives the orthogonal and normalized vector for 2D vectors
"vector.ortho2D" <- function(P) {
   if (is.matrix(P)) {
      return.matrix <- TRUE
      stopifnot(ncol(P) == 2)
   } else {
      return.matrix <- FALSE
      stopifnot(is.vector(P))
      stopifnot(length(P) == 2)
      P <- matrix(P, nrow = 1, ncol = 2)
   }
   
   P.ortho <- t(apply(P, 1, function(p) { c(p[2], -p[1]) / sqrt(sum(p^2)) } ))
   
   if (return.matrix) {
      rownames(P.ortho) <- rownames(P)
      P.ortho
   } else {
      P.ortho[1,]
   }
}

##
## Rotations
##

"rad2deg" <- function(rad) {
   rad / pi * 180
}

"deg2rad" <- function(deg) {
   deg / 180 * pi
}

# create a 2D rotation matrix
"rot" <- function(rad, deg, drop = TRUE) {
   if (missing(rad) + missing(deg) != 1L) {
      stop("Exactly one argument of 'rad' or 'deg' must be specified")
   }
   
   if (missing(rad)) {
      rad <- deg2rad(deg)
      names(rad) <- paste("deg = ", deg, sep = "")
   } else {
      names(rad) <- paste("rad = ", rad, sep = "")
   }
   
   R <- lapply(rad, function(alpha) {
      sin.alpha <- sin(alpha)
      cos.alpha <- cos(alpha)
      matrix(c(cos.alpha, -sin.alpha, sin.alpha, cos.alpha), nrow = 2, ncol = 2, byrow = TRUE)
   })
   
   if (length(R) == 1L && isTRUE(drop)) {
      R <- R[[1]]
   }
   
   R
}

# calculate angle from 2d rotation matrix
"rot2angle" <- function(rot, only.positive = FALSE) {
   if (!is.list(rot)) {
      rot <- list(rot)
   }
   
   # check arguments
   lapply(rot, function(R) {
      if (!is.matrix(R) || !is.numeric(R) || !identical(dim(R), c(2L, 2L))) {
         stop("Each element of the argument 'rot' must be a 2x2 rotation matrix")
      }
   })
   
   # needed often
   twoPi <- 2 * pi
   
   # calculate the angle for each given rotation matrix 
   angles <- unlist(lapply(rot, function(R) {
      # deal with numerical imprecisions
      R.gr1 <- sapply(as.vector(R), function(x) {
         equal(x, 1) && x > 1
      })
      R[R.gr1] <- 1
      
      R.lem1 <- sapply(as.vector(R), function(x) {
         equal(x, -1) && x < -1
      })
      R[R.lem1] <- -1
      
      # check if R is a valid rotation matrix
      if (!equal(det(R), 1)) {
         warning("R is not a valid rotation matrix")
         return(NA)
      }
      
      angle.sin <- asin(R[2,1])
      angle.cos <- acos(R[1,1])
      angle.sum <- angle.sin + angle.cos
      
      if (equal(angle.sin, angle.cos)) {
         # 0° - 90°
         angle <- angle.sin
      } else if (equal(angle.sum, 0)) {
         # 270° - 360°
         angle <- angle.sin + twoPi
      } else if(equal(angle.sum, pi)) {
         # 90° - 180°
         angle <- angle.cos
      } else {
         # 180° - 270°
         angle <- twoPi - angle.cos
      }
      
      angle
   }))
   
   if (isFALSE(only.positive)) {
      angles[angles > pi / 2] <- angles[angles > pi / 2] - twoPi
   }
   
   angles
}

##
## Probability
##

# multivariate Gaussian density
"mvdnorm" <- function(x, mean, S, S.inv) {
   # check types
   if (!is.vector(mean) || !is.numeric(mean)) {
      stop("Argument 'mean' must be a numeric vector")
   }
   
   # check dimensions
   N <- length(mean)
   
   # get inverse covariance matrix
   if (missing(S.inv)) {
      if (missing(S)) {
         stop("At least one argument of 'S' and 'S.inv' must be specified")
      } else {
         if (!is.matrix(S) || !is.numeric(S) || dim(S)[1] != dim(S)[2] || dim(S)[1] != N) {
            stop("Argument 'S' must be a numeric square matrix with dimensions matching the length of argument 'mean'")
         }
         S.inv <- solve(S)
      }
   } else {
      if (!is.matrix(S.inv) || !is.numeric(S.inv) || dim(S.inv)[1] != dim(S.inv)[2] || dim(S.inv)[1] != N) {
         stop("Argument 'S.inv' must be a numeric square matrix with dimensions matching the length of argument 'mean'")
      }
   }
   
   # convert x to a matrix (to also handle the cases in which more than one point is given)
   x <- as.matrix(x)
   T <- ncol(x)
   if (nrow(x) != N) {
      stop("Argument 'x' must be a vector of length N or a NxT matrix (N: dimension, T: number of points to calculate the density for)")
   }
   x0 <- x - mean
   
   # values that can be precomputed
   S.inv.negHalf <- -0.5 * S.inv
   d.norm <- sqrt((2 * pi) ^ N * det(S.inv))
   
   # point densities
   apply(x0, 2, function(z) exp(z %*% S.inv.negHalf %*% z) / d.norm)
}

"entropy" <- function(...) {
   p <- c(...)
   
   # check arguments
   if (!is.vector(p) || !is.numeric(p) || any(p < 0)) {
      stop("The arguments do not describe a valid discrete probability distribution")
   }
   
   # nomalize to sum(p) = 1 (discrete probability distribution)
   p <- p / sum(p)
   
   # definition of entropy
   sum(-p * log2(p), na.rm = TRUE)
}

"kurtosis" <- function(x, excess = FALSE, ...) {
   mean((x - mean(x))^4) / sd(x)^4 - isTRUE(excess) * 3
}
