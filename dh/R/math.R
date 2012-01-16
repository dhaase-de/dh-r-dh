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