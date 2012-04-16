# returns true if 'x' is a vector of length 1
"is.singleton" <- function(x) {
   is.vector(x) && (length(x) == 1)
}

# recycle a vector to a given length and give a warning if it was partially repeated
"recycle" <- function(x, length.out) {
   # check arguments
   if (!is.singleton(length.out) || !is.numeric(length.out)) {
      stop("Argument 'length.out' must be a numeric vector of length one")
   }
   
   # check if 'x' will be partially recycled
   x.length <- length(x)
   if (!equal(length.out / x.length, length.out %/% x.length)) {
      warning("Output length is not an integer multiple of the input vector length")
   }
   
   # recycle values of 'x' to the given length
   rep(x = x, length.out = length.out)
}

# transform a vector of index interval lengths to the "from-to" form
"indexIntervalLengths.toEndpoints" <- function(lengths, return.list = FALSE) {
   if (!is.vector(lengths) || !is.numeric(lengths) || !all(lengths >= 1)) {
      stop("Argument 'lengths' must be a numeric vector with entries >= 1")
   }
   lengths <- as.integer(lengths)
   N <- length(lengths)
   to <- cumsum(lengths)
   from <- c(1, to[-N] + 1)
   if (isTRUE(return.list)) {
      lapply(seq(N), function(n) list("from" = from[n], "to" = to[n]))
   } else {
      matrix(c(from, to), ncol = 2, byrow = FALSE, dimnames = list(seq(N), c("from", "to")))
   }
}

# return a (sorted) subset of a numeric vector 'v' which has a length of 'length.out' and whose elements are as equidistant as possible
"equidistantSubset" <- function(v, length.out) {
   # sort vector v
   v <- sort(v)
   N <- length(v)
   
   # if output length is not smaller than the input length, do nothing
   if (length.out >= N) {
      return(v)
   }
   
   # ideal ouput vector (equidistant values between min and max)
   v.ideal <- seq(from = v[1], to = v[N], length.out = length.out)
   v.selected <- rep(FALSE, N)
   
   # repeatedly select next best element
   while (sum(v.selected) < length.out) {
      dists <- outer(v[!v.selected], v.ideal, function(a, b) abs(a - b))
      dists.min <- which(dists == min(dists), arr.ind = TRUE)[1,]
      
      v.selected[!v.selected][dists.min[1]] <- TRUE
      v.ideal <- v.ideal[-dists.min[2]]
   }
   
   # return vector of desired length
   v[v.selected]
}
