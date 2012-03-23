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
