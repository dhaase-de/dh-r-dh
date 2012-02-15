# returns true if 'x' is a vector of length 1
"is.singleton" <- function(x) {
   is.vector(x) && (length(x) == 1)
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
