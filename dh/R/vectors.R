# returns true if 'x' is a vector of length 1
"is.singleton" <- function(x) {
   is.vector(x) && (length(x) == 1)
}