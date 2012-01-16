"pm" <- function(X, col = gray(0:255 / 255), main = desub(X), ...) {
   stopifnot(is.matrix(X))
   N <- ncol(X)
   T <- nrow(X)
   image(t(X[T:1,]), col = col, axes = FALSE, main = main, xlab = N, ylab = T, asp = T / N, ...)
}