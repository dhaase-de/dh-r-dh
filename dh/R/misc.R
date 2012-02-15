# source file 'main.R'
"sm" <- function(dir = getwd(), return = FALSE) {
   filename <- paste(dir, "/", "main.R", sep = "")
   if (file.exists(filename)) {
      oldwd <- getwd()
      setwd(dir)
      source("main.R")
      if (isTRUE(return)) {
         setwd(oldwd)
      }
   } else {
      stop(paste("File '", filename, "' not found", sep = ""))
   }
}

# counterpart of 'isTRUE'
"isFALSE" <- function(...) {
   !isTRUE(...)
}

# alias for 'seq_along' or 'seq(along=)'
"along" <- function(along.with) {
   seq_along(along.with = along.with)
}

# all finite elements
"finite" <- function(x) {
   x[is.finite(x)]
}

# clip values outside of a certain range
"clip" <- function(x, low = 0, high = 1) {
   x[x < low] <- low
   x[x > high] <- high
   x
}
