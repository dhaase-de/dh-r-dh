# source file 'main.R'
"sm" <- function(dir = getwd()) {
   filename <- paste(dir, "/", "main.R", sep = "")
   if (file.exists(filename)) {
      setwd(dir)
      source("main.R")
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
