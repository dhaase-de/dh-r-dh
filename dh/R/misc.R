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
"isFALSE" <- function(x) {
   !isTRUE(x)
}

# wrapper for 'match.arg' which behaves similar to 'several.ok=TRUE' but with some more security checks
# (e.g. unmatched args trigger errors, 'args=NULL' triggers an error, result only has unique elements)
"match.args" <- function(args, choices, args.reorder = TRUE, allow.null = FALSE) {
   # match each element of 'arg' to 'choices'
   matches <- unlist(lapply(args, function(arg) {
      match.arg(arg, choices, several.ok = FALSE)
   }))
   
   # check if argument was NULL
   if (length(matches) == 0L) {
      if (isTRUE(allow.null)) {
         return(NULL)
      } else {
         stop("Argument 'args' must not be 'NULL'")
      }
   }
   
   # remove duplicate matches
   matches <- unique(matches)
   
   # reorder matched elements like in argument 'choices'
   if (isTRUE(args.reorder)) {
      matches <- choices[which(choices %in% matches)]
   }
   
   # return matches
   matches
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
