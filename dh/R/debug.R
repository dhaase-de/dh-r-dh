# list objects and their size
"objects.sizes" <- function(environment = 1, plot = TRUE, threshold = 0.05) {
   objects <- ls(name = environment)
   sizes <- sapply(objects, function(object.name) unclass(object.size(get(object.name))))
   
   if (isTRUE(plot)) {
      sizes.relative <- sizes / sum(sizes)
      sizes.plot <- c(sizes[sizes.relative >= threshold], sum(sizes[sizes.relative < threshold]))
      labels.plot <- c(objects[sizes.relative >= threshold], paste("(", sum(sizes.relative < threshold), " object(s) < ", threshold * 100, "%)", sep = ""))
      pie(sizes.plot, paste(labels.plot, " (", bytes.toHumanReadable(sizes.plot), ")", sep = ""), main = "Object Sizes", sub = paste("Total: ", length(objects), " object(s), ", bytes.toHumanReadable(sum(sizes)), sep = ""))
   }
   
   invisible(sizes)
}

# test matrix
M <- matrix(1:16, nrow = 4)

# print name and value of variables
"out" <- function(...) {
   "args.names" <- function(object, ...) {
      object.name <- deparse(substitute(object))
      if (length(list(...)) > 0L) {
         c(object.name, args.names(...))
      } else {
         object.name
      }
   }
   
   # get arguments and their names
   args <- list(...)
   names <- args.names(...)
   names.lengths <- strlen(names)
   names.maxLength <- max(names.lengths)
   
   # show objects
   for (i in seq(length(names))) {
      if (names.lengths[i] < names.maxLength) {
         name.sep <- " "
      } else {
         name.sep <- ""
      }
      cat(">>> ", fill(paste(names[i], name.sep, sep = ""), to.length = names.maxLength, with.character = ".", from.left = FALSE), " <<< ", sep = "")
      str(args[[i]], nest.lev = 1, indent.str = "      ")
   }
}

# counterpart of 'isTRUE'
"isFALSE" <- function(...) {
   !isTRUE(...)
}

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

# wait for key press
"wait" <- function() {
   readline("Press [RETURN] to continue...")
}