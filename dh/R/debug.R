# creates an image of random 2d Gaussian densities
"gaussians2d" <- function(N = 20, X = 100, Y = 100, S.factor = 1, plot = FALSE) {
   # check if suggested package 'clusterGeneration' is installed
   if (!is.installed("clusterGeneration")) {
      warning("Install package 'clusterGeneration' to get better random covariance matrices")
   } else {
      # load package
      if (isFALSE(suppressWarnings(library("clusterGeneration", logical.return = TRUE)))) {
         stop("Error loading package 'clusterGeneration'")
      }
   }
   
   D <- matrix(0, nrow = Y, ncol = X)
   
   xs <- t(expand.grid(x = seq(X), y = seq(Y)))
   means <- matrix(NA, nrow = 2, ncol = N)
   Ss <- array(NA, dim = c(2, 2, N))
   
   for (n in seq(length = N)) {
      means[,n] <- runif(2, min = 1, max = c(X, Y))
      if (is.loadedPackage("clusterGeneration")) {
         # generate random covriance matrix
         Ss[,,n] <- genPositiveDefMat(dim = 2, covMethod = "onion", rangeVar = min(X, Y) * S.factor * c(0.5, 1))$Sigma
      } else {
         # create a very simple covariance matrix
         Ss[,,n] <- diag(runif(2, min = c(X, Y) * S.factor / 2, max = c(X, Y) * S.factor))
      }
      
      d <- mvdnorm(xs, means[,n], Ss[,,n])
      D <- D + matrix(d, nrow = Y, ncol = X, byrow = TRUE)
   }
   
   if (isTRUE(plot)) {
      pm(D, col = pal$jet)
   }
   
   invisible(D)
}

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

# print name and value of variables
# print name and value of variables
"out" <- function(...) {
   "args.names" <- function(object, ...) {
      if (missing(object)) {
         NULL
      } else {
         object.name <- deparse(substitute(object))
         
         # deal with line breaks in object name (when object name is too long)
         if (length(object.name) > 1L) {
            object.name <- paste(object.name[1], "...", sep = "")
         }
         
         if (length(list(...)) > 0L) {
            c(object.name, args.names(...))
         } else {
            object.name
         }
      }
   }
   
   # get arguments and their names
   args <- list(...)
   names <- args.names(...)
   names.lengths <- strlen(names)
   if (!is.null(names.lengths)) {
      names.maxLength <- max(names.lengths)
      cat("\n")
   }
   
   # show objects
   for (i in seq(along.with = names)) {
      if (names.lengths[i] < names.maxLength) {
         name.sep <- " "
      } else {
         name.sep <- ""
      }
      cat(">>> ", fill(paste(names[i], name.sep, sep = ""), to.length = names.maxLength, with.character = ".", from.left = FALSE), " <<< ", sep = "")
      str(args[[i]], nest.lev = 1, indent.str = "      ")
   }
   
   if (!is.null(names.lengths)) {
      cat("\n")
   }
}

# 'str' with maximum level 1
"st1" <- function(...) {
   str(..., max.level = 1L)
}

# wait for key press
"wait" <- function() {
   readline("Press [RETURN] to continue...")
}

# test matrix
M <- matrix(1:16, nrow = 4)
