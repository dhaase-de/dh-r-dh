# list objects and their size
"lso" <- function() {
   objects <- ls(1)
   objects.data <- matrix("", nrow = length(objects), ncol = 4)
   colnames(objects.data) <- c("Object", "Class", "Dimension", "Size")
   for (i in seq(along.with = objects)) {
      object.name <- objects[i]
      objects.data[i,] <- c(
         object.name,
         paste(class(get(object.name)), collapse = ","),
         ifelse(
            is.vector(get(object.name)),
            paste("<", length(get(object.name)), ">", sep = ""),
            ifelse(
               is.array(get(object.name)),
               paste("<", paste(dim(get(object.name)), collapse = " x "), ">", sep = ""),
               ""
            )
         ),
         unclass(object.size(get(object.name)))
      )
   }
   
   objects.sizes <- as.numeric(objects.data[,4])
   total.size <- sum(objects.sizes)
   objects.data <- data.frame(objects.data[order(objects.sizes, decreasing = TRUE),], stringsAsFactors = FALSE)
   
   for (i in seq(along.with = objects)) {
      object.size <- as.numeric(objects.data[i,4])
      objects.data[i,4] <- paste(
         round(object.size / 1024 / 1024, 2),
         " MB (",
         round(100 * object.size / total.size),
         "%)",
         sep = ""
      )
   }
   
   objects.data
}

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
"out" <- function(object, ...) {
   cat(deparse(substitute(object)), " = ", object, "\n", sep = "")
   if (length(list(...)) > 0L) {
      out(...)
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