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

# print name and value of variables
"out" <- function(object, ...) {
   cat(deparse(substitute(object)), " = ", object, "\n", sep = "")
   if (length(list(...)) > 0L) {
      out(...)
   }
}