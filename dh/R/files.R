"file.extension" <- function(filenames) {
   # check arguments
   if (!is.vector(filenames) || !is.character(filenames)) {
      stop("Argument 'filenames' must be a character vector")
   }
   
   basenames <- basename(filenames)
   
   # main pattern to find extension
   pattern <- "^.+\\.([^\\.]+)$"
   
   # match pattern with filenames
   extensions <- gsub(pattern, "\\1", basenames)
   matches <- grep(pattern, basenames)
   
   # set extension to "" for filenames without a match
   extensions[-matches] <- ""
   
   # the names of the extension vector are the original filenames
   names(extensions) <- filenames
   
   extensions
}

"file.extension<-" <- function(filenames, value) {
   # check arguments
   if (!is.vector(filenames) || !is.character(filenames)) {
      stop("Target object must be a character vector")
   }
   if (!is.vector(value) || !is.character(value)) {
      stop("Value must be a character vector")
   }
   
   values <- rep(value, length.out = length(filenames))
   sapply(seq(length(filenames)), function(i) {
      filename <- filenames[i]
      sub(paste(file.extension(filename), "$", sep = ""), values[i], filename)
   })
}

# returns TRUE iff file exists and it is a directory
"is.directory" <- function(...) {
   args <- list(...)
   args <- lapply(args, as.character)
   unlist(lapply(do.call(file.info, args)$isdir, isTRUE))
}

"dirname<-" <- function(filenames, value) {
   # check arguments
   if (!is.vector(filenames) || !is.character(filenames)) {
      stop("Target object must be a character vector")
   }
   if (!is.vector(value) || !is.character(value)) {
      stop("Value must be a character vector")
   }
   
   values <- rep(value, length.out = length(filenames))
   sapply(seq(length(filenames)), function(i) {
      filename <- filenames[i]
      sub(paste("^", dirname(filename), sep = ""), values[i], filename)
   })
}

"bytes.toHumanReadable" <- function(bytes, digits = 2, decimal = TRUE) {
   if (isTRUE(decimal)) {
      base <- 1000
      prefixes <- c("", "k", "M", "G", "T", "P", "E")
   } else {
      base <- 1024
      prefixes <- c("", "ki", "Mi", "Gi", "Ti", "Pi", "Ei")
   }
   
   log.int <- floor(log(bytes) / log(base))
   
   # handle zero bytes and too large numbers
   log.int[!is.finite(log.int) | log.int + 1 > length(prefixes)] <- 0
   
   values <- round(bytes / base^log.int, digits = digits)
   
   paste(values, " ", prefixes[log.int + 1], "B", sep = "")
}
