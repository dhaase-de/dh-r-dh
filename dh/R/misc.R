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

# save single R object to file of same name (+ ".RData")
"sv" <- function(object, directory = getwd(), force = FALSE, quiet = FALSE) {
   # get object name and remove quotes from object name (this happens if object is a character string)
   object.name <- gsub("[\" ]", "", deparse(substitute(object)))
   
   # check if object exists
   if (isFALSE(exists(object.name, envir = parent.frame()))) {
      stop("Object '", object.name, "' not found")
   }
   
   # check if object name contains invalid characters
   if (isFALSE(grepl("^([A-Za-z0-9.])*$", object.name))) {
      stop("Object name '", object.name, "' contains invalid characters")
   }
   
   # construct object filename and path
   object.filename <- paste0(object.name, ".RData")
   object.path <- file.path(directory, object.filename)
   
   # create target directory if it does not already exist
   if (!file.exists(directory)) {
      dir.create(directory)
      message("Target directory '", directory, "' did not exist and was created")
   }
   
   # check if file already exists
   if (file.exists(object.path) && isFALSE(force)) {
      answer <- readline(paste0("Target file \'", object.path, "\' already exists, overwrite? [y/N] "))
      if (length(answer) != 1 || strlen(answer) == 0 || !(substr(answer, 1, 1) %in% c("y", "Y"))) {
         if (isFALSE(quiet)) {
            message("Object '", object.name, "' has NOT been saved")
         }
         return(invisible(NULL))
      }
   }
   
   # save object
   save(list = object.name, file = object.path, envir = parent.frame())
   
   # show some information
   if (isFALSE(quiet)) {
      # object size
      object.objectsize <- unclass(object.size(x = get(x = object.name, envir = parent.frame())))
      object.sizeText <- bytes.toHumanReadable(object.objectsize)
      
      # get size of the written file
      object.filesize <- file.info(object.path)$size
      if (is.na(object.filesize)) {
         object.filesizeText <- "unknown file size"
      } else {
         object.filesizeText <- paste0(bytes.toHumanReadable(object.filesize), " = ", round(100 * object.filesize / object.objectsize), "%")
      }
      
      # show file path, object size and file size
      message("Object '", object.name, "' (", object.sizeText, ") has been saved to file '", object.path, "' (", object.filesizeText, ")")
   }
   
   # return object path
   invisible(object.path)
}

# load R object save in file of same name (+ ".RData")
"ld" <- function(objects.filenames, quiet = FALSE) {
   # check argument 'objects.filenames'
   if (!is.vector(objects.filenames) || !is.character(objects.filenames)) {
      stop("Argument 'objects.filenames' must be a character vector")
   }
   
   # globbing
   objects.filenames <- Sys.glob(objects.filenames)
   
   # for each filename, load the object
   for (object.filename in objects.filenames) {
      # get object name from filename
      object.name <- gsub(".RData$", "", basename(object.filename))
      
      # load .RData file into new (empty) environment
      load.envir <- new.env()
      load(file = object.filename, envir = load.envir)
      
      # try to find an object with matching name in this environment
      object <- try(get(x = object.name, envir = load.envir, inherits = FALSE), silent = TRUE)
      
      # show the result
      if (inherits(object, "try-error")) {
         stop("Object '", object.name, "' was not found in file '", object.filename, "'")
      } else {
         assign(x = object.name, value = object, envir = parent.frame())
         if (isFALSE(quiet)) {
            message("Object '", object.name, "' has been loaded from file '", object.filename, "'")
         }
      }
      
      # check if enviroment has more than one object
      if (length(ls(load.envir)) > 1L) {
         warning("More than one object was found in file '", object.filename, "', only loading object '", object.name, "'")
      }
   }
   
   invisible(NULL)
}
