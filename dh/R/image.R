.imageFormats <- list(
   "jpeg" = list(
      "package" = "jpeg",
      "extensions" = c("jpg", "jpeg")
   ),
   "png" = list(
      "package" = "png",
      "extensions" = "png"
   ),
   "pnm" = list(
      "package" = "pixmap",
      "extensions" = c("pnm", "pbm", "pgm", "ppm")
   ),
   "tiff" = list(
      "package" = "rtiff",
      "extensions" = c("tif", "tiff")
   )
)

"readImage" <- function(filenames, gray = FALSE, extensions = NULL, list = FALSE, ...) {
   # check arguments
   filenames.glob <- Sys.glob(filenames)
   if (length(extensions) > 1L && (length(filenames.glob) != length(filenames))) {
      stop("If filename globbing is used, argument 'extensions' must be NULL or a character vector of length one")
   }
   filenames <- filenames.glob
   
   if (!is.vector(filenames) || !is.character(filenames)) {
      stop("Argument 'filenames' must be a character vector")
   }
   
   # recycle 'extensions'
   extensions <- rep(extensions, len = length(filenames))
   
   # check if files exist
   if (any(invalid <- !sapply(filenames, file.exists))) {
      stop("The file(s) '", paste(filenames[invalid], collapse = "', '"), "' can not be found")
   }
   
   # load images
   images <- lapply(seq(length(filenames)), function(filename.index) {
      filename <- filenames[filename.index]
      
      # get file extension
      if (is.null(extensions[filename.index]) || identical(extensions[filename.index], "")) {
         extension <- tolower(file.extension(filename))
      } else {
         extension <- extensions[filename.index]
      }
      
      # match file extension with valid image formats
      matches <- unlist(lapply(.imageFormats, function(format) extension %in% format$extensions))
      
      # check if format is supported
      if (all(!matches)) {
         stop("Unsupported image format '", extension, "' for file '", filename, "'")
      }
      format <- .imageFormats[[which(matches)[1]]]
      
      # check if necessary package is installed
      if (!is.installed(format$package)) {
         stop("To load image format '", extension, "', package '", format$package, "' must be installed")
      }
      
      # load package
      if (isFALSE(suppressWarnings(library(format$package, character.only = TRUE, logical.return = TRUE)))) {
         stop("Error loading package '", format$package, "'")
      }
      
      # call the package specific functions for image loading
      if (identical(format$package, "jpeg")) {
         capture.output(image <- readJPEG(filename, ...))
      } else if (identical(format$package, "png")) {
         capture.output(image <- readPNG(filename, ...))
      } else if (identical(format$package, "pixmap")) {
         capture.output(image <- read.pnm(filename, ...))
         if (inherits(image, "pixmapGrey")) {
            image <- image@grey
         } else if (inherits(image, "pixmapRGB")) {
            image <- array(c(as.vector(image@red), as.vector(image@green), as.vector(image@blue)), dim = c(image@size, 3))
         }
      } else if (identical(format$package, "rtiff")) {
         capture.output(image <- readTiff(filename, ...))
         if (inherits(image, "pixmapGrey")) {
            image <- image@grey
         } else if (inherits(image, "pixmapRGB")) {
            image <- array(c(as.vector(image@red), as.vector(image@green), as.vector(image@blue)), dim = c(image@size, 3))
         }
      } else {
         stop("Loading images with package '", format$package, "' is not supported")
      }
      
      # convert images to gray scale if desired
      if (isTRUE(gray)) {
         if (is.matrix(image)) {
            # already grayscale
         } else if (is.array(image) && dim(image)[3] == 3) {
            image <- (image[,,1] + image[,,2] + image[,,3]) / 3
         } else {
            stop("Can not convert image '", filename, "' to gray scale because of unknown array dimension (", paste(dim(image), collapse = ", "), ")")
         }
      }
      
      image
   })
   
   # use the filenames of the images as names of the list
   names(images) <- filenames
   
   # if only one image, do not return a list
   if (isFALSE(list) && length(images) == 1L) {
      images <- images[[1L]]
   }
   
   # return loaded images
   invisible(images)
}

"writeImage" <- function(images, filenames, gray = FALSE, extensions = NULL, overwrite = FALSE, ...) {
   # check arguments
   if (!is.list(images)) {
      images <- list(images)
   }
   if (any(sapply(images, function(image) { !(is.numeric(image) || is.character(image)) || !(is.matrix(image) || is.array(image)) } ))) {
      stop("Argument 'images' must be a numeric/character matrix/array or a list of such")
   }
   if (!is.vector(filenames) || !is.character(filenames) || length(filenames) != length(images)) {
      stop("Argument 'filenames' must be a character vector with a length identical to the number of images")
   }
   filenames <- path.expand(filenames)
   
   # recycle 'extensions'
   extensions <- rep(extensions, len = length(filenames))
   
   # check if files exist
   if (isFALSE(overwrite) && any(invalid <- sapply(filenames, file.exists))) {
      stop("The file(s) '", paste(filenames[invalid], collapse = "', '"), "' already exist(s), you may use 'overwrite=TRUE'")
   }
   
   # save images
   sapply(seq(length(filenames)), function(filename.index) {
      image <- images[[filename.index]]
      filename <- filenames[filename.index]
      
      # convert raster image to array
      if (is.matrix(image) && is.character(image)) {
         image <- aperm(array(col2rgb(image) / 255, c(3, nrow(image), ncol(image))), c(2, 3, 1))
      }
      
      # get file extension
      if (is.null(extensions[filename.index]) || identical(extensions[filename.index], "")) {
         extension <- tolower(file.extension(filename))
      } else {
         extension <- extensions[filename.index]
      }
      
      # match file extension with valid image formats
      matches <- unlist(lapply(.imageFormats, function(format) extension %in% format$extensions))
      
      # check if format is supported
      if (all(!matches)) {
         stop("Unsupported image format '", extension, "' for file '", filename, "'")
      }
      format <- .imageFormats[[which(matches)[1]]]
      
      # check if necessary package is installed
      if (!is.installed(format$package)) {
         stop("To save image format '", extension, "', package '", format$package, "' must be installed")
      }
      
      # load package
      if (isFALSE(suppressWarnings(library(format$package, character.only = TRUE, logical.return = TRUE)))) {
         stop("Error loading package '", format$package, "'")
      }
      
      # call the package specific functions for image writing
      if (identical(format$package, "jpeg")) {
         capture.output(writeJPEG(image = image, target = filename, ...))
      } else if (identical(format$package, "png")) {
         capture.output(writePNG(image = image, target = filename, ...))
      } else if (identical(format$package, "pixmap")) {
         if (is.matrix(image)) {
            image <- pixmapGrey(image)
         } else {
            image <- pixmapRGB(image)
         }
         capture.output(write.pnm(object = image, file = filename, ...))
      } else if (identical(format$package, "rtiff")) {
         if (is.matrix(image)) {
            image <- pixmapGrey(image)
         } else {
            image <- pixmapRGB(image)
         }
         writeTiff(pixmap = image, fn = filename)
      } else {
         stop("Saving images with package '", format$package, "' is not supported")
      }
   })
   
   invisible(NULL)
}

