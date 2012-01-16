# list of useful color palettes
"palettes" <- function(n = 256) {
   stopifnot(is.singleton(n) && is.numeric(n) && n >= 1)
   pal.n <- list(
      "ampel"           = colorRampPalette(c("darkgreen", "green", "yellow", "red", "darkred"))(n),
      "atlas"           = c("#5AC7DC", "#6DD0ED", "#A1D8DB", "#DBEDED", "#F2F8F8", "#93BB3E", "#E8DE85", "#DCB341", "#DB6528", "#B15725"),
      "cm"              = cm.colors(n),
      "gray"            = gray(seq(from = 0, to = 1, length = n)),
      "heat"            = heat.colors(n),
      "jet"             = c(colorRampPalette(c("#00008F", "#0000FF"))(8 * n / 64), colorRampPalette(c("#0000FF", "#00FFFF"))(16 * n / 64), colorRampPalette(c("#00FFFF", "#FFFF00"))(16 * n / 64), colorRampPalette(c("#FFFF00", "#FF0000"))(16 * n / 64), colorRampPalette(c("#FF0000", "#800000"))(8 * n / 64)),
      "log"             = gray((log(seq(from = 0, to = n - 1) + 1) / log(n + 1))),
      "lsb"             = rep(c("black", "white"), length.out = n),
      "msb"             = c(rep("black", floor(n / 2)), rep("white", ceiling(n / 2))),
      "oranges"         = colorRampPalette(c("black", "darkorange", "orange", "yellow", "white"))(n),
      "rainbow"         = rainbow(n),
      "terrain"         = terrain.colors(n),
      "topo"            = topo.colors(n)
   )
   
   # create palette name aliases
   pal.n$grey <- pal.n$gray

   # sort by palette names
   pal.n <- pal.n[sort(names(pal.n))]
   
   pal.n
}

# palettes with 256 colors
pal <- palettes(256)

# colorize image matrix according to palette given in argument 'col'
"colorize" <- function(X, col, normalize = FALSE, alpha = FALSE, split.channels = TRUE) {
   if (!is.matrix(X) || !(is.numeric(X) || is.logical(X))) {
      stop("Argument 'X' must be a numeric or logical matrix")
   }
   
   N <- ncol(X)
   T <- nrow(X)
   
   # check range (must be in [0,1]) and normalize if necessary
   X.range <- range(X, na.rm = TRUE, finite = TRUE)
   if (isTRUE(normalize) || X.range[1] < 0 || X.range[2] > 1) {
      X <- interval.transform(X, X.range, c(0, 1))
   }
   
   # colorize image (according to argument 'col')
   X.col <- matrix(col[(interval.transform(X, c(0, 1), c(1, length(col))))], nrow = T, ncol = N)
   
   if (isTRUE(split.channels)) {
      X.col <- interval.transform(aperm(array(sapply(X.col, col2rgb, alpha = isTRUE(alpha)), dim = c(3 + isTRUE(alpha), T, N)), c(2, 3, 1)), c(0, 255), c(0, 1))
   }
   
   invisible(X.col)
}

# function for plotting a (numeric) matrix
"pm" <- function(X, raster = TRUE, normalize = FALSE, col = pal$gray, xlab = "", ylab = "", ...) {
   if (!is.matrix(X) || !(is.numeric(X) || is.logical(X))) {
      stop("Argument 'X' must be a numeric or logical matrix")
   }
   
   # dimensions
   N <- ncol(X)
   T <- nrow(X)
   asp <- T / N
   
   # check if drawing of raster images is supported by R (needs R >= 2.11.0)
   use.raster <- isTRUE(raster) && is.loadedPackage("graphics") && exists("rasterImage", "package:graphics")
   
   if (isTRUE(use.raster)) {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), asp = asp, axes = FALSE, xlab = xlab, ylab = ylab, ...)
      
      # colorize image according to palette
      X.col <- colorize(X, col = col, normalize = normalize, split.channels = FALSE)
      
      # draw image
      rasterImage(X.col, 0, 0, 1, 1, interpolate = FALSE)
   } else {
      image(t(X[seq(from = T, to = 1, by = -1),]), col = col, axes = FALSE, asp = asp, xlab = xlab, ylab = ylab, ...)
   }
}

# create color from R object
"colorhash" <- function(x, alpha = FALSE) {
   # check if necessary package 'digest' is installed and load it
   if (!is.installed("digest")) {
      stop("To use this function, package 'digest' must be installed")
   }
   if (isFALSE(suppressWarnings(library("digest", character.only = TRUE, logical.return = TRUE)))) {
      stop("Error loading package 'digest'")
   }
   
   x.md5 <- as.integer(digest(x, algo = "md5", raw = TRUE))
   if (isTRUE(alpha)) {
      rgb(red = sum(x.md5[1:4]) %% 255, green = sum(x.md5[5:8]) %% 255, blue = sum(x.md5[9:12]) %% 255, alpha = sum(x.md5[13:16]) %% 255, maxColorValue = 255)
   } else {
      rgb(red = sum(x.md5[1:4]) %% 255, green = sum(x.md5[5:8]) %% 255, blue = sum(x.md5[9:12]) %% 255, maxColorValue = 255)
   }
}