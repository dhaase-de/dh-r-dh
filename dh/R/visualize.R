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
   
   # create derived palettes
   #pal.n$alphajet <- paste(pal.n$jet, fill(as.character(as.hexmode(seq(from = 0, to = 255, length.out = n))), to.length = 2), sep = "")
   pal.n$alphajet <- paste(pal.n$jet, fill(as.character(as.hexmode(c(seq(from = 0, to = 255, length.out = 40 * n / 64), rep(255, 24 * n / 64)))), to.length = 2), sep = "")
   
   # create palette name aliases
   pal.n$grey <- pal.n$gray

   # sort by palette names
   pal.n <- pal.n[sort(names(pal.n))]
   
   pal.n
}

# palettes with 256 colors
pal <- palettes(256)

# colorize image matrix according to palette given in argument 'col'
"colorize" <- function(X, col = pal$gray, normalize = FALSE) {
   if (!(is.matrix(X) || (is.array(X) && length(dim(X)) == 3L && dim(X)[3] %in% 1:4)) || !(is.numeric(X) || is.logical(X))) {
      stop("Argument 'X' must be a numeric/logical matrix/array")
   }
   
   # convert array to matrix if possible
   if (is.array(X) && length(dim(X)) == 3L && dim(X)[3] == 1L) {
      X <- X[,,1]
   }
   
   # convert logicals to numerics
   if (is.logical(X)) {
      mode(X) <- "numeric"
   }
   
   N <- ncol(X)
   T <- nrow(X)
   
   # check range (must be in [0,1]) and normalize if necessary
   X.range <- range(X, na.rm = TRUE, finite = TRUE)
   if (isTRUE(normalize) || X.range[1] < 0 || X.range[2] > 1) {
      X <- interval.transform(X, X.range, c(0, 1))
   }
   
   # colorize image (if X is a matrix colorize according to argument 'col', and
   # if X is an array, colorize according to the channels)
   if (is.matrix(X)) {
      # is.matrix(X): use colors of argument 'col'
      if (is.matrix(col) && identical(dim(col), dim(X)) && is.character(col)) {
         col.rgb <- col2rgb(col, alpha = TRUE) / 255
         if (nrow(col.rgb) == 4) {
            col.alpha <- col.rgb[4,]
         } else {
            col.alpha <- 1
         }
         # y = x * ( (1-alpha)*1 + alpha*color )
         X.col <- matrix(rgb(red = X * (1 - col.alpha + col.rgb[1,] * col.alpha), green = X * (1 - col.alpha + col.rgb[2,] * col.alpha), blue = X * (1 - col.alpha + col.rgb[3,] * col.alpha)), nrow = T, ncol = N)
      } else {
         X.col <- matrix(col[(interval.transform(X, c(0, 1), c(1, length(col))))], nrow = T, ncol = N)
      }
   } else {
      # is.array(X): use channels to colorize image
      Z <- dim(X)[3L]
      if (Z == 2L) {
         X.col <- matrix(rgb(red = X[,,1], green = X[,,1], blue = X[,,1], alpha = X[,,2]), nrow = T, ncol = N)
      } else if (Z == 3L) {
         X.col <- matrix(rgb(red = X[,,1], green = X[,,2], blue = X[,,3]), nrow = T, ncol = N)
      } else if (Z == 4L) {
         X.col <- matrix(rgb(red = X[,,1], green = X[,,2], blue = X[,,3], alpha = X[,,4]), nrow = T, ncol = N)
      } else {
         stop("Unknown array image format - valid are one channel (gray scale image), two channels (gray scale with alpha), three channels (red, green, blue) and four channels (red, green, blue with alpha)")
      }
   }
   
   invisible(X.col)
}

# plot an raster image (also compatible with R < 2.11.0)
"pr" <- function(X, xlab = "", ylab = "", useRaster = TRUE, ...) {
   # check if argument is raster image
   if (!is.matrix(X) || !is.character(X) || any(dim(X) < 1L)) {
      stop("Argument 'X' is not a valid raster image")
   }
   
   # dimensions
   N <- ncol(X)
   T <- nrow(X)
   asp <- T / N
   
   # check if drawing of raster images is supported by R (needs R >= 2.11.0)
   use.raster <- isTRUE(useRaster) && is.loadedPackage("graphics") && exists("rasterImage", "package:graphics")
   
   if (isTRUE(use.raster)) {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), asp = asp, axes = FALSE, xlab = xlab, ylab = ylab, ...)
      rasterImage(X, 0, 0, 1, 1, interpolate = FALSE)
   } else {
      image(t(matrix(along(X), nrow = T, ncol = N)[seq(from = T, to = 1, by = -1),]), col = as.vector(X), axes = FALSE, asp = asp, xlab = xlab, ylab = ylab, ...)
   }
}

# _p_lot _m_atrix - wrapper for plotting and colorizing matrix-like images
"pm" <- function(X, col = pal$gray, normalize = FALSE, xlab = "", ylab = "", useRaster = TRUE, ...) {
   pr(colorize(X, col = col, normalize = normalize), xlab = xlab, ylab = ylab, useRaster = useRaster, ...)
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

# plot partially shaded curve of a function
"scurve" <- function(f, from = -3, to = 3, from.shade = 0, to.shade = 0, N = 101, density = 10, border = NA, col.curve = 1, col.shade = 1, args.curve = list(), args.shade = list(), ...) {
   args.f = list(...)
   g <- function(z) {
      do.call(f, c(list(z), args.f))
   }
   xs <- seq(from = from.shade, to = to.shade, length.out = N)
   ys <- g(xs)
   do.call(curve, c(list(expr = parse(text = "g")[[1]], from = from, to = to, col = col.curve), args.curve))
   if (!equal(from.shade, to.shade)) {
      do.call(polygon, c(list(x = c(xs[1], xs, xs[N]), y = c(0, ys, 0), density = density, col = col.shade, border = border), args.shade))
   }
}

