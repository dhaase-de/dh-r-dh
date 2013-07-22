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

##
## DBLP query functions
##

# turn phrase into query string
"dblp.phraseToQueryString" <- function(phrase, year = NA) {
   query <- tolower(phrase)
   
   # remove leading and trailing white space
   query <- gsub("(^\\s+|\\s+$)", "", query)
   
   # treat single and multi-word queries differently
   if (length(grep("\\s", query)) == 0L) {
      # single word queries must be quoted
      query <- paste0("\"", query, "\"")
   } else {   
      # replace inter-word whitespace by dots (results in match as phrase)
      query <- gsub("\\s", ".", query)
   }
   
   # add year to query if given
   if (is.finite(year)) {
      query <- paste0(query, " ce:year:", year, ":*")
   }
   
   query
}

# count DBLP entries for a given query
"dblp.query" <- function(queries = character(0)) {
   library(XML)
   
   # run DBLP query and parse returned XML document
   counts <- sapply(queries, function(query) {
      xml <- XML::xmlParse(
         file =  paste0(
            "http://dblp.org/search/api/",   # base URL
            "?q=", query,                    # query string
            "&h=0&c=0&f=0&format=xml"        # retrieve only result count (as XML)
         ),
         isURL = TRUE,
         getDTD = FALSE
      )
   
      # extract result count
      as.numeric(XML::xmlAttrs(XML::xmlRoot(xml)[["hits"]])["total"])
   })
   names(counts) <- queries
   
   counts
}

# count DBLP hits for a given phrase
"dblp.count" <- function(phrase, year = NA) {
   dblp.query(dblp.phraseToQueryString(phrase, year = year))
}

# count DBLP hits for a given phrase by year
"dblp.count.byYear" <- function(phrase, years = 1990:2013, do.plot = FALSE, ...) {
   counts <- sapply(years, function(year) {
      dblp.count(phrase = phrase, year = year)
   })
   names(counts) <- years
   
   if (isTRUE(do.plot)) {
      dblp.count.byYear.plot(counts.list = counts, ...)
   }
   
   counts
}

# plot several DBLP-by-year count results
"dblp.count.byYear.plot" <- function(counts.list = list(), colors = numeric(0), normalization = "none") {
   # check argument 'counts.list'
   if (!is.list(counts.list)) {
      counts.list <- list(counts.list)
   }
   if (length(counts.list) == 0L) {
      return
   }
   if (is.null(names(counts.list))) {
      names(counts.list) <- seq(length(counts.list))
   }
   
   if (length(colors) == 0L) {
      colors <- seq(length(counts.list)) + 1
   }
   
   # check argument 'normalization'
   normalization <- match.arg(normalization, c("none", "max", "sum"))
   
   # plot delta x (in years, smaller values produce smoother curves)
   x.subUnits <- 10
   x.gaussRadius <- 45
   
   # plot range
   x.int <- seq(from = min(sapply(counts.list, function(l) as.numeric(names(l)))), to = max(sapply(counts.list, function(l) as.numeric(names(l)))) + 1, by = 1) - 0.5
   x.sub <- seq(from = min(x.int), to = max(x.int), by = 1 / x.subUnits)
   
   # label positions
   x.at <- ceiling(x.int[-length(x.int)])
   
   # normalize counts
   counts.list <- lapply(counts.list, function(l) {
      if (normalization == "none") {
         l
      } else if (normalization == "max") {
         l / max(l)
      } else if (normalization == "sum") {
         l / sum(l)
      }
   })
   y.min <- min(as.numeric(unlist(counts.list)))
   y.max <- max(as.numeric(unlist(counts.list)))
   
   # get label of y axis (depends on normnalization method)
   if (normalization == "none") {
      ylab <- "Count"
   } else if (normalization == "max") {
      ylab <- "Relative Count"
   } else if (normalization == "sum") {
      ylab <- "Density"
   }
   
   # plot base graph
   plot(x = x.int, y = NULL, xlim = range(x.int), ylim = c(min(0, y.min), y.max), xlab = "Year", ylab = ylab, main = "Publications by Year", xaxt = "n")
   grid(nx = NA, ny = NULL, lwd = 1, lty = 2, col = "gray")
   abline(v = x.at, lwd = 1, lty = 2, col = "gray")
   axis(1, at = x.at, labels = x.at)
   
   # plot year counts
   color.index <- 1
   lapply(counts.list, function(l) {
      lines(x = x.int, y = c(l, NA), t = "s", col = colors[color.index], lwd = 2, lty = 2)
      y <- rep(l, each = x.subUnits)
      
      # smooth line
      y <- c(rep(y[1], x.gaussRadius), y, rep(y[length(y)], x.gaussRadius))
      y <- dh::rgaussian(y, radius = x.gaussRadius)
      y <- y[seq(x.gaussRadius + 1, length(y) - x.gaussRadius)]
      lines(x = x.sub[-length(x.sub)], y = y, type = "l", col = colors[color.index], lwd = 4)
      
      # next color
      color.index <<- color.index + 1
   })
   
   # draw legend
   legend(
      x = "topleft",
      legend = names(counts.list),
      col = colors,
      lty = 1,
      lwd = 4,
      bg = "white"
   )
}

"dblp.game" <- function(players = c("P1", "P2"), best.of = 3, score.type = "peak", score.upperFactor = 10, years = 1990:2012, timer.halfLife = 30) {
   # type of the scores
   # "peak":      peak value of the counts
   # "sum":       sum of all counts
   # "absdiff":   absolute difference between count in last and first year
   # "reldiff":   absolute difference between count in last and first year
   score.type <- match.arg(score.type, c("peak", "sum", "diff"))
   
   ##
   ## helper functions
   ##
   
   # plot scoreboard
   "plot.scoreboard" <- function() {
      par(mfrow = c(1, 2))
      
      # plot area
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
      
      # "best of" info
      text(x = 0.5, y = 1.0, labels = paste0("Best of ", best.of, " (", points.needed, " to win)"), adj = c(0.5, 1.0))
      
      # points
      text(x = 0.4, y = 0.7, labels = points[1], cex = 6, adj = c(1.0, 0.5))
      text(x = 0.6, y = 0.7, labels = points[2], cex = 6, adj = c(0.0, 0.5))
      text(x = 0.5, y = 0.7, labels = "-", cex = 6, adj = c(0.5, 0.5))
      
      # player names
      text(x = 0.325, y = 0.5, labels = players[1], cex = 1, adj = c(0.5, 0.5))
      text(x = 0.675, y = 0.5, labels = players[2], cex = 1, adj = c(0.5, 0.5))
      
      if (length(countss) == 0L) {
         plot(NULL, axes = FALSE, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
      } else {
         dblp.count.byYear.plot(countss)
      }
   }
   
   # return index of next player
   "player.next" <- function(player) {
      3 - player
   }
   
   ##
   ## init
   ##
   
   points <- c(0, 0)
   points.needed <- floor(max(best.of, 1) / 2) + 1
   best.of <- 2 * points.needed - 1
   
   ##
   ## main loops
   ##
   
   round <- 0
   repeat {
      #cat("Round", sum(points) + 1, "\n")
      #cat(players[1], " ", points[1], "-", points[2], " ", players[2], "\n", sep = "")
      
      round <- round + 1
      player.turn <- (round - 1) %% 2 + 1
      score.range <- c(-Inf, Inf)
      scores <- numeric(0)
      countss <- list()
      
      repeat {
         # show scoreboard
         plot.scoreboard()
         
         # get phrase from current player
         if (length(countss) == 0L) {
            score.info <- " (score must be in [1, Inf))"
         } else {
            score.info <- paste0(" (score must be in [", scores[length(scores)] + 1, ",", round(score.upperFactor * scores[length(scores)]), "]")
         }
         
         timer <- round(system.time(phrase <- readline(paste0(players[player.turn], score.info, ": ")))["elapsed"], 2)
         timer <- max(timer - 5, 0)
         if (is.finite(timer.halfLife)) {
            timer.factor <- 2^(-timer / timer.halfLife)
         } else {
            timer.factor <- 1
         }
         
         # get raw publication counts for entered phrase
         counts.raw <- dblp.count.byYear(phrase = phrase, years = years)
         
         # turn counts into scores depending on match rules
         if (score.type == "peak") {
            counts <- counts.raw
            score <- max(counts.raw)
         } else if (score.type == "sum") {
            counts <- cumsum(counts.raw)
            score <- counts[length(counts)]
         } else if (score.type == "diff") {
            counts <- counts.raw - counts.raw[1]
            score <- counts[length(counts)]
         }
         
         # save scores and counts
         countss[[length(countss) + 1]] <- counts
         names(countss)[length(countss)] <- phrase
         scores <- c(scores, score)
         
         # switch current player
         player.turn <- player.next(player.turn)
         
         # check if one player won this set
         win <- FALSE
         if (length(scores) == 1L) {
            win <- ((score.type %in% c("peak", "sum", "diff")) && score <= 0L)
            score.lower <- 1
            score.upper <- Inf
         } else {
            N <- length(scores)
            score.lower <- scores[N - 1] + 1
            score.upper <- round(timer.factor * ((score.upperFactor * scores[N - 1]) - score.lower) + score.lower)
            win <- ((scores[N] < score.lower) || (scores[N] > score.upper))
         }
         
         cat("(phrase = \"", phrase, "\", score = ", score, ", time = ", timer, "s, time corrected score interval = [", score.lower, ",", score.upper, "])\n", sep = "")
         
         if (isTRUE(win)) {
            plot.scoreboard()
            points[player.turn] <- points[player.turn] + 1
            readline(paste0("--> ", players[player.turn], " wins this round!"))
            break
         }
      }
      
      # check if game is over
      if (any(points >= points.needed)) {
         plot.scoreboard()
         readline(paste0("--> ", players[player.turn], " wins the match!"))
         break
      }
   }
}

