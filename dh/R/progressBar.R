setClass(
   "progressBar",
   
   representation = representation(
      to         = "numeric",
      from       = "numeric",
      by         = "numeric",
      step       = "numeric",
      step.max   = "numeric",
      style      = "character",
      time.start = "POSIXt"
   ),
   
   prototype = prototype(
      to         = 1,
      from       = 1,
      by         = 1,
      step       = 0,
      step.max   = 1,
      style      = "  |{BAR:40:=> }| {METER:|/-\\} {PERCENT} ({STEP}/{STEP.MAX}) {TIME.ELAPSED} {TIME.TOTAL} {TIME.REMAINING}",
      time.start = Sys.time()
   ),
   
   validity = function(object) {
         TRUE
   }
)

setMethod("show", "progressBar",
   function(object) {
      # neccessary values
      percent <- round(100 * object@step / object@step.max)
      
      # times (elapsed, remaining, total)
      time.now <- Sys.time()
      time.elapsed <- as.numeric(difftime(time.now, object@time.start, units = "secs"))
      time.remaining <- ceiling((object@step.max - object@step) * time.elapsed / object@step)
      time.total <- time.elapsed + time.remaining
      
      # progress bar style to be used for layout
      text <- object@style
      
      # progress bar
      text.bar <- gsub("^.*(\\{BAR:[^}]*:[^}]*\\}).*$", "\\1", text)
      text.bar.symbols <- gsub("\\{BAR:[^}]*:([^}]*)\\}", "\\1", text.bar)
      bar.symbols <- unlist(strsplit(text.bar.symbols, ""))
      
      # columns of the terminal
      columns <- as.numeric(Sys.getenv("COLUMNS"))
      if (is.na(columns)) {
         columns <- 40
      }
      
      bar.length <- as.numeric(gsub("\\{BAR:([^}]*):[^}]*\\}", "\\1", text.bar))
      if (bar.length < 1) {
         bars.total <- floor(bar.length * columns)
      } else {
         bars.total <- floor(bar.length)
      }
      
      bars <- round(object@step / object@step.max * bars.total)
      text <- gsub("\\{BAR:[^}]*\\}", paste(c(rep(bar.symbols[1], max(0, bars - 1)), ifelse(bars > 0, bar.symbols[2], ""), rep(bar.symbols[3], bars.total - bars)), collapse = ""), text)
      
      # progress meter
      text.meter <- gsub("^.*(\\{METER:[^}]*\\}).*$", "\\1", text)
      text.meter.symbols <- gsub("\\{METER:([^}]*)\\}", "\\1", text.meter)
      meter.symbols <- unlist(strsplit(text.meter.symbols, ""))
      text <- gsub(text.meter, meter.symbols[(object@step %% length(meter.symbols)) + 1], text, fixed = TRUE)
      
      # percent
      text <- gsub("\\{PERCENT\\}", paste(percent, "%", sep = ""), text)
      
      # steps
      text <- gsub("\\{STEP\\}", object@step, text)
      text <- gsub("\\{STEP\\.MAX\\}", object@step.max, text)
      
      # times
      text <- gsub("\\{TIME\\.ELAPSED\\}", ifelse(is.finite(time.elapsed), sec.dhms(time.elapsed), "?"), text)
      text <- gsub("\\{TIME\\.REMAINING\\}", ifelse(is.finite(time.remaining), sec.dhms(time.remaining), "?"), text)
      text <- gsub("\\{TIME\\.TOTAL\\}", ifelse(is.finite(time.total), sec.dhms(time.total), "?"), text)
      
      # values
      text <- gsub("\\{VALUE\\}", object@from + object@step * object@by, text)
      text <- gsub("\\{VALUE.FROM\\}", object@from, text)
      text <- gsub("\\{VALUE.TO\\}", object@to, text)
      
      # clear the line
      cat("\r", rep(" ", columns), "\r", sep = "")
      
      # show the final text
      if (object@step < object@step.max) {
         cat("\r", text, sep = "")
      }
   }
)

# initializes a progressBar object
setGeneric("init", function(object, silent) standardGeneric("init"))
setMethod("init", c("progressBar", "logical"),
   function(object, silent) {
      object@time.start <- Sys.time()
      object@step <- 0L
      object@step.max <- floor((object@to - object@from) / object@by) + 1
      if (isFALSE(silent)) {
         methods::show(object)
      }
      object
   }
)
setMethod("init", c("progressBar", "missing"),
   function(object, silent) {
      init(object, FALSE)
   }
)

# updates a progressBar object
setGeneric("step", function(object, silent) standardGeneric("step"))
setMethod("step", c("progressBar", "logical"),
   function(object, silent) {
      if (object@step == object@step.max) {
         warning("Max reached")
      } else {
         object@step <- object@step + 1
         if (isFALSE(silent)) {
            methods::show(object)
         }
      }
      object
   }
)
setMethod("step", c("progressBar", "missing"),
   function(object, silent) {
      step(object, FALSE)
   }
)

# wrapper
"progressBar" <- function(to = 1, silent = FALSE, ...) {
   init(new("progressBar", to = to, ...), silent = silent)
}

".progressBar.test" <- function() {
   pb <- new("progressBar", from = -100, to = 320, by = 2.5)
   pb <- init(pb)
   for (i in seq(from = -100, to = 320, by = 2.5)) {
      Sys.sleep(0.075)
      pb <- step(pb)
   }
}