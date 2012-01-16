setClass(
   "progressBar",
   
   representation = representation(
      to         = "numeric",
      from       = "numeric",
      by         = "numeric",
      step       = "numeric",
      style      = "character",
      time.start = "POSIXt"
   ),
   
   prototype = prototype(
      to         = 1,
      from       = 1,
      by         = 1,
      step       = 0,
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
      step.max <- floor((object@to - object@from) / object@by) + 1
      percent <- round(100 * object@step / step.max)
      
      # times (elapsed, remaining, total)
      time.now <- Sys.time()
      time.elapsed <- as.numeric(difftime(time.now, object@time.start, units = "secs"))
      time.remaining <- ceiling((step.max - object@step) * time.elapsed / object@step)
      time.total <- time.elapsed + time.remaining
      
      # progress bar style to be used for layout
      text <- object@style
      
      # progress bar
      text.bar <- gsub("^.*(\\{BAR:[^}]*:[^}]*\\}).*$", "\\1", text)
      text.bar.symbols <- gsub("\\{BAR:[^}]*:([^}]*)\\}", "\\1", text.bar)
      bar.symbols <- unlist(strsplit(text.bar.symbols, ""))
      
      bar.length <- as.numeric(gsub("\\{BAR:([^}]*):[^}]*\\}", "\\1", text.bar))
      if (bar.length < 1) {
         bars.total <- floor(bar.length * as.numeric(Sys.getenv("COLUMNS")))
      } else {
         bars.total <- floor(bar.length)
      }
      
      bars <- round(object@step / step.max * bars.total)
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
      text <- gsub("\\{STEP\\.MAX\\}", step.max, text)
      
      # times
      text <- gsub("\\{TIME\\.ELAPSED\\}", ifelse(is.finite(time.elapsed), sec.dhms(time.elapsed), "?"), text)
      text <- gsub("\\{TIME\\.REMAINING\\}", ifelse(is.finite(time.remaining), sec.dhms(time.remaining), "?"), text)
      text <- gsub("\\{TIME\\.TOTAL\\}", ifelse(is.finite(time.total), sec.dhms(time.total), "?"), text)
      
      # values
      text <- gsub("\\{VALUE\\}", object@from + object@step * object@by, text)
      text <- gsub("\\{VALUE.FROM\\}", object@from, text)
      text <- gsub("\\{VALUE.TO\\}", object@to, text)
      
      # clear the line
      cat("\r", rep(" ", as.numeric(Sys.getenv("COLUMNS"))), "\r", sep = "")
      
      # show the final text
      if (object@step < step.max) {
         cat("\r", text, sep = "")
      }
   }
)

# initializes a progressBar object
"init" <- function(object, show = TRUE) {
   stopifnot(class(object) == "progressBar")
   
   object@time.start <- Sys.time()
   object@step <- 0
   if (show) {
      methods::show(object)
   }
   object
}

# updates a progressBar object
"step" <- function(object, show = TRUE) {
   stopifnot(class(object) == "progressBar")
   
   object@step <- object@step + 1
   if (show) {
      methods::show(object)
   }
   object
}

"progressBar.test" <- function() {
   pb <- new("progressBar", from = 1, to = 3000, by = 2.5)
   pb <- init(pb)
   for (i in seq(from = 1, to = 3000, by = 2.5)) {
      Sys.sleep(0.075)
      pb <- step(pb)
   }
}