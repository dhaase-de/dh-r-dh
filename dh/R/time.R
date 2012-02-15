# reformat seconds to days, hours, minutes and seconds (vectorized)
"sec.dhms" <- function(sec) {
   sec.negative <- sec < 0
   sec <- abs(sec)
   
   d <- floor(sec / 86400)
   sec <- sec - 86400 * d
   
   h <- floor(sec / 3600)
   sec <- sec - 3600 * h
   
   m <- floor(sec / 60)
   s <- floor(sec - 60 * m)
   
   sign.text <- ifelse(sec.negative, "-", "")
   
   d.text <- ifelse(d == 0, "", paste(d, "d", sep = ""))
   h.text <- ifelse(d == 0 & h == 0, "", ifelse(d == 0, paste(h, "h", sep = ""), paste(fill(h, 2), "h", sep = "")))
   m.text <- ifelse(d == 0 & h == 0 & m == 0, "", ifelse(d == 0 & h == 0, paste(m, "m", sep = ""), paste(fill(m, 2), "m", sep = "")))
   s.text <- ifelse(d == 0 & h == 0 & m == 0, paste(s, "s", sep = ""), paste(fill(s, 2), "s", sep = ""))
   
   t.dhms <- character(length(sec))
   for (i in seq(1, length(sec))) {
      t.dhms[i] <- paste(sign.text[i], d.text[i], h.text[i], m.text[i], s.text[i], sep = "")
   }
   
   t.dhms
}

# returns current date and time in 'yyyymmdd-hhmmss' format
"time.ymdhms" <- function() {
   format(Sys.time(), "%Y%m%d-%H%M%S")
}
