# deparse and substitute an expression ("turn symbol into string")
"desub" <- function(expr) {
   deparse(substitute(expr))
}

# fills strings to a given length (vectorized)
"fill" <- function(v, to.length = 4, with.character = "0", from.left = TRUE) {
   v.fill <- character(length(v))
   
   remaining <- to.length - strlen(v)
   remaining[remaining < 0] <- 0
   
   for (i in seq(1, length(v))) {
      filler <- paste(rep(with.character, remaining[i]), collapse = "")
      v.fill[i] <- paste(
         ifelse(from.left, "", v[i]),
         filler,
         ifelse(from.left, v[i], ""),
         sep = ""
      )
   }
   
   names(v.fill) <- names(v)
   v.fill
}

# 'paste' with 'sep = ""'
"paste0" <- function(...) {
   paste(..., sep = "")
}

# return the first characters of strings (vectorized)
"strfirst" <- function(v, characters = 1) {
   substr(v, 1, characters)
}

# return the last characters of strings (vectorized)
"strlast" <- function(v, characters = 1) {
   v.strlen <- strlen(v)
   substr(v, v.strlen - characters + 1, v.strlen)
}

# return the length of strings (vectorized)
"strlen" <- function(v) {
   v.strlen <- unlist(lapply(strsplit(as.character(v), ""), length))
   names(v.strlen) <- names(v)
   v.strlen
}
