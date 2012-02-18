".onLoad" <- function(lib, pkg) {
   if (isTRUE(interactive())) {
      info <- utils::packageDescription(pkg)
      cat("This is package '", pkg, "', version ", info$Version, " (", info$Date, "). Have fun!\n", sep = "")
   }
}
