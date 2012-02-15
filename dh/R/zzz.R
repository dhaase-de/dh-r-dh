".onLoad" <- function(lib, pkg) {
   info <- utils::packageDescription(pkg)
   cat("This is package '", pkg, "', version ", info$Version, " (", info$Date, "). Have fun!\n", sep = "")
}
