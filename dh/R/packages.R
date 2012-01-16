"is.installed" <- function(packages) {
   if (!is.vector(packages) || !is.character(packages)) {
      stop("Argument 'packages' must be a character vector of package names")
   }
   sapply(packages, function(package) system.file(package = package) != "")
}

"is.loadedPackage" <- function(packages) {
   if (!is.vector(packages) || !is.character(packages)) {
      stop("Argument 'packages' must be a character vector of package names")
   }
   paste("package", packages, sep = ":") %in% search()
}

"install.suggestedPackages" <- function(packages, ...) {
   if (!is.vector(packages) || !is.character(packages)) {
      stop("Argument 'packages' must be a character vector of package names")
   }
   suggests <- unique(unlist(lapply(packages, function(package) strsplit(utils::packageDescription(package)$Suggests, "\\s*,\\s*"))))
   install.packages(pkgs = suggests, ...)
}
