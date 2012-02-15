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

"reload.package" <- function(packages) {
   lapply(packages, function(package) {
      detach(paste("package:", package, sep = ""), unload = TRUE, character.only = TRUE)
      library(package, character.only = TRUE)
   })
   invisible(NULL)
}

# check if at least a certain version of package 'dh' is installed
"minRequiredVersion" <- function(main = 0L, sub = 6L, fix = 0L, require.identical = FALSE) {
   # check arguments
   if (!is.singleton(main) || !is.singleton(sub) || !is.singleton(fix) || !is.numeric(main) || !is.numeric(sub) || !is.numeric(fix)) {
      stop("Arguments 'main', 'sub' and 'fix' must be numeric vectors of length one")
   }
   if (main == 0L && sub < 6L) {
      warning("Version check for package 'dh' is not available for package versions < '0.6.0'")
   }
   
   # get version of installed package 'dh'
   dh.description <- utils::packageDescription("dh")
   dh.version <- as.numeric(unlist(strsplit(dh.description$Version, "\\.")))
   names(dh.version) <- NULL
   if (length(dh.version) != 3L) {
      stop("Invalid version of installed package 'dh'")
   }
   
   # check if installed version is smaller than required version
   dh.installedVersion <- paste0(dh.version[1], ".", dh.version[2], "-", dh.version[3])
   dh.requiredVersion <- paste0(main, ".", sub, "-", fix)
   comp <- utils::compareVersion(dh.installedVersion, dh.requiredVersion)
   
   # for output, use "x.y.z" instead of "x.y-z" (as required by 'utils::compareVersion')
   dh.installedVersion.dots <- paste(dh.version[1], dh.version[2], dh.version[3], sep = ".")
   dh.requiredVersion.dots <- paste(main, sub, fix, sep = ".")
   
   # stop if the version requirements are not met, otherwise return 'TRUE'
   if (isTRUE(require.identical) && comp != 0) {
      stop("Installed package version '", dh.installedVersion.dots, "' is not identical to the required version '", dh.requiredVersion.dots, "' for package 'dh'")
   } else if (comp < 0) {
      stop("Installed package version '", dh.installedVersion.dots, "' is smaller than the minimum required version '", dh.requiredVersion.dots, "' for package 'dh'")
   }
   
   invisible(TRUE)
}
