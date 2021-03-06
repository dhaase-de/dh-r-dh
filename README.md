dh-r-dh
=======

`R` toolbox of Daniel Haase.


Note
----

This package probably won't be updated anymore. It was last used with `R` 2.x,
and probably needs some fixing for newer `R` versions.


Change Log
==========


0.8.0 (2017-01-23)
------------------

* this release just summarizes all changes between 0.7.1 and 2013-07-26 (no
  changes have been made after 2013-07-26), and this is probably the final
  release
* added some DBLP (literature database) query functions
* added functions `munion` and `mintersect`, which are versions of `union` and
  `intersect` with an arbitrary number of arguments 


0.7.1 (2012-09-11)
------------------

* finished first working version of Kalman filter
* added function `centroid` which computes the (weighted) centroid of an image
* added function `rgaussian` for running Gaussian smoothing


0.7.0 (2012-07-06)
------------------

* added Kalman-filter related code (including a very simple own implementation)
* added function `rmedian` which calculates the running median


0.6.9 (2012-05-03)
------------------

* added benchmark functions `benchmark.sanity` and `benchmark.matrix` to check
  the performances of various systems


0.6.8 (2012-04-16)
------------------

* added function `equidistantSubset`


0.6.7 (2012-04-13)
------------------

* added functions `sv` and `ld` for saving/loading R objects to/from files of
  the same name


0.6.6 (2012-04-10)
------------------

* added function `pjacobian` which is a parallel version of `numDeriv::jacobian`
* added package `numDeriv` to suggested packages
* added debug function `pst` which is a shortcut for `print(system.time(...))`


0.6.5 (2012-03-23)
------------------

* added function `recycle` for the safe recycling of a vector
* updated argument of function `isFALSE` to match the format of `isTRUE`
* added dummy manual file `.empty.Rd` which can be used for new code object
  documentations
* added documentation entry `Undocumented` which includes all undocumented
  code objects
* changed naming convention of documentation files
* added documentation for some code objects


0.6.4 (2012-02-20)
------------------

* fixed bug in function `readImage`, now an error is triggered if no image
  file was found for a given `filename` argument


0.6.3 (2012-02-18)
------------------

* changed package startup message to only appear for interactive sessions
* changed last parameter name of function `minRequiredVersion` from `fix`
  to `patch`, removed constraint of checked version to be at least 0.6.0
  and set all three default parameter values to `0L` (this allows easier
  checks for main and subversions, e.g. `minRequiredVersion(1)`)


0.6.2 (2012-02-17)
------------------

* added function `match.args` which behaves similar to `match.arg` with
  `several.ok=TRUE` but with some more security checks/features


0.6.1 (2012-02-16)
------------------

* updated function `is.directory` to be more robust concerning its inputs
  (e.g. `is.directory(NULL)` now returns `NULL`, as expected)


0.6.0 (2012-02-15)
------------------

* added function `minRequiredVersion` to ensure a certain version of this
  package is used
* added function `is.directory` to check if a file exists and if it is a
  directory
* added function `paste0` as wrapper for `paste` with `sep=""`
* added function `clip` to clip values of objects to a certain range


0.5.5 (2012-02-14)
------------------

* added parameter `return` for function `sm` which causes the working
  directory to be set to the old value after changing to the specified
  directory and including the file `main.R` (`return` is `FALSE` by default,
  so the standard behavior is identical to previous versions)


0.5.4 (2012-01-18)
------------------

* added function `scurve` which plots a curve of a function and shades parts
  of the area under the curve


0.5.3 (2012-01-16)
------------------

* renamed function `st1` to `st`
* updated functions `st` and `out` to better deal with output of large lists


0.5.2 (2012-01-16)
------------------

* fixed bug in function `out` which occured if an object name was too long
* added function `st1` as shortcut for `str` with maximum level 1 (useful
  for displaying largely nested list structures)


0.5.1 (2011-11-30)
------------------

* added function `finite` which returns all finite elements of an object
* added function `reaload.package` to entirely reload a package (e.g. after
  re-installing a package)


0.5.0 (2011-11-29)
------------------

* rewrote and reorganized the functions for image display and colorization,
  `pm(X)` is now a wrapper for `pr(colorize(X))`, `pr` is a function for
  _p_lotting _r_aster images and `colorize` can now also use an entire
  image as `col` argument which causes an overplotting of the original
  image with `col`
* fixed a bug in function `pm` which now can display logical matrices
* updated function `writeImage` to also deal with raster images (and not
  just numeric arrays)
* added function `mvdnorm` which calculates the density of a multivariate
  normal distribution
* added function `gaussians2d` which creates random 2d normal distributions
  and creates an image for the resulting density
* added package `clusterGeneration` (useful but not essential for function
  `gaussians2d`) to the list of suggested packages
* fixed the behavior of `out()` and improved its output format
* added file `misc.R` and moved functions `isFALSE` and `sm` there
* added function `along` as an alias for `seq_along` or `seq(along=)`
* fixed typo in this change log


0.4.4 (2011-11-22)
------------------

* updated function `out` to work with any kind of R object (instead of just
  scalars as before) and improved the output format
* removed function `lso` which was replaced by function `objects.sizes` in
  version 0.4.0


0.4.3 (2011-11-21)
------------------

* added function `colorhash` to map an arbitrary R object to a color (based
  on package `digest`, which was added to the suggested packages)


0.4.2 (2011-11-09)
------------------

* added function `kurtosis` for the calculation of the kurtosis of discrete
  probability distributions


0.4.1 (2011-11-08)
------------------

* fixed bug in function `readImage` which caused pixmap and tiff images
  not to be loaded correctly (wrong use of `capture.output`)
* fixed bug in error message of conversion to gray scale images in function
  `readImage`
* updated functions `readImage` and `writeImage` to trigger an error if
  none of the supported packages for loading/saving an image can be used
* added function `file.extension<-` to replace the file extension of
  filenames
* added function `dirname<-` to replace the directory of filenames
* added function `entropy` to calculate the entropy of a discrete
  probability distribution


0.4.0 (2011-11-07)
------------------

* added package namespace
* changed `init` and `step` functions for progress bar to S4 methods
* added package `methods` in the package dependencies
* added wrapper function `progressBar`
* added functions `rot`, `rot2angle`, `deg2rad`, `rad2deg` for 2D rotations
* fixed usage of `rasterImage` in the function `pm`, now checks if this
  function exists and thus also works with R versions < 2.11.0
* added support for color palettes when using the raster drawing option in
  the function `pm`
* fixed the range calculation and scaling when using the raster drawing
  option in the function `pm` to also work with non-finite values
* added predefined color palettes (function `palettes` and object `pal`)
* added example matrix (object `M`, value `matrix(seq(16), 4)`)
* added function `is.loadedPackage` to check whether a package is currently
  loaded
* added function `equal` to check if two values are (numerically) equal
* added function `sm()` the source the file `main.R`
* minor code layout improvements
* added file `files.R`
* added function `file.extension` which returns the extensions of filenames
* added file `image.R` for image-related functions
* added functions `readImage` and `writeImage` as wrappers for image reading
  and writing packages (supports JPEG, PNM, PNM, TIFF)
* added packages `jpeg`, `png`, `pixmap` and `rtiff` to package suggests
* added function `bytes.toHumanReadable` to transform a number of bytes into
  a human readable string
* replaced function `lso` with function `objects.sizes` to output the size
  of R objects
* added function `wait` which waits for the user to hit [RETURN]
* added file `zzz.R` and startup message
* added package `utils` in the dependecies (needed to obtain package
  information using function `packageDescription`)
* updated function `is.installed` to use `system.file` instead of
  `installed.packages` for performance reasons


0.1.3, 0.1.4, 0.1.5, 0.1.6
--------------------------

* added usage of `rasterImage` in the function `pm`
* added function `indexIntervalLengths.toEndpoints`
* added function `is.installed` to check if a package is installed
* minor fixes for progress bar (more robust use of `Sys.getenv("COLUMNS")`)


0.1.2 (2010-10-20)
------------------

* moved file `geometry.R` to `math.R`
* some minor additions


0.1.1 (2010-08-27)
------------------

* added file `geometry.R`


0.1.0 (2010-08-25)
------------------

* added class `progressBar`
* added function `sec.dhms`
* added some other small functions


0.0.1 (2010-08-20)
------------------

First version.

