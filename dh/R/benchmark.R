# always compute the same operation and check the result
"benchmark.sanity" <- function(N = 1e3, cores = parallel::detectCores(), seed = 17041985) {
   # allow to test multiple CPUs
   library(parallel)
   
   # prepare result
   set.seed(seed)
   M <- matrix(rnorm(N^2), nrow = N, ncol = N)
   MtM <- t(M) %*% M
   MtM.inv <- solve(MtM)
   
   # statistics
   runs.ok <- 0
   runs.fail <- 0
   
   # run tests in an infinite loop
   while (TRUE) {
      # let the cores calculate the results
      cores.results <- parallel::mclapply(seq(cores), function(core) {
         MtM.core <- t(M) %*% M
         MtM.core.inv <- solve(MtM.core)
         MtM.core.inv
      })
      
      # check results
      lapply(cores.results, function(core.result) {
         if (identical(MtM.inv, core.result)) {
            runs.ok <<- runs.ok + 1
         } else {
            runs.fail <<- runs.fail + 1
         }
      })
      
      # print statistics
      print(c("runs.ok" = runs.ok, "runs.fail" = runs.fail))
   }
}

"benchmark.matrix" <- function(Ns = c(10, 50, 100, 500, 1000), operations = c("mul", "solve", "chol", "qr", "eigen", "svd"), T = 5, seed = 17041985) {
   # set seed
   set.seed(seed = seed)
   
   # check argument 'Ns'
   if (!is.vector(Ns) || !is.numeric(Ns) || !is.finite(Ns) || any(Ns <= 0)) {
      stop("Argument 'Ns' must be a numeric vector containing positive values")
   }
   
   # check argument 'operations'
   operations <- match.args(operations, c("mul", "solve", "chol", "qr", "eigen", "svd"))
   
   # check argument 'T'
   if (!is.singleton(T) || !is.numeric(T) || T <= 0) {
      stop("Argument 'T' must be a numeric vector of length one, containg a positive value")
   }
   
   # test parameters
   ts <- seq(from = 1, to = T, by = 1)
   
   # array of timing results
   results <- array(NA, dim = c(length(Ns), length(operations), length(ts)), dimnames = list(Ns, operations, ts))
   
   # do tests for varying matrix sizes
   for (N in Ns) {
      # progress bar
      cat("N = ", N, "\n", sep = "")
      pb <- progressBar(to = length(operations) * length(ts))
      
      # prepare matrices
      X <- matrix(rnorm(N^2), nrow = N, ncol = N)
      XtX <- t(X) %*% X
      
      
      # repeat operation several times to get reliable timings
      for (t in ts) {
         # test various matrix operations
         for (operation in operations) {
            f <- switch(operation,
               "mul"   = function() { X %*% X },
               "solve" = function() { solve(XtX) },
               "chol"  = function() { chol(XtX) },
               "qr"    = function() { qr(XtX) },
               "eigen" = function() { eigen(XtX) },
               "svd"   = function() { svd(XtX) },
               stop("Invalid test operation '", operation, "'")
            )
            results[as.character(N), operation, ts] <- system.time(f())["elapsed"]
            pb <- step(pb)
         }
      }
   }
   
   # apply median to results
   results.median <- apply(results, c(1, 2), median)
   print(results.median)
   
   # return full results
   invisible(results)
}
