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

