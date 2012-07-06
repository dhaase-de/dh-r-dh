##
## class 'kf'
##

setClass(
   "kf",
   
   representation = representation(
      x = "numeric",            # state vector
      P = "matrix",             # state covariance
      F = "matrix",             # transition matrix
      H = "matrix",             # 
      Q = "matrix",             # process noise
      R = "matrix",             # measurement noise
      step = "character"        # 
   ),
   
   prototype = prototype(
      step = "+"
   )
)

"kf" <- function(...) {
   new("kf", ...)
}

"kf.predict" <- function(kf) {
   kf@x <- as.vector(kf@F %*% kf@x)
   kf@P <- kf@F %*% kf@P %*% t(kf@F) + kf@Q
   kf@step <- "-"
   kf
}

"kf.correct" <- function(kf, z) {
   K <- kf@P %*% t(kf@H) %*% solve(kf@H %*% kf@P %*% t(kf@H) + kf@R)
   kf@x <- as.vector(kf@x + K %*% as.matrix(z - kf@H %*% kf@x))
   kf@P <- (diag(nrow = nrow(K), ncol = ncol(kf@H)) - K %*% kf@H) %*% kf@P
   kf@step <- "+"
   kf
}

##
## extensions for external package 'FKF'
##

"fkf.predict" <- function(a, P, control) {
   res <- FKF::fkf(
      a0  = a,
      P0  = P,
      dt  = control$dt,
      ct  = control$ct,
      Tt  = control$Tt,
      Zt  = control$Zt,
      HHt = control$HHt,
      GGt = control$GGt,
      yt  = matrix(as.numeric(NA), nrow = length(control$ct), ncol = 1)
   )
   
   list(
      "a" = res$at[,2],
      "P" = res$Pt[,,2]
   )
}

"fkf.correct" <- function(a, P, y, control) {
   res <- FKF::fkf(
      a0  = a,
      P0  = P,
      dt  = control$dt,
      ct  = control$ct,
      Tt  = control$Tt,
      Zt  = control$Zt,
      HHt = control$HHt,
      GGt = control$GGt,
      yt  = as.matrix(y)
   )
   
   list(
      "a" = res$att[,1],
      "P" = res$Ptt[,,1]
   )
}

##
## demos
##

"kalman.demo" <- function() {
   # ground truth process
   set.seed(2)
   N <- 200
   g.a <- rnorm(N, mean = 0, sd = 0.1)
   g.v <- cumsum(g.a)
   g.x <- cumsum(g.v)

   # measurements
   m.x <- g.x + rnorm(N, mean = 0, sd = 1)
   
   # process settings
   x <- rep(0, 3)
   P <- 0 * diag(3)
   F <- matrix(c(1, 1, 0, 0, 1, 1, 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
   H <- matrix(c(1, 0, 0), nrow = 1, ncol = 3)
   Q <- diag(c(0, 0, 0.1^2))
   R <- matrix(1^2, nrow = 1, ncol = 1)
   
   # class 'kf'
   mdl.kf <- kf(
      x = x,
      P = P,
      F = F,
      H = H,
      Q = Q,
      R = R
   )
   
   # class 'fkf'
   mdl.fkf <- FKF::fkf(
      a0  = x,
      P0  = P,
      dt  = c(0, 0, 0),
      ct  = 0,
      Tt  = F,
      Zt  = H,
      HHt = Q,
      GGt = R,
      yt  = matrix(c(0, m.x), nrow = 1, ncol = N + 1)
   )
   
   xhat.kf <- xhat.fkf <- matrix(numeric(0), nrow = 0, ncol = 3)
   for (i in along(m.x)) {
      # kf
      mdl.kf <- kf.predict(mdl.kf)
      mdl.kf <- kf.correct(mdl.kf, m.x[i])
      xhat.kf <- rbind(xhat.kf, mdl.kf@x)
   }
   xhat.fkf <- t(mdl.fkf$att)[-1,]
   
   plot(g.x, t = "l", col = "blue", lwd = 3)
   lines(m.x, col = "red", lwd = 2)
   lines(xhat.kf[,1], col = "green", lwd = 2, lty = 1)
   
   list(
      "xhat.kf"  = xhat.kf,
      "xhat.fkf" = xhat.fkf
   )
}
