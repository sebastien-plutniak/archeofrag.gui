.heap <- function(s, n, env, ...) {
  if (n == 1L) {
    env$i <- env$i + 1L
    env$r[env$i, ] <- env$s
  } else {
    .heap(s, n - 1L, env)
    for (i in seq(1, (n - 1L))) {
      if (n %% 2L == 0L) {
        j <- i
      } else {
        j <- 1L
      }
      tmp <- env$s[n]
      env$s[n] <- env$s[j]
      env$s[j] <- tmp
      .heap(s, n - 1L, env)
    } 
  }
}

.heap_permutation <- function(s) {
  env <- environment()
  len <- length(s)
  i <- 0L
  env$r <- matrix(NA, factorial(len), len)
  .heap(s, len, env, i)
  env$r
}

