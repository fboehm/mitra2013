#' Update the bernoulli parameter pp vector
#'
#' @param zmat trinary matrix
#' @export
update_pp <- function(zmat){
  imax <- nrow(zmat)
  out <- numeric(length = imax)
  for (i in 1:imax){
    n0 <- sum(zmat[i, ] == 0)
    n1 <- sum(zmat[i, ] == 1)
    out[i] <- rbeta(n = 1, shape1 = 1 + n1, shape2 = 1 + n0)
  }
  return(out)
}
