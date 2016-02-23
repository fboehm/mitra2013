#' Update the lambda vector
#'
#' @param lambdavec a lambda vector of Poisson means
#' @param ymat a y data matrix
#' @param zmat a trinary z indicator matrix
#' @param a gamma hyperprior
#' @param b gamma hyperprior
#' @export
update_lambda <- function(lambdavec, ymat, zmat, a = 0.001, b = 0.001){
  imax <- nrow(ymat)
  out <- lambdavec
  for (i in 1:imax){
    indic <- zmat[i,] == -1
    n <- sum(indic)
    sumy <- sum(ymat[i, indic])
    out[i] <- rgamma(n = 1, shape = a + sumy, rate = b + n)
  }
}
