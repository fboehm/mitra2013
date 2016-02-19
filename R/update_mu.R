#' Update a mu vector (mu1 or mu2)
#'
#' @param muvec mu vector to be updated
#' @param sigmavec sd vector (goes with mu vector)
#' @param ymat data matrix
#' @param zmat a trinary indicator matrix
#' @param indicator (0 or 1) indicator to denote which mu vector (1 or 2) is being updated
#' @param mu0 hyperparameter, mean of normal hyperprior
#' @param t0 hyperparameter, precision of normal hyperprior
#' @export
update_mu <- function(muvec, sigmavec, ymat, zmat, indicator, mu0, t0){
  imax <- length(muvec)
  for (i in 1:imax){
    tau <- (1 / sigmavec[i])^2
    indic <- zmat[i,] == indicator
    n <- sum(indic)
    sumlogy <- sum(log(y[indic]))
    mm <- (t0 * mu0 + tau * sumlogy) / (t0 + n * tau)
    prec <- t0 + n * tau
    muvec[i] <- rnorm(n = 1, mean = mm, sd = sqrt(1 / prec))
  }
  return(muvec)
}
