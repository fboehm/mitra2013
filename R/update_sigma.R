#' Update a sigma vector
#'
#' @param sigmavec an inputted sigmavec
#' @param ymat the data matrix
#' @param muvec the 'corresponding' means vector
#' @param zmat matrix of trinary indicators
#' @param indicator 0 or 1 to denote which pair of means and sd's are of interest
#' @param a0 hyperparameter, shape parameter of hyperprior
#' @param b0 hyperparameter, rate parameter of hyperprior
#' @export

update_sigma <- function(sigmavec, ymat, muvec, zmat,
                         indicator, a0 = 0.001, b0 = 0.001){
  precvec <- 1 / sigmavec ^ 2
  imax <- length(sigmavec)
  for (i in 1:imax){
    indic <- zmat[i, ] == indicator
    n <- sum(indic)
    additive_term <- sum((log(ymat[i, indic]) - muvec[i]) ^ 2)
    bnew <- additive_term / 2 + b0
    anew <- a0 + n / 2
    precvec[i] <- rgamma(n = 1, shape = anew, rate = bnew)
  }
  return(1 / sqrt(precvec))
}
