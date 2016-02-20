#' Update trinary scalar
#'
#' @param e a scalar, with value 0 or 1
#' @param y a scalar
#' @param pp a scalar probability
#' @param mu1 a scalar mean
#' @param mu2 a scalar mean
#' @param sigma1 a scalar sd
#' @param sigma2 a scalar sd
#' @export
update_trinary_scalar <- function(e, y, pp, mu1, mu2, sigma1, sigma2){
  if (e == 0) out <- -1
  if (e == 1){
    p1 <- dlnorm(x = y, mean = mu1, sd = sigma1)
    p2 <- dlnorm(x = y, mean = mu2, sd = sigma2)
    prob <- pp * p1 / (pp * p1 + (1 - pp) * p2)
    out <- rbinom(n = 1, size = 1, prob = prob)
  }
  return(out)
}
#' Wrapper to update a column of z matrix
#'
#' @param evec a column from the e matrix
#' @param yvec a column from the y matrix
#' @param mu1vec a mu vector
#' @param mu2vec a mu vector
#' @param sd1vec a sd vector
#' @param sd2vec a sd vector
#' @export
update_trinary_vector <- function(evec, yvec, mu1vec,
                                  mu2vec, sigma1vec, sigma2vec){
  imax <- length(evec)
  out <- numeric(length = imax)
  for (i in 1:imax){
    out[i] <- update_trinary_scalar(e = evec[i], y = yvec[i],
                                    mu1 = mu1vec[i], mu2 = mu2vec[i],
                                    sigma1 = sigma1vec[i],
                                    sigma2 = sigma2vec[i])
  }
  return(out)
}



#' Wrapper to update trinary matrix
#'
#' @param emat a binary matrix
#' @param ymat a data matrix
#' @param mu1vec a mu vector
#' @param mu2vec a mu vector
#' @param sd1vec a sd vector
#' @param sd2vec a sd vector
#' @export
update_trinary_mat <- function(emat, ymat, mu1vec, mu2vec, sd1vec, sd2vec){
  out <- emat
  for (t in 1:tmax){
    out[, t] <- update_trinary_vec(evec = emat[, t],
                                   yvec = ymat[, t],
                                   mu1vec = mu1vec,
                                   mu2vec = mu2vec,
                                   sd1vec = sd1vec,
                                   sd2vec = sd2vec)
  }
  return(out)
}
