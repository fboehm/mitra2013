#' Update a single element of the binary indicator matrix e
#'
#' @param evec vector e_-it in Mitra 2013 notation
#' @param Gvec vector G_-it in Mitra 2013 notation
#' @param beta a scalar, entry [i, i] of beta matrix
#' @param betavec a vector from beta matrix, beta_matrix[i, -i]
#' @param y scalar entry from data matrix
#' @param theta sampling distribution parameters, a list
#' @return a binary scalar (0 or 1)
#' @export
update_binary <- function(evec # the vector e_-it
                          , Gvec # vector G_-it
                          , beta # scalar
                          , betavec # beta_i[-i] vector
                          , y,
                          theta
){
  bern_prob <- calc_bern_prob_binary(evec, Gvec, beta, betavec, y, theta)
  e <- rbinom(n = 1, size = 1, prob = bern_prob)
  return(e)
}


#' Calculate a bernoulli probability for assigning e_it = 0 v. 1.
#'
#' @param evec a vector e[-i, t] from the e matrix
#' @param Gvec a vector G[-i, i] from the graph matrix G
#' @param beta a scalar (on the diagonal) beta[i, i] from beta matrix
#' @param betavec a vector betamat[-i, i] from beta matrix
#' @param y a scalar, the [i,t] entry of data matrix
#' @param theta sampling parameters, a list
#' @return a scalar probability
#' @export

calc_bern_prob_binary <- function(evec, Gvec, beta, betavec, y, theta){
  p0 <- calc_exp_binary(e = 0, evec, Gvec, beta, betavec)
  p1 <- calc_exp_binary(e = 1, evec, Gvec, beta, betavec)
  # calc likelihoods
  l0 <- calc_lik_y(y, e = 0, theta)
  l1 <- calc_lik_y(y, e = 1, theta)
  # calc bern prob
  out <- p1 * l1 / (p0 * l0 + p1 * l1)
  return(out)
}


#' Calculate the exponential factor in updating e
#'
#' @param e a binary scalar
#' @param evec a vector e[-i, t] from e matrix
#' @param Gvec a vector G[-i, i] from G graph matrix
#' @param beta a scalar from the diagonal of beta matrix
#' @param betavec a vector betamatrix[-i, i] from beta matrix
#' @return a scalar
#' @export
calc_exp_binary <- function(e, evec, Gvec, beta, betavec){
  v_i <- boehm::expit(beta)
  v_vec <- boehm::expit(betavec)
  tmp <- (e - v_i) * (betavec %*% (evec - v_vec))[as.logical(Gvec)]
  out <- exp(beta * e + tmp)
  return(out)
}

#' Calculate the likelihood given y, after equation 5 of Mitra et al. 2013
#'
#' @param y a scalar entry from y matrix
#' @param e a scalar entry from binary e matrix
#' @param theta a list of sampling parameters
#' @return a likelihood, a scalar
#' @export
calc_lik_y <- function(y, e, theta){
  if (e == 0) {out <- dpois(x = y, lambda = theta$lambda) * (y < theta$c)}
  if (e == 1) {
    p1 <- dlnorm(y, meanlog = theta$mu1, sdlog = theta$sigma1)
    p2 <- dlnorm(y, meanlog = theta$mu2, sdlog = theta$sigma2)
    out <- p1 * pp + (1 - pp) * p2
  }
  return(out)
}

#' Wrapper function to update binary matrix
#'
#' @param emat a binary matrix
#' @param Gmat a graph matrix
#' @param betamat a beta matrix
#' @param y data matrix
#' @param theta sampling parameters, as a list
#' @return binary matrix of same dimensions as emat, ie, an updated e matrix
#' @export
update_binary_mat <- function(emat, Gmat, betamat, y, theta){
  tmax <- ncol(emat)
  out <- emat
  for (t in 1:tmax){
    out[, t] <- update_binary(evec = e[-i, t], Gvec = G[-i, i], beta = beta[i,i], betavec = beta[-i, i], y = y, theta = theta)
  }
  return(out)
}
