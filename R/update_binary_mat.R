#' Update a single element of the binary indicator matrix e
#'
#' @param evec vector e_-it in Mitra 2013 notation
#' @param Gvec vector G_-it in Mitra 2013 notation
#' @param beta a scalar, entry [i, i] of beta matrix
#' @param betavec a vector from beta matrix, beta_matrix[i, -i]
#' @param y data matrix
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
  bern_prob <- calc_bern_prob_binary(evec, Gvec, beta, betavec, yvec, theta)
  e <- rbinom(n = 1, size = 1, prob = bern_prob)
  return(e)
}

calc_bern_prob_binary <- function(evec, Gvec, beta, betavec, yvec, theta){
  lik <- calc_lik_y(y, e, theta)
  mult0 <- calc_exp_binary(e = 0, evec, Gvec, beta, betavec)
  mult1 <- calc_exp_binary(e = 1, evec, Gvec, beta, betavec)
  return(lik*multiplier)
}

calc_exp_binary <- function(e, evec, Gvec, beta, betavec){
  v_i <- boehm::expit(beta)
  v_vec <- boehm::expit(betavec)
  tmp <- (e - v_i) * (betavec %*% (evec - v_vec))[as.logical(Gvec)]
  out <- beta * e + tmp
  return(out)
}


calc_lik_y <- function(yvec, e, theta){

}

# wrapper to allow for updating every element in the matrix of binary indicators
update_binary_mat <- function(e, G, beta, y, theta){
  tmax <- ncol(e)
  out <- e
  for (t in 1:tmax){
    out[, t] <- update_binary(evec = e[-i, t], Gvec = G[-i, i], beta = beta[i,i], betavec = beta[-i, i], y = y, theta = theta)
  }
  return(out)
}
