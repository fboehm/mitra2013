#' Update beta
#'
#' @param beta a beta matrix
#' @param emat e binary matrix
#' @param s standard deviation for jump
#' @param K number of binary vectors to sample
#' @export
update_beta <- function(beta, emat, s, K = 5000){
  # create a proposed beta
  beta_prop <- beta
  diag(beta_prop) <- calc_beta_prop(diag(beta), sd = s)
  # assemble pieces for acceptance ratio
  normalizing_constant_ratio <- calc_RR(beta = beta, beta_prop = beta_prop)
  # note that the above is c(beta_prop) / c(beta), so we need to divide by
  # normalizing_constant_ratio when calculating acceptance ratio
  K_beta_prop <- exp(sum(apply(FUN = calc_logist_prob, X = emat, MARGIN = 2, beta = beta_prop)))
  K_beta <- exp(sum(apply(FUN = calc_logist_prob, X = emat, MARGIN = 2, beta = beta)))
  p_beta_prop <- dnorm(beta_prop, mean = 0, sd = sqrt(0.3))
  p_beta <- dnorm(beta, mean = 0, sd = sqrt(0.3))
  acc_ratio<- p_beta_prop * K_beta_prop / (p_beta * K_beta * normalizing_constant_ratio)
  u <- runif(n = 1)
  if (u < acc_ratio) {out <- beta_prop} else {out <- beta}
  return(out)
}

#' Estimate normalizing constant ratio, RR
#'
#' @param K number of v vectors to sample
#' @param beta a beta matrix
#' @param beta_prop a proposed beta matrix
#' @export
calc_RR <- function(K = 5000, beta, beta_prop){
  # sample K binary vectors
  ratios <- numeric(length = K)
  for (k in 1:K){
    v <- sample_binary(beta)
    K_beta <- calc_logist_prob(v, beta)
    K_beta_prop <- calc_logist_prob(v, beta_prop)
    ratios[k] <- exp(K_beta_prop - K_beta)
  }
  rr <- mean(ratios)
  return(rr)
}

#' Calculate logistic probabilities for all binary vectors of a given length
#'
#' @param beta a beta matrix of coefficients
#' @export
sample_binary <- function(beta){
  imax <- nrow(beta)
  nvec <- 2 ^ imax
  inds <- nvec:(2*nvec - 1)
  foovecs <- R.utils::intToBin(inds)
  vec_list <- stringr::str_split(foovecs, "")
  probs <- numeric(length = nvec)
  for (i in 1:nvec){
    vec <- as.numeric(vec_list[[i]])[-1] # remove leading 1
    # calculate unnormalized probability
    # need to exponentiate, since calc_logist_prob returns log of probability
    probs[i] <- exp(calc_logist_prob(v = vec, beta = beta))
  }
  probs_normalized <- probs / sum(probs)
  samp <- sample(1:nvec, size = 1, prob = probs_normalized)
  out <- as.numeric(vec_list[[samp]])[-1]
  return(out)
}



#' Calculate log of unnormalized logistic probability
#'
#' @param v a binary vector
#' @param beta a beta matrix of coefficients
#' @export
calc_logist_prob <- function(v, beta){
  term1 <- diag(beta) %*% v
  foo_mat <- beta * (v - boehm::expit(diag(beta))) %*% t((v - boehm::expit(diag(beta))))
  term2 <- sum(foo_mat[lower.tri(foo_mat)])
  return(term1 + term2)
}


#' Calculate a proposal for beta matrix's diagonal vector
#'
#' @param beta a vector of diagonal entries from a beta matrix
#' @param sd standard deviation for the distribution of the jump
#' @export
calc_beta_prop <- function(beta, sd){
  for (b in 1:length(beta)){
    beta[b] <- beta[b] + rnorm(n = 1, mean = 0, sd = sd)
  }
  return(beta)
}
