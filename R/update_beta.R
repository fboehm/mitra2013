#' Update beta
#'
#' @param beta a beta matrix
#' @param emat e binary matrix
#' @param s standard deviation for jump
#' @param K number of binary vectors to sample
#' @export
update_beta <- function(beta, emat, s = 0.1, K = 5000){
  # create a proposed beta
  beta_prop <- beta
  diag(beta_prop) <- calc_beta_prop(diag(beta), sd = s)
  # assemble pieces for acceptance ratio
  normalizing_constant_ratio <- calc_RR(beta = beta, beta_prop = beta_prop)
  # note that the above is c(beta_prop) / c(beta), so we need to divide by
  # normalizing_constant_ratio when calculating acceptance ratio
  logK_beta_propvec <- apply(FUN = calc_logist_prob,
                               X = emat, MARGIN = 2, beta = beta_prop)
  logK_betavec <- apply(FUN = calc_logist_prob,
                          X = emat, MARGIN = 2, beta = beta)
  logK_diff <- logK_beta_propvec - logK_betavec
  logKratio <- sum(logK_diff)
  logp_beta_prop <- dnorm(beta_prop, mean = 0, sd = sqrt(0.3), log = TRUE) # matrix
  logp_beta <- dnorm(beta, mean = 0, sd = sqrt(0.3), log = TRUE) # matrix
  logacc_ratio <- logKratio - log(normalizing_constant_ratio) +
    sum(logp_beta_prop - logp_beta)
  acc_ratio <- exp(logacc_ratio)
  u <- runif(n = 1)
  if (u < acc_ratio) {
    out <- beta_prop
  }
  else {
    out <- beta
    }
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
  foo <- calc_sampling_probs(beta)
  sampled_cols <- sample_columns(probs = foo$probs, vecmat = foo$vecmat)
  for (k in 1:K){
    v <- sampled_cols[,k]
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
calc_sampling_probs <- function(beta){
  imax <- nrow(beta)
  nvec <- 2 ^ imax
  inds <- nvec:(2 * nvec - 1)
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
  # process vec_list
  vecmat <- sapply(FUN = function(x)as.numeric(x)[-1], X = vec_list)
return(list(probs = probs_normalized, vecmat = vecmat))
}


#' Sample columns according to specified probabilities
#'
#' @param probs probabilities vector
#' @param vecmat a matrix from which to sample columns
#' @param K number of times to sample
#' @export
sample_columns <- function(probs, vecmat, K = 5000){
  nvec <- length(probs)
  samp <- sample(1:nvec, size = K, prob = probs, replace = TRUE)
  out <- vecmat[, samp]
  return(out)
}



#' Calculate log of unnormalized logistic probability
#'
#' @param v a binary vector
#' @param beta a beta matrix of coefficients
#' @export
calc_logist_prob <- function(v, beta){
  term1 <- diag(beta) %*% v
  foo_mat <- beta * (v - boehm::expit(diag(beta))) %*%
    t((v - boehm::expit(diag(beta))))
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
