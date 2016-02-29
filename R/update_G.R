#' Update graph, G (and beta matrix)
#'
#' @param G a graph matrix
#' @param betamat a beta matrix
#' @param emat an e matrix of binary indicators
#' @export
update_G <- function(G, betamat, emat){
  # make a candidate Gprop
  # sample two indices & order them
  imax <- nrow(betamat)
  indices <- sample(1:imax, size = 2, replace = FALSE)
  indmax <- max(indices)
  indmin <- min(indices)
  Gprop <- G
  Gprop[indmin, indmax] <- 1 - Gprop[indmin, indmax] # switch one entry
  # symmetrize Gprop
  Gprop[indmax, indmin] <-  Gprop[indmin, indmax]
  # define betamatprop
  betamatprop <- betamat
  # if we changed the G entry from 1 to 0, we need to change
  # corresponding entry of beta to 0
  if (G[indmin, indmax] == 1){
    betamatprop[indmin, indmax] <- 0
    betamatprop[indmax, indmin] <- 0 #symmetrize
  }
  if (G[indmin, indmax] == 0){
    betamatprop[indmin, indmax] <- rnorm(n = 1,
                                         mean = betamatprop[indmin, indmax],
                                         sd = 0.1)
    #symmetrize
    betamatprop[indmax, indmin] <- betamatprop[indmin, indmax]
  }
  # We now have a pair: betamatprop and Gprop
  # We must calculate the acceptance probability
  normalizing_constant_ratio <- calc_RR(beta = betamat, beta_prop = betamatprop)
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
    out <- list(beta = betamatprop, G = Gprop)
  }
  else{
    out <- list(beta = betamat, G = G)
  }
  return(out)
}
