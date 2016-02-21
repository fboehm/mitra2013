#' Update graph, G
#'
#' @param G a graph matrix
#' @param betamat a beta matrix
#' @param emat an e matrix of binary indicators
#' @export
update_G <- function(G, betamat, emat){
  # make a candidate Gprop
  ## sample two indices & order them
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
    betamatprop[indmin, indmax] <- rnorm(n = 1, mean = betamatprop[indmin, indmax], sd = 0.1)
    #symmetrize
    betamatprop[indmax, indmin] <- betamatprop[indmin, indmax]
  }
  # We now have a pair: betamatprop and Gprop
  # We must calculate the acceptance probability
  normalizing_constant_ratio <- calc_RR(beta = beta, beta_prop = beta_prop)
  # note that the above is c(beta_prop) / c(beta), so we need to divide by
  # normalizing_constant_ratio when calculating acceptance ratio
  K_beta_prop <- exp(sum(apply(FUN = calc_logist_prob,
                               X = emat, MARGIN = 2, beta = beta_prop)))
  K_beta <- exp(sum(apply(FUN = calc_logist_prob,
                          X = emat, MARGIN = 2, beta = beta)))
  p_beta_prop <- dnorm(beta_prop, mean = 0, sd = sqrt(0.3))
  p_beta <- dnorm(beta, mean = 0, sd = sqrt(0.3))
  acc_ratio <- p_beta_prop * K_beta_prop /
    (p_beta * K_beta * normalizing_constant_ratio)
  u <- runif(n = 1)
  if (u < acc_ratio) {
    out <- list(beta = betamatprop, G = Gprop)
  }
  else{
    out <- list(beta = betamat, G = G)
  }
  return(out)
}
