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

}
