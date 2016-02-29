#' Simulate beta matrix
#'
#' @export
simulate_beta <- function(G){
  n_nodes <- nrow(G)
  out <- matrix(data = 0, nrow = n_nodes, ncol = n_nodes)
  sampvec <- c(log(2), log(4), - log(2))
  for (j in 2:n_nodes){
    for (i in 1:(j - 1)){
      if (G[i, j] == 1){
        out[i, j] <- sample(x = sampvec, size = 1)
      }
    }
  }
  return(out)
}
