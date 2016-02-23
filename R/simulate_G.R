#' Simulate a G graph matrix
#'
#' @export
simulate_G <- function(n_nodes = 10, edge_prob = 0.25){
  out <- matrix(nrow = n_nodes, ncol = n_nodes)
  binom_coef <- n_nodes * (n_nodes - 1) / 2
  edge_indicator <- rbinom(n = 45, size = 1, prob = edge_prob)
  lower.tri(out) <- edge_indicator
  out <- out + t(out)
  return(out)
}
