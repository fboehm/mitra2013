#' Update trinary vector
#'
#'
update_trinary <- function(){

}


#' Wrapper to update trinary matrix
#'
#'
update_trinary_mat <- function(zmat){
  out <- zmat
  for (i in 1:tmax){
    out[, t] <- update_trinary()
  }
  return(out)
}

