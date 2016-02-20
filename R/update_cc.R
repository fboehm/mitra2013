#' Update cc vector
#'
#' @param ymat data matrix
#' @param zmat trinary indicator matrix
#' @export
update_cc <- function(ymat, zmat){
  imax <- nrow(ymat)
  out <- numeric(length = imax)
  for (i in 1:imax){
    indic <- zmat[i, ] == -1
    maxint <- ceiling(max(ymat[i, indic]))
    tosample <- intersect(1:5, maxint:5)
    out[i] <- sample(tosample, size = 1)
  }
}
