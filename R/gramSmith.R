#' Gram-Smidth process in r
#'
#' Calculates a dot product between matrices
#'
#' @param x1 The first vector.
#' @param x2 The second vector.
#'
#' @return Should give us a number.
#' @export
#' @examples
#' x1 <- c(1,1,1,1,1,1)
#' x2 <- c(1,1,1,1,1,1)
#' gramSmith(x1,x2)
gramSmith <- function(x1,x2){
  x2x1 <- t(x1) %*% x2
  x1x1 <- t(x1) %*% x1
  v1 <- x1
  v2 <- x2 - drop(x2x1/x1x1) * x1

  return(cbind(v1,v2))
}
