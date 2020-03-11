#' Gram-Smidth process in r
#'
#' Calculates a dot product between matrices
#'
#' @param x1 The first vector.
#' @param x2 The second vector.
#' @param x3 The third vector.
#'
#' @return Should give us a number.
#' @export
#' @examples
#' x1 <- c(1,1,1,1,1,1)
#' x2 <- c(1,1,1,1,1,1)
#' gramSmith(x1,x2)
gramSmith <- function(x1,x2,x3){
  if (missing(x3)){
  x2x1 <- t(x1) %*% x2
  x1x1 <- t(x1) %*% x1
  v1 <- x1
  v2 <- x2 - drop(x2x1/x1x1) * x1
  return(cbind(v1,v2))
  } else {
    x2x1 <- t(x2) %*% x1
    x1x1 <- t(x1) %*% x1
    v1 <- x1
    v2 <- x2 - drop(x2x1/x1x1) * x1
    x3v1 <- t(x3) %*% v1
    x3v2 <- t(x3) %*% v2
    v2v2 <- t(v2) %*% v2
    v3 <- x3 - (drop(x3v1/x1x1) * x1) - (drop(x3v2/v2v2) * v2)
    return(cbind(v1,v2,v3))
  }
}
x1 <- matrix_create(c(1,1,1,1),4,1)
x2 <- matrix_create(c(0,1,1,1), 4, 1)
x3 <- matrix_create(c(0,0,1,1), 4, 1)
gramSmith(x1,x2,x3)
