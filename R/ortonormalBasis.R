#' Gives us ortonomal basis
#'
#' Calculates a dot product between matrices
#'
#' @param x1 The first vector.
#' @param x2 The second vector.
#'
#' @return Should give us a number.
#' @export
ortonormalBasis <- function(x1,x2){

  x2x1 <- t(x1) %*% x2
  x1x1 <- t(x1) %*% x1

  v1 <- x1
  v2 <- x2 - drop(x2x1/x1x1) * x1

  norm_vec <- function(x) sqrt(sum(x^2))

  u1 <- drop(1/norm_vec(v1)) * v1
  u2 <- drop(1/norm_vec(v2)) * v2

  cbind(u1,u2)
}
