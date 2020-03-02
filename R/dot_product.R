#' Calculate the dot product
#'
#' Calculates a dot product between matrices
#'
#' @param n1 The first vector.
#' @param n2 The second vector.
#'
#' @return Should give us a number.
#' @export
#' @examples
#' n1 <- c(1,1,1,1,1,1)
#' n2 <- c(1,1,1,1,1,1)
#' dot_product(n1, n2)
dot_product <- function(n1,n2){

  dot <- t(n1) %*% n2
  dot <- as.numeric(dot)
  dot
}

