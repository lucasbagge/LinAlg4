#' Create a Matrix
#'
#' Create a Matrix where you only need to specify which numbers you have to put in,
#' and how it should be organiszed.
#'
#' @param string Numbers to put in the matrix
#' @param nr1 Number of rows
#' @param nr2 Number of columns
#'
#' @return A matrix
#' @export
#' @examples
#' string <- c(1,1,1,1,1,1)
#' matrix_create(string, 2,2)
matrix_create <- function(string, nr1, nr2){

  x <- matrix(c(string), nr1, nr2, byrow = T)

  x
}
