#' Multiplication of two data sets if they are numeric
#'
#' @param ds1 data set 1
#' @param ds2 data set 2
#'
#' @details
#' The two data sets are multiplied if they are numeric.
#'
#' @return
#' A vector containing the product of data set 1 and 2.
#'
#' @author
#' Thomas Nauss
#'
#' @note
#' The data sets must be of type vector.
#'
#' @seealso
#' For further functions something else.
#'
#' @examples
#' # Multiply two data sets
#'
#' a <- c(1,2,3,4,5)
#' b <- c(10,20,30,40,50)
#' example <- multiply(a, b)
#' print(example)
#'
#' @aliases
#' mult
#'
multiply <- function(ds1, ds2){
  if(is.numeric(ds1) & is.numeric(ds2)){
    result <- ds1 * ds2
    return(result)
  }
}