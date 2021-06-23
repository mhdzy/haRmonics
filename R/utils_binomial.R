#' binomial
#'
#' @description A generalized (complex-valued) binomial function.
#'
#' @param a The first (top) parameter.
#' @param k The second (bottom) parameter.
#'
#' @return `a` choose `k`.
#'
#' @export
#'
binomial <- function(a, k) {
  num <- unlist(lapply(1:k, function(x) { a - x + 1 } ))

  return(prod(num)/factorial(k))
}
