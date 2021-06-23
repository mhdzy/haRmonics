#' assoc_laguerre
#'
#' @description A utils function
#'
#' @param n The degree.
#' @param a The generalized parameter.
#' @param x The point at which to evaluate the function.
#'
#' @return The Laguerre(n, a) polynomial at point x.
#' @export
#'
assoc_laguerre <- function(n, k, x) {

  ret_val <- 0

  for (i in 0:n) {
    alt_term <- (-x)^(i)
    bin_term <- haRmonics::binomial(k+n, n-i)
    exp_term <- factorial(n)/factorial(i)
    ret_val <- ret_val + (alt_term * bin_term * exp_term)
  }

  return((1/factorial(n)) * ret_val)
}
