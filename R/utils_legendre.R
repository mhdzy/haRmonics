#' assoc_legendre
#'
#' @description Computes the value of the associated Legendre polynomial
#' P(`l`, `m`) at point `x`.
#'
#' @section special cases:
#' When |m| > l, the returned value is 0.
#'
#' @param l The order l.
#' @param m The degree m.
#' @param x The value at which to evaluate the function.
#'
#' @return The P^m_l(x) value.
#'
#' @export
#'
assoc_legendre <- function(l, m, x) {

  # |m| > l; P(l, m) = 0
  if (abs(m) > l) return(0)

  # call self
  if (m < 0) {
    phase <- (-1)^abs(m)
    fac_term <- factorial(l-abs(m))/factorial(l+abs(m))
    return(phase * fac_term * haRmonics::assoc_legendre(l, abs(m), x))
  }

  phase <- (-1)^m
  exp_term <- 2^l * ( (1-x^2)^(m/2) )
  bin_term <- sum(
    unlist(
      lapply(m:l, function(k) {
        ( factorial(k) / factorial(k - m) ) *
          ( x^(k-m) ) *
          haRmonics::binomial(l, k) *
          haRmonics::binomial((l + k - 1)/2, l)
      })
    )
  )

  return(phase * exp_term * bin_term)
}

#' assoc_legendre2
#'
#' @description Computes the value of the associated Legendre polynomial
#' P(`l`, `m`) at point `x`.
#'
#' @section special cases:
#' When |m| > l, the returned value is 0.
#'
#' @param l The order l.
#' @param m The degree m.
#' @param x The value at which to evaluate the function.
#'
#' @return The P^m_l(x) value.
#'
#' @export
#'
assoc_legendre2 <- function(l, m, x) {
  p <- 1

  if ( m > 0 ) {
    somx2 = sqrt((1 - x) * (1 + x))
    f = 1
    for (i in 1:m) {
      p = p * ((-f) * somx2)
      f = f + 2
    }
  }

  if ( l == m )
    return(p)

  p1 = x * (2 * m + 1) * p

  if ( l == m + 1 )
    return(p1)

  p2 = 0
  for ( l2 in (m + 2):l ) {
    p2 = ( (2 * l2 - 1) * x * p1 - (l2 + m - 1) * p ) / (l2 - m)
    p = p1
    p1 = p2
  }

  return(p2)
}
