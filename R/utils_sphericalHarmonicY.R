#' sphericalHarmonicY
#'
#' @description Computes the spherical harmonic of degree l, order m at the
#' point `P = < theta, phi >`.
#'
#' @param l The degree of the Legendre polynomial.
#' @param m The order of the Legendre polynomial.
#' @param theta The angle theta at which to evaluate the function at.
#' @param phi The angle phi at which to evaluate the function at.
#'
#' @return The spherical harmonic Y(l, m) evaluated at (theta, phi).
#'
#' @export
#'
sphericalHarmonicY <- function(l, m, theta, phi) {
  return(
    sphericalHarmonicK(l, m) *
      assoc_legendre(l, m, cos(theta)) *
      exp(1i * m * phi)
  )
}

#' sphericalHarmonicY2
#'
#' @description Computes the spherical harmonic of degree l, order m at the
#' point `P = < theta, phi >`.
#'
#' @param l The degree of the Legendre polynomial.
#' @param m The order of the Legendre polynomial.
#' @param theta The angle theta at which to evaluate the function at.
#' @param phi The angle phi at which to evaluate the function at.
#'
#' @return The spherical harmonic Y(l, m) evaluated at (theta, phi).
#'
#' @export
#'
sphericalHarmonicY2 <- function(l, m, theta, phi) {
  if ( m == 0 )
    return(
      sphericalHarmonicK(l, 0) *
        assoc_legendre2(l, m, cos(theta))
    )
  else if ( m > 0 )
    return(
      sqrt(2) *
        sphericalHarmonicK(l, m) *
        cos(m * phi) *
        assoc_legendre2(l, m, cos(theta))
    )
  else
    return(
      sqrt(2) *
        sphericalHarmonicK(l, -m) *
        sin(-m * phi) *
        assoc_legendre2(l, -m, cos(theta))
    )
}
