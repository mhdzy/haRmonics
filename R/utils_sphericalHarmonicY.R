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

  sqrt_t1 <- (2*l + 1)/(4*pi)
  sqrt_t2 <- factorial(l-m)/factorial(l+m)

  sqrt_term <- sqrt(sqrt_t1 * sqrt_t2)
  plm_term <- assoc_legendre(l, m, cos(theta))
  img_term <- exp(1i*m*phi)

  return(sqrt_term * plm_term * img_term)
}

#' sphericalHarmonicK
#'
#' @description
#' Computes the K/N coefficient (renormalization constant) for the Real
#' Spherical Harmonics.
#'
#' @param l The degree of the Legendre polynomial.
#' @param m The order of the Legendre polynomial.
#'
#' @return The renormalization constant for the Re(SphHm).
#'
#' @export
#'
sphericalHarmonicK <- function(l, m) {
  t = ((2 * l + 1) * factorial(l - m)) / (4*pi*factorial(l + m))
  return(sqrt(t))
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
