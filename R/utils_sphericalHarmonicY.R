#' sphericalHarmonicY
#'
#' @description Computes the angular component of the solution.
#'
#' @param l The degree of the Legendre polynomial.
#' @param m The order of the Legendre polynomial.
#' @param theta The angle theta at which to evaluate the function at.
#' @param phi The angle phi at which to evaluate the function at.
#'
#' @return The spherical harmonic Ylm() evaluated at (theta, phi).
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
