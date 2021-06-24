#' sphericalHarmonicK
#'
#' @description
#' Computes the K coefficient (re-normalization constant) for the Spherical
#' Harmonics.
#'
#' @param l The degree of the Legendre polynomial.
#' @param m The order of the Legendre polynomial.
#'
#' @return The re-normalization constant for Y(l, m).
#'
#' @export
#'
sphericalHarmonicK <- function(l, m) {
  return(
    sqrt(
      ((2 * l + 1) * factorial(l - m)) /
        (4 * pi * factorial(l + m))
    )
  )
}
