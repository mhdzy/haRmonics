#' rptSphericalHarmonics
#'
#' @description Computes the Spherical Harmonics for order `l` and degree `m`
#' and returns a matrix of [ r phi theta ] components.
#'
#' @param l The order `l`.
#' @param m The degree `m`.
#' @param density The density of points being returned. `1` is 100% (default).
#'
#' @return A complex matrix of [ r phi theta ] coordinates.
#'
#' @importFrom tidyr crossing
#' @importFrom tibble tibble
#' @export
#'
rptSphericalHarmonics <- function(l = 1, m = 0, density = 1) {
  # phi is azimuthal ; theta is polar
  A <- crossing(
    phi = seq(0, 2*pi, by = pi/(20 * d)),
    theta = seq(0, pi, by = pi/(40 * d)),
    r = 0
  )

  for (i in 1:nrow(A)) {
    A$r[i] <- sphericalHarmonicY(l = l, m = m, phi = A$phi[i], theta = A$theta[i])
  }

  return(A)
}
