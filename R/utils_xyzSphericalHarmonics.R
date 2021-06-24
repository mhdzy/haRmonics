#' xyzSphericalHarmonics
#'
#' @description Computes the Spherical Harmonics for order `l` and degree `m`
#' and returns a matrix of [ x y z ] components.
#'
#' @param l The order `l`.
#' @param m The degree `m`.
#' @param density The density of points being returned. `1` is 100% (default).
#'
#' @return A complex matrix of [ x y z ] coordinates.
#'
#' @importFrom tidyr crossing
#' @importFrom tibble tibble
#' @export
#'
xyzSphericalHarmonics <- function(l = 1, m = 0, density = 1) {
  A <- rptSphericalHarmonics(l, m, density)

  B <- list()
  for (j in 1:nrow(A)) {
    B[[j]] <- sph2car(A$r[j], A$theta[j], A$phi[j])
  }

  vec_x <- unlist(lapply(B, function(x) x$x))
  vec_y <- unlist(lapply(B, function(x) x$y))
  vec_z <- unlist(lapply(B, function(x) x$z))
  C <- tibble(x = vec_x, y = vec_y, z = vec_z)
}
