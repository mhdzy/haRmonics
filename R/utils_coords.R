#' sph2car
#'
#' @description Transforms spherical coordinates to cartesian.
#'
#' @param r The radius.
#' @param theta The azimuthal angle.
#' @param phi The polar angle.
#'
#' @return A vector of length 3, corresponding to { x, y, z }.
#' @export
#'
sph2car <- function(r, theta, phi) {
  x <- r * sin(theta) * cos(phi)
  y <- r * sin(theta) * sin(phi)
  z <- r * cos(theta)
  return(
    list(
      x = x,
      y = y,
      z = z
    )
  )
}

#' car2sph
#'
#' @description Transforms cartesian coordinates to spherical.
#'
#' @param x The cartesian x coordinate.
#' @param y The cartesian y coordinate.
#' @param z The cartesian z coordinate.
#'
#' @return A vector of length 3, corresponding to { r, theta, phi }.
#' @export
#'
car2sph <- function(x, y, z) {
  r <- sqrt(x^2 + y^2 + z^2)
  theta <- acos(z/r)
  phi <- atan(y/x)

  return(
    list(
      r = r,
      theta = theta,
      phi = phi
    )
  )
}

#' complex2phase
#'
#' @description Given a complex number `c`, compute the transformation from
#' `c = x + iy` to `z = r * exp(i * theta)`.
#'
#' @param c A complex number.
#'
#' @return A list of `r` and `theta`.
#' @export
#'
complex2phase <- function(c) {
  c_x <- Re(c)
  c_y <- Im(c)

  r <- sqrt(c_x^2 + c_y^2)
  t <- atan(c_y/c_x)

  return(
    list(
      r = r,
      theta = t
    )
  )
}
