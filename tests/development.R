## libraries
library(devtools)
library(haRmonics)

## check Ylm harmonics
L=4
M=-3
phi <- seq(0, 2*pi, by = pi/20)
theta <- seq(0, pi, by = pi/40)

{
  A <- tidyr::crossing(phi, theta)
  Ylm <- seq(1, nrow(A))
  for (i in 1:nrow(A)) {
    Ylm[i] <- haRmonics::sphericalHarmonicY2(l = L, m = M, phi = A$phi[i], theta = A$theta[i])
  }
  A$Ylm <- Ylm

  B <- list()
  for (j in 1:nrow(A)) {
    B[[j]] <- haRmonics::sph2car(A$Ylm[j], A$theta[j], A$phi[j])
  }

  vec_x <- unlist(lapply(B, function(x) x$x))
  vec_y <- unlist(lapply(B, function(x) x$y))
  vec_z <- unlist(lapply(B, function(x) x$z))
  C <- tibble::tibble(x = vec_x, y = vec_y, z = vec_z)
}

# full
rgl::plot3d(C)

# Real
rgl::plot3d(x=Re(C$x), y=Re(C$y), z=Re(C$z))

# Imaginary
rgl::plot3d(x=Im(C$x), y=Im(C$y), z=Im(C$z))

## COMPLEX PHASE
co2ph <- lapply(A$Ylm, haRmonics::complex2phase)
phases <- unlist(lapply(co2ph, function(x) x$theta))
phases[is.nan(phases)] <- 0

### LAGUERRE POLYNOMIALS
x_vals <- seq(-2, 2, by=0.1)
y_vals <- unlist(lapply(x_vals, function(x) haRmonics::assoc_laguerre(n=3, k=1, x)))
plot(x_vals, y_vals)
