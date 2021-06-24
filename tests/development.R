## libraries
library(devtools)
library(haRmonics)
library(plot3D)
library(rgl)

## check Ylm harmonics
l <- 6
m <- 0
d <- 2

axis_lim <- c(-1, 1)

{
  C <- xyzSphericalHarmonics(l, m, d)
  D <- -C
  Y <- rptSphericalHarmonics(l, m, d)
  Z <- -Y

  J <- dplyr::bind_rows(C, D)
}

# positive and negative values
rgl::plot3d(
  J,
  xlim = axis_lim,
  ylim = axis_lim,
  zlim = axis_lim
)

# Real
rgl::plot3d(x=Re(J$x), y=Re(J$y), z=Re(J$z))

# Imaginary
rgl::plot3d(x=Im(J$x), y=Im(J$y), z=Im(J$z))

## COMPLEX PHASE
co2ph <- lapply(A$Ylm, complex2phase)
phases <- unlist(lapply(co2ph, function(x) x$theta))
phases[is.nan(phases)] <- 0

### LAGUERRE POLYNOMIALS
x_vals <- seq(-2, 2, by=0.1)
y_vals <- unlist(lapply(x_vals, function(x) assoc_laguerre(n=3, k=1, x)))
plot(x_vals, y_vals)
