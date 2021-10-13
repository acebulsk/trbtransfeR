#' STABLE CASE: Stability Correction for Momentum, Heat, or Water Vapour Flux
#'
#' Provides a correction for the stable or neutral case for any of the flux types
#'
#' @param z_L measurement height divided by the monin obukhov length (unitless)
#'
#' @return stability correction to be used in the optim flux calculations, if z_l is less than zero returns NA.
#' @export
#'
#' @examples
phi_stbl <- function(z_L) {
  dplyr::if_else(z_L >= 0, -5 * z_L, NA)
}

#' UNSTABLE CASE: Stability Correction for Momentum
#'
#' @param z_L
#'
#' @return stability correction to be used in the optim flux calculations, if z_l is less than zero returns NA.
#' @export
#'
#' @examples
phi_unstble_m <- function(z_L){
  x <- (1-(16*z_L))^{1/4}
  phi <- 2*log((1 + x )/2) + log((1 + x^2) / 2) - (2 * atan(x) + pi/2)

  out <- if_else(z_L < 0, phi, NA)

  return(out)
}

#' UNSTABLE CASE: Stability Correction for Heat or Water Vapour
#'
#' @param z_L
#'
#' @return stability correction to be used in the optim flux calculations, if z_l is less than zero returns NA.
#' @export
#'
#' @examples
phi_unstble_hv <- function(z_L){
  x <- (1-(16*z_L))^{1/4}
  phi <- 2 * log((1 + x^2)/2)

  out <- if_else(z_L < 0, phi, NA)

  return(out)
}
