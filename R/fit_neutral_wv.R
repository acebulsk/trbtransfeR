#' Estimate Turbulent Water Vapour Flux
#'
#' Use this function if have all params listed below. If not use the optimiser function fit_neutral_wv. Taken from This formula is taken from the head and mass exchange lecture slide 19.
#'
#' @inheritParams sensible_H_flux
#' @param ref_rh relative humidity fraction at reference height
#' @param surf_rh relative humidity fraction at surface
#' @param phi_v stability correction due to water vapour. 0 is for neutral or stable case.
#' @param lambda  2.5e-6 J kg-1 (latent head of vapourization)
#'
#' @return w / m2
#' @export
#'
#' @examples

water_vapour_flux <- function(ref_temp, surf_temp, p_atm, ref_rh, surf_rh, rho_air, zHeight, u_star, z_0m, d_0 = NA, phi_v, k = 0.4, lambda = 2.5e6){
  ref_e_a <- psychRomet::actual_vapour_pressure(psychRomet::tetens(ref_temp), ref_rh)
  surf_e_a <- psychRomet::actual_vapour_pressure(psychRomet::tetens(surf_temp), surf_rh)

  ref_sh <- psychRomet::specific_humidity(ref_e_a, total_pressure = p_atm, e_o = 0.622) # g/g == kg/kg
  surf_sh <- psychRomet::specific_humidity(surf_e_a, total_pressure = p_atm, e_o = 0.622)

  z_0v <- z_0m * 0.1

  -((ref_sh - surf_sh) * (k*u_star*rho_air*lambda)) * (log((zHeight - d_0)/(z_0v)) - phi_v)^-1
}

#' Estimate Turbulent Sensible Heat Flux
#'
#' @param tDiffMeas temp - temp at surface (kelvin)
#' @param rho_air density of moist air
#' @param zHeight height above the surface (metres)
#' @param u_star friction velocity
#' @param z_0m roughness length
#' @param q specific humidity (kg/kg)
#'
#'
#' @param k 0.4 (von Karman’s constant)
#' @param C_p 1004 (specific heat at constant pressure J kg-1 K-1
#' @param d_0 displacement height due to veg (m)
#'
#' @return
#' @export
#'
#' @examples
# fit_neutral_temp <- function(ref_temp, surf_temp, rho_air, zHeight, u_star, z_0m, q, k = 0.4, C_pl = 1004.67, d_0 = NA){
#   tDiffMeas <- ref_temp - surf_temp
#
#   C_p <- C_pl * (1+0.84*q)
#   z_0t <- z_0m * 0.1
#
#   loglinfun <- function(params){ # the input variable params must be the same length and dimension as the initial guesses (i.e. start points)
#     Qh <- params[[1]]
#     d_0 <- dplyr::if_else(is.na(d_0), params[[2]], d_0) # estimate d_0 if not given
#
#     tDiffSim <- (Qh/(k*u_star*rho_air*C_p))*log((zHeight - d_0)/(z_0t)) # log-linear wind speed function
#
#     obj_fn <- tDiffSim - tDiffMeas
#     SSE <- sum(obj_fn^2) # this is the sum square error, which is the value that we are trying to minimize.
#
#     #out <- list('SSE' = SSE, 'FittedWspeed' = FittedWspeed) # create list for multi arg return
#     return(SSE)
#   }
#
#   # set initial guesses for friction velocity and roughness length
#   start_point <- c(0.5, 0.5)
#
#   parameter_estimates <- stats::optim(par = start_point, fn = loglinfun)
#
#   out <- list('par_est' = parameter_estimates, 'model' = loglinfun)
#
#   est_pars <- data.frame(
#     Q_h = out$par_est$par[1],
#
#     d_0 = out$par_est$par[2]
#   )
#
#   return(est_pars)
#
# }
