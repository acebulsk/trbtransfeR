#' Estimate Turbulent Sensible Heat Flux
#'
#' Use this function if have all params listed below. If not use the optimiser function fit_neutral_temp. Taken from This formula is taken from the head and mass exchange lecture slide 19.
#'
#' @param ref_temp temperature at reference height
#' @param surf_temp temperautre of surface
#' @param p_atm atmospheric pressure kpa
#' @param rho_air density of moist air
#' @param zHeight height above the surface (metres)
#' @param u_star friction velocity
#' @param z_0m roughness length
#' @param d_0 displacement height due to veg (m)
#' @param phi_h stability correction due to sensible heat. 0 is for neutral or stable case.
#'
#' @param k 0.4 (von Karman’s constant)
#' @param C_p 1004 (specific heat at constant pressure J kg-1 K-1
#'
#' @return J m-2 s-1 OR W m-2
#' @export
#'
#' @examples

sensible_H_flux <- function(ref_temp, surf_temp, p_atm, rho_air, zHeight, u_star, z_0m, d_0 = NA, phi_h, k = 0.4, C_p = 1004.67){
  # q <- psychRomet::specific_humidity(psychRomet::tetens(ref_temp), p_atm)
  #
  # C_p <- C_pl * (1+0.84*q)

  z_0t <- z_0m * 0.1

  # need mixing ratio for virtual temp calc
  # ref_ah <- psychRomet::mixing_ratio_p(psychRomet::tetens(ref_temp), p_atm)
  # surf_ah <- psychRomet::mixing_ratio_p(psychRomet::tetens(surf_temp), p_atm)

  # # calc virtual temp
  # ref_0 <- psychRomet::virtual_temp(ref_temp, ref_ah) + 273.15
  # surf_0 <- psychRomet::virtual_temp(surf_temp, surf_ah) + 273.15

  # ref_0 <- psychRomet::potential_temp(ref_temp, p_atm) + 273.15
  # surf_0 <- psychRomet::potential_temp(surf_temp, p_atm) + 273.15


  -((ref_temp - surf_temp) * (k*u_star*rho_air*C_p)) * (log((zHeight - d_0)/(z_0t)) - phi_h)^-1
}

#' Estimate Turbulent Sensible Heat Flux
#'
#' @inheritParams sensible_H_flux
#'
#' @return
#' @export
#'
#' @examples
optim_heat_params <- function(ref_temp, surf_temp, p_atm, rho_air, zHeight, u_star, z_0m, d_0 = NA, phi_h, k = 0.4, C_pl = 1004.67){
  q <- psychRomet::specific_humidity(psychRomet::tetens(ref_temp), p_atm)

  C_p <- C_pl * (1+0.84*q)
  z_0t <- z_0m * 0.1

  # need mixing ratio for virtual temp calc
  # ref_ah <- psychRomet::mixing_ratio_p(psychRomet::tetens(ref_temp), p_atm)
  # surf_ah <- psychRomet::mixing_ratio_p(psychRomet::tetens(surf_temp), p_atm)

  # # calc virtual temp
  # ref_0 <- psychRomet::virtual_temp(ref_temp, ref_ah)
  # surf_0 <- psychRomet::virtual_temp(surf_temp, surf_ah)

  ref_0 <- psychRomet::potential_temp(ref_temp, p_atm) + 273.15
  surf_0 <- psychRomet::potential_temp(surf_temp, p_atm) + 273.15

  tDiffMeas <- ref_0 - surf_0

  loglinfun <- function(params){ # the input variable params must be the same length and dimension as the initial guesses (i.e. start points)
    Qh <- params[[1]]
    d_0 <- dplyr::if_else(is.na(d_0), params[[2]], d_0) # estimate d_0 if not given

    tDiffSim <- (Qh/(k*u_star*rho_air*C_p))*(log((zHeight - d_0)/(z_0t)) - phi_h) # log-linear wind speed function

    obj_fn <- tDiffSim - tDiffMeas
    SSE <- sum(obj_fn^2) # this is the sum square error, which is the value that we are trying to minimize.

    #out <- list('SSE' = SSE, 'FittedWspeed' = FittedWspeed) # create list for multi arg return
    return(SSE)
  }

  # set initial guesses for friction velocity and roughness length
  start_point <- c(0.5, 0.5)

  parameter_estimates <- stats::optim(par = start_point, fn = loglinfun)

  out <- list('par_est' = parameter_estimates, 'model' = loglinfun)

  est_pars <- data.frame(
    Q_h = out$par_est$par[1],

    d_0 = out$par_est$par[2]
  )

  return(est_pars)

}
