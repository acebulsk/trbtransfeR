#' Estimate Turbulent Sensible Heat Flux
#'
#' Use this function if have all params listed below. If not use the optimiser function fit_neutral_temp. Taken from This formula is taken from the head and mass exchange lecture slide 19.
#'
#' @param ref_temp temperature at reference height
#' @param surf_temp temperautre of surface
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

sensible_H_flux <- function(ref_temp, surf_temp, rho_air, zHeight, u_star, z_0m, q, k = 0.4, C_pl = 1004.67, d_0 = NA){
  C_p <- C_pl * (1+0.84*q)

  z_0t <- z_0m * 0.1

  ((ref_temp - surf_temp) * (k*u_star*rho_air*C_p)) / log((zHeight - d_0)/(z_0t))
}

#' Estimate Turbulent Sensible Heat Flux
#'
#' @param ref_temp temperature at reference height
#' @param surf_temp temperautre of surface
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
fit_neutral_temp <- function(ref_temp, surf_temp, rho_air, zHeight, u_star, z_0m, q, k = 0.4, C_pl = 1004.67, d_0 = NA){
  tDiffMeas <- ref_temp - surf_temp

  C_p <- C_pl * (1+0.84*q)
  z_0t <- z_0m * 0.1

  loglinfun <- function(params){ # the input variable params must be the same length and dimension as the initial guesses (i.e. start points)
    Qh <- params[[1]]
    d_0 <- dplyr::if_else(is.na(d_0), params[[2]], d_0) # estimate d_0 if not given

    tDiffSim <- (Qh/(k*u_star*rho_air*C_p))*log((zHeight - d_0)/(z_0t)) # log-linear wind speed function

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
