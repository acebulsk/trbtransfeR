#' Calculate Friction Velocity
#'
#' Use this if you have all params listed below. If missing more than 1 use the fit_neitral_wind optimizer. This formula is taken from the head and mass exchange lecture slide 19
#'
#' @param uMeas measured wind speed m s-1
#' @param zHeight measurement height (m)
#' @param d_0 displacement height (m)
#' @param z0_m roughness length of momentum (m)
#' @param phi_m stability correction due to momentum. 0 is for neutral or stable case
#' @param k von karmans constant
#'
#' @return friction velocity (m/s)
#' @export
#'
#' @examples

friction_velocity <- function(uMeas, zHeight, d_0, z_0m, phi_m, k = 0.4) {

  u_star <- uMeas * k * (log((zHeight-d_0)/z_0m) - phi_m) ^-1

  return(u_star)
}

#' Least Squares Calculate Parameters Ustar, Z_0m, and, d_0 Given Measured Wind Speed and stability correction.
#'
#' @param uMeas measured wind speed (m/s)
#' @param zHeight height of wind measurement (m)
#' @param d_0 displacement height (m)
#' @param z0_m roughness length of momentum (m)
#' @param phi_m stability correction due to momentum. 0 is for neutral or stable case.
#' @param k 0.4 von karmans constant
#'
#' @return List of optimization result including ustar (friction velocity), Z_0m (roughness length), d_0 (displacement height) Params are accessed by 'returnedobj'$...
#' @export
#'
#' @examples df <- data.frame(
#' uMeas = c(4.6, 6.0, 7.6, 9.0),
#'
#' zHeight = c(1, 3, 10, 30)
#' )
#'
#'
#'
#'
optim_wind_params <- function(uMeas, zHeight, z_0m = NA, d_0 = NA, phi_m, k = 0.4){
  loglinfun <- function(params){ # the input variable params must be the same length and dimension as the initial guesses (i.e. start points)
    ustar <- params[[1]]
    z_0m <- dplyr::if_else(is.na(z_0m), params[[2]], z_0m) # estimate d_0 if not given
    d_0 <- dplyr::if_else(is.na(d_0), params[[3]], d_0) # estimate d_0 if not given
    FittedWspeed <- ustar/k*log((zHeight - d_0)/(z_0m)) # log-linear wind speed function

    obj_fn <- FittedWspeed - uMeas
    SSE <- sum(obj_fn^2) # this is the sum square error, which is the value that we are trying to minimize.

    #out <- list('SSE' = SSE, 'FittedWspeed' = FittedWspeed) # create list for multi arg return
    return(SSE)
  }

  # set initial guesses for friction velocity and roughness length

  start_point <- c(0.5, 0.5, 0.5)

  parameter_estimates <- stats::optim(par = start_point, fn = loglinfun)

  out <- list('par_est' = parameter_estimates, 'model' = loglinfun)

  est_pars <- data.frame(
   ustar = out$par_est$par[1],

   z_0m = out$par_est$par[2],

   d_0 = out$par_est$par[3]
  )
  return(est_pars)
}


#' Helper Function to Estimate Wind Speed
#'
#' @param params estimated from fit_neutral_wind function (list of 3 params)
#' @param zHeight heights that you want wind speed estimated at
#'
#' @return list of estimated wind speeds
#' @export
#'
#' @examples  NA

fit_neutral_wind_helpr <- function(params, zHeight){ # params must be the same length and dimension as the initial guesses (i.e. start points)
  ustar <- params[[1]]
  z_0m <- params[[2]]
  d_0 <- params[[3]]

  FittedWspeed <- ustar/0.4*log((zHeight - d_0)/(z_0m)) # log-linear wind speed function

  return(FittedWspeed)
}
