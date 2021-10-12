#' Calculate Friction Velocity
#'
#' Use this if you have all params listed below. If missing more than 1 use the fit_neitral_wind optimizer. This formula is taken from the head and mass exchange lecture slide 19
#'
#' @param u wind speed m s-1
#' @param z measurement height (m)
#' @param d_0 displacement height (m)
#' @param z0_m roughness length of momentum (m)
#' @param k 0.4 von karmans constant
#'
#' @return friction velocity (m/s)
#' @export
#'
#' @examples
friction_velocity <- function(u, z, d_0, z0_m, k = 0.4) {
  (u * k) / log((z-d_0)/z0_m)
}

#' Calculate Ustar, Z_0m, and, d_0 Given Measured Wind Speed
#'
#' @param uMeas measured wind speed (m/s)
#' @param zHeight height of wind measurement (m)
#'
#' @return List of optimization result. Params are accessed by 'returnedobj'$par_est.
#' @export
#'
#' @examples df <- data.frame(
#' U_1 = c(4.6, 6.0, 7.6, 9.0),
#'
#' Z_1 = c(1, 3, 10, 30)
#' )
#'
#'
#'
#' # Find parameters
#'fitted_wind <- fit_neutral_wind(df$U_1, df$Z_1)
#'
#'# Put estimated parameters in data frame
#'est_pars <- data.frame(
#'  ustar = fitted_wind$par_est$par[1],
#'
#'  z_0m = fitted_wind$par_est$par[2],
#'
#'  d_0 = fitted_wind$par_est$par[3]
#')
#' Use helper function fit_neutral_wind_helpr to confirm worked
#'
#'
#'
#'
fit_neutral_wind <- function(uMeas, zHeight, z_0m = NA, d_0 = NA){
  loglinfun <- function(params){ # the input variable params must be the same length and dimension as the initial guesses (i.e. start points)
    ustar <- params[[1]]
    z_0m <- dplyr::if_else(is.na(z_0m), params[[2]], z_0m) # estimate d_0 if not given
    d_0 <- dplyr::if_else(is.na(d_0), params[[3]], d_0) # estimate d_0 if not given
    FittedWspeed <- ustar/0.4*log((zHeight - d_0)/(z_0m)) # log-linear wind speed function

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
#' @examples  fit_neutral_wind_helpr(c(est_pars$ustar, est_pars$z_0m, est_pars$d_0), meas_wind$height_m)

fit_neutral_wind_helpr <- function(params, zHeight){ # params must be the same length and dimension as the initial guesses (i.e. start points)
  ustar <- params[[1]]
  z_0m <- params[[2]]
  d_0 <- params[[3]]

  FittedWspeed <- ustar/0.4*log((zHeight - d_0)/(z_0m)) # log-linear wind speed function

  return(FittedWspeed)
}
