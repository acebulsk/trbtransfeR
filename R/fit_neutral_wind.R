
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
#'fitted_wind <- fit_neutral_wind(meas_wind$wind_spd_ms, meas_wind$height_m)
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
fit_neutral_wind <- function(uMeas, zHeight){
  loglinfun <- function(params){ # the input variable params must be the same length and dimension as the initial guesses (i.e. start points)
    ustar <- params[[1]]
    z_0m <- params[[2]]
    d_0 <- params[[3]]
    FittedWspeed <- ustar/0.4*log((zHeight - d_0)/(z_0m)) # log-linear wind speed function

    obj_fn <- FittedWspeed - uMeas
    SSE <- sum(obj_fn^2) # this is the sum square error, which is the value that we are trying to minimize.

    #out <- list('SSE' = SSE, 'FittedWspeed' = FittedWspeed) # create list for multi arg return
    return(SSE)
  }

  # set initial guesses for friction velocity and roughness length
  start_point <- c(0.5, 0.5, 0.5)

  parameter_estimates <- optim(par = start_point, fn = loglinfun)

  out <- list('par_est' = parameter_estimates, 'model' = loglinfun)
  return(out)
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
