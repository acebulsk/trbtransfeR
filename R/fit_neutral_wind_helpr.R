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
