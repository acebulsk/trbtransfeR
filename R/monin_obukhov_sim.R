#' Monin Obukov Simularity Variable
#'
#' Used to determine atmospheric stability.
#'     Stable: x > 0
#'     Neutral: x ~= 0
#'     Unstable: x < 0
#'
#'     From shuttleworth et al., 2012 eq. 20.10
#'
#' @param u_star friction velocity (m/s)
#' @param T_c air temperature (celsuis)
#' @param z measurement height (m)
#' @param Q_h surface kinematic heat flux (w / m2)
#' @param d_0 displacement height (m)
#' @param k von karman constant (0.4)
#' @param g acceleration due to gravity (9.81 m s -2 )
#'
#'
#' @return dimensionless
#' @export
#'
#' @examples
monin_obukhov_sim <- function(u_star, T_c, z, Q_h, d_0, g = 9.81, k = 0.4){
    T_k <- T_c + 273.15
    L <- -(u_star^3 * T_k) / (k * g * Q_h)

    return((z-d_0)/L)
}

