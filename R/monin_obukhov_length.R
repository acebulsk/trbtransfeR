#' Monin Obukov Length
#'
#' Used to determine atmospheric stability.
#'     Stable: z/L > 0
#'     Neutral: z/L ~= 0
#'     Unstable: z/L < 0
#'
#'     From shuttleworth et al., 2012 eq. 20.10
#'
#' @param u_star friction velocity (m/s)
#' @param T_c air temperature (celsuis)
#' @param rho_air density of moist air
#' @param z measurement height (m)
#' @param Q_h surface kinematic heat flux (w / m2)
#' @param d_0 displacement height (m)
#' @param k von karman constant
#' @param g acceleration due to gravity (m s -2 )
#' @param C_p (specific heat at constant pressure J kg-1 K-1
#'
#'
#' @return L in metres. stability (x) = z - d_0 / L
#' @export
#'
#' @examples
monin_obukhov_length <- function(u_star, T_c, rho_air, z, Q_h, d_0, g = 9.81, k = 0.4, C_p = 1005){
    T_k <- T_c + 273.15
    L <- dplyr::if_else(Q_h == 0,
                   1e6,
                   -(u_star^3 * T_k * rho_air * C_p) / (k * g * Q_h)
                   )
    return(L)
}

