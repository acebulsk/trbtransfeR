# library(psychRomet)
# p_atm <- 101.325
# rh <- .60
# e_s <- tetens(T_c) # kpa
# e_a <- actual_vapour_pressure(e_s, rh)
#
# #z_0m <- 0.001 # wet sand
# z_0m_crop <- 0.15 # 1 m crop from wieringa, 1992
#
# ref_temp <- 35
# surf_temp <- 20
#
# z <- 2.5
#
# d_0_crop <- 1
#
# spec_humid <- specific_humidity(e_s, p_atm) # g / g
# mixing_ratio <- mixing_ratio_p(e_a, p_atm) # g/g
#
# rho_air <- density_moist_air(ref_temp, rh, p_atm) / 1000 # kg
#
# lst <- optim_wind_params(uMeas = 2, z, z_0m_crop, d_0_crop, phi_m = 0)
# lst$ustar
# lst$z_0m
# lst$d_0
#
# friction_velocity(2, 2.5, 0, 0.001, phi_m = 0)
#
# qh2 <- sensible_H_flux(ref_temp = ref_temp, surf_temp = surf_temp, p_atm = p_atm, rho_air = rho_air, zHeight = z, u_star = lst$ustar, z_0m = z_0m_crop, d_0 = d_0_crop, phi_m = 0)
#
# qh2
#
# water_vapour_flux(ref_temp = ref_temp, surf_temp = surf_temp, p_atm = p_atm, ref_rh = 0.6, surf_rh = 1, rho_air = rho_air, zHeight = z, u_star = lst$ustar, z_0m = z_0m_crop , d_0 = d_0_crop, phi_m = 0)
#
# monin_obukhov_sim(u_star = lst$ustar, T_c = ref_temp, z = z, Q_h = qh2, d_0 = d_0_crop)
