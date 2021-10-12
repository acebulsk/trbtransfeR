lst <- fit_neutral_wind(uMeas = 2, zHeight = 2.5, z_0m = 0.001, d_0 = 0)
lst$ustar
lst$z_0m
lst$d_0

friction_velocity(2, 2.5, 0, 0.001)

qh <- fit_neutral_temp(ref_temp = 5, surf_temp = 20, rho_air = 1.19, zHeight = 2.5, u_star = lst$ustar, z_0m = 0.001, q = 0.01436213, d_0 = 0)

qh2 <- sensible_H_flux(ref_temp = 20, surf_temp = 45, rho_air = 1.19, zHeight = 2.5, u_star = lst$ustar, z_0m = 0.001, q = 0.01436213, d_0 = 0)

qh$Q_h

qh$d_0

