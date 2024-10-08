% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stability_corrections.R
\name{phi_stbl}
\alias{phi_stbl}
\title{STABLE CASE: Stability Correction for Momentum, Heat, or Water Vapour Flux}
\usage{
phi_stbl(z_L)
}
\arguments{
\item{z_L}{measurement height divided by the monin obukhov length (unitless)}
}
\value{
stability correction to be used in the optim flux calculations, if z_l is less than zero returns NA.
}
\description{
Provides a correction for the stable or neutral case for any of the flux types
}
