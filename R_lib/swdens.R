#------------------
# author: Christiane Uhlig, Graduate School of Oceanography, URI
# Version: 1.0 April 2017
# Email: cuhlig@uri.edu
#------------------


### calculaste seawater density from temperature and salinity 
### after Millero and Poisson, 1981, Deap Sea Research

swdens <- function(temp, sal){
  
  # Inputs: 
  #   temp: temperatures in degress celsius
  #   sal: sea water salinity in PSU
  #
  # Output:
  #   sea water density in kg/m^3 (g/L)
  
  A = 8.24493e-1 - 4.0899e-3*temp + 7.6438e-5*temp^2 - 8.2467e-7*temp^3 + 5.3875e-9*temp^4
  B = - 5.72466e-3 + 1.0227e-4*temp - 1.6546e-6 * temp^2
  C = 4.8314e-4
  
  # Calculating the water density
  rho.w <- 999.842594 + 6.793952e-2*temp - 9.095290e-3*temp^2 + 1.001685e-4*temp^3 - 1.120083e-6*temp^4 + 6.536336e-9*temp^5
  
  # Calculating the sea water density 
  rho.sw <- rho.w + A*sal + B*(sal^1.5) + C*(sal^2)
  
  return(rho.sw)
}

#print (swdens(0.0, 35))

# data.bag$rho.sw <- swdens(data.bags$T.equil, data.bags$sal)