#------------------
# author: Christiane Uhlig, Graduate School of Oceanography, URI
# Version: 1.0 April 2017
# Email: cuhlig@uri.edu
#------------------

#bunsen coefficient at given temp and sal

#Bunsen coefficient (beta) calculation and constants (Yamamoto 1986)
#ln(beta) = A1 + A2 * (100/T) + A3 * ln(T/100)+ S * [B1 + B2(T/lOO) + B3(T/100)^(2)] # with T: temperature in Kelvin, S: salinity

bunsen <- function(temp, sal){
  
  # Inputs: 
  #   temp: temperature during sample equilibration in degress celsius
  #   sal: sea water salinity in PSU
  #
  # Output:
  #   bunsen.coefff: bunsen coefficient at given temp and sal

  A1 <- -67.1962
  A2 <- 99.1624
  A3 <- 27.9015
  B1 <- -0.072909
  B2 <- 0.041674
  B3 <- -0.0064603

  # source("./IGC.R")
  # IGC <- idealgasconstant(temp)
  
  T.equil.K <- temp + 273.15
  
  bunsen.coeff.L.L.x <- exp(A1 + A2*(100/T.equil.K) + A3*(log(T.equil.K/100)) + sal*(B1 + B2*(T.equil.K/100) + B3*((T.equil.K/100)^2)))
  bunsen.coeff.mol.L.x <- bunsen.coeff.L.L.x*IGC
  
  
  bunsen.coeff.L.L <- c(bunsen.coeff.L.L.x)
  bunsen.coeff.mol.L <- c(bunsen.coeff.mol.L.x)
  
  bunsen.coeff <- data.frame(bunsen.coeff.L.L, bunsen.coeff.mol.L)
  
  return(bunsen.coeff)
}

# bunsen.coeff <- bunsen(0, 71.4)
# bunsen.coeff
