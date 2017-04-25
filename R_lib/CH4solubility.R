#------------------
# author: Christiane Uhlig, Graduate School of Oceanography, URI
# Version: 1.0 April 2017
# Email: cuhlig@uri.edu
#------------------


#CH4 solubility in water

#calculate nM CH4 in water
#headspace calculation after Magen et al 2014
#n(CH4,w) = (beta) * (p(CH4,hs) * V(gas) * (V(water)/V(hs)) / R*(273.15+T)) # with T: temperature in Celsius, V:volumes in L
#n(CH4,hs) = (p(CH4,hs) * V(gas) * P(hs))/(V(hs)) / R*(273.15+T))
#c(CH4,init) = (n(CH4,w) + n(CH4,hs)) / V.water

#Bunsen coefficient (beta) calculation and constants (Yamamoto 1986)
#ln(beta) = A1 + A2 * (100/T) + A3 * ln(T/100)+ S * [B1 + B2(T/lOO) + B3(T/100)^(2)] # with T: temperature in Kelvin, S: salinity

CH4solubility <- function(temp, sal, V.hs, V.gas, V.sw, hs.ppm){
  
  # Inputs: 
  #   temp: temperature during sample equilibration in degress celsius
  #   sal: sea water salinity in PSU
  #   hs.ppm: partial pressure of methane in the headspace in ppm
  #   V.hs: volume of headspace in L
  #   V.gas: voulume of gas introduced to create headspace in L
  #   V.sw: volume of seawater in L
  #
  # Output:
  #   bunsen.coefff: bunsen coefficient at given temp and sal
  #   n.CH4.hs: mass of CH4 in headspace after equilibration in mol
  #   n.CH4.water: mass of CH4 in water after equilibration in mol
  #   n.CH4.total: total mass of CH4 in water and headspace after equilibration of in inital sample in mol  
  #   c.CH4.init.water: concentration of CH4 in initial water sample in nmol/L
  
  
  A1 <- -67.1962
  A2 <- 99.1624
  A3 <- 27.9015
  B1 <- -0.072909
  B2 <- 0.041674
  B3 <- -0.0064603
  R <- 0.08206 #(L*atm)/(K*mol)
  
  T.equil.K <- temp + 273.15
  P.hs <- V.gas/V.hs
  
  bunsen.coeff.x <- exp(A1 + A2*(100/T.equil.K) + A3*(log(T.equil.K/100)) + sal*(B1 + B2*(T.equil.K/100) + B3*((T.equil.K/100)^2)))
  
  n.CH4.hs.x <- (hs.ppm*10^(-6)*V.hs*P.hs)/(R*(273.15+temp)) # mol
  n.CH4.water.x <- bunsen.coeff.x* (hs.ppm * 10^(-6) * V.gas * (V.sw/V.hs) / (R*(273.15+temp))) # mol
  n.CH4.total.x <- (n.CH4.hs.x + n.CH4.water.x)
  c.CH4.water.x <- (n.CH4.water.x/V.sw)*10^9
  c.CH4.init.water.x <- (n.CH4.total.x/V.sw)*10^9 # nmol/L #total pool of CH4 in system correted for the amount that was removed for sampling in the previous days
 
  bunsen.coeff <- c(bunsen.coeff.x)
  n.CH4.hs <- c(n.CH4.hs.x)
  n.CH4.water <- c(n.CH4.water.x)
  n.CH4.total <- c(n.CH4.total.x)
  c.CH4.water <- c(c.CH4.water.x)
  c.CH4.init.water <- c(c.CH4.init.water.x)

  
  solubility.values <- data.frame(bunsen.coeff, n.CH4.hs, n.CH4.water, n.CH4.total, c.CH4.water, c.CH4.init.water)
  
  return(solubility.values)
}

# data.bag$rho.sw <- swdens(data.bags$T.equil, data.bags$sal)

# solubility.values <- CH4sol(0, 35, 0.15, 0.15, 0.8, 3, 3)

