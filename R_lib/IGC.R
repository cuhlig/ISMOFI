#Ideal gas constant in mol/L at given temperature

  # Inputs: 
  #   temp: temperature during sample equilibration in degress celsius

  # Output:
  #   IGC: ideal gas constant
  
  
  #constants
  R <- 8.3141 #J/(mol*K) universal gas constant
  P <- 101325 # Pa [kg/(m*s^2)] = 1 atm
  
  idealgasconstant <- function(T) {
    
  IGC <- P/R/(T+273.16)/1000
  
  return(IGC)
  
  }


  # IGC <- idealgasconstant(0)
  # IGC