#------------------
# author: Christiane Uhlig, Graduate School of Oceanography, URI
# Version: 1.0 April 2017
# Email: cuhlig@uri.edu
#------------------



#calibrate delta CH4 vs daily calibration curve and calculate concentration in hs after dilution with SSIM


calCH4deltappm <- function(Picarro.ppm, b.ppm, m.ppm, SSIM.dil.factor, Picarro.delta, b.delta, m.delta){
  
  # Inputs: 
  #   Picarro.ppm: Picarro ppm concentration measurement 
  #   Picarro.delta: Picarro isotope ratio of CH4 
  
  #
  # Output:
  #   calibrated.ppm: calibrated concentration of CH4 
  #   calibrated.delta: calibrated isotope ratio of CH4

  
  calibrated.ppm.x = m.ppm * Picarro.ppm + b.ppm
  hs.ppm.x <- calibrated.ppm.x * SSIM.dil.factor
  
  calibrated.delta.x = m.delta * Picarro.delta + b.delta

  
  calibrated.ppm <- c(calibrated.ppm.x)
  hs.ppm <- c(hs.ppm.x)
  
  calibrated.delta <- c(calibrated.delta.x)

  
  
  calibrated.CH4 <- data.frame(calibrated.ppm, hs.ppm, calibrated.delta)
  
  
  return(calibrated.CH4)
}


#test <- calCH4ppm(2,3,4,5,6,2)