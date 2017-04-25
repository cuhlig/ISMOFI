#------------------
# author: Christiane Uhlig, Graduate School of Oceanography, URI
# Version: 1.0 April 2017
# Email: cuhlig@uri.edu
#------------------


# calculate y-axis for high and low epsilon to calculate oxidation rate from isotopic ratios

yaxis <- function(
  calibrated.delta,
  alpha.PS.low,
  alpha.PS.high
)
  
{  
  # Inputs: 
  #     calibrated.delta,
  #     alpha.PS.low,
  #     alpha.PS.high,
  #
  # Output:
  #   yaxis.low
  #   yaxis.high


  yaxis.low.x <- (log((10^(3)+calibrated.delta)/((10^3)+calibrated.delta[1])))/((1/alpha.PS.low)-1)
  yaxis.high.x <- (log((10^(3)+calibrated.delta)/((10^3)+calibrated.delta[1])))/((1/alpha.PS.high)-1)
  
  
  yaxis.low <- c(yaxis.low.x)
  yaxis.high <- c(yaxis.high.x)

  yaxis <- data.frame(yaxis.low.x, yaxis.high.x)
  
  return(yaxis)
  
}  
#  test <- yaxisHP(2,3,4)

