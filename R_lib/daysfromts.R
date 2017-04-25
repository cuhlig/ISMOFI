#------------------
# author: Christiane Uhlig, Graduate School of Oceanography, URI
# Version: 1.0 April 2017
# Email: cuhlig@uri.edu
#------------------

# calculated days from start of experiment

daysfromts <- function(Timestamp){
  
  # Inputs: 
  #   Timestamp: Timestamp from Picarro file
  #
  # Output:
  #   time.days: time in days from start of experiment i.e. compared to 1st entry in dataframe
  
  
  time.days <-
    (Timestamp - Timestamp[1]) / 86400 
  
  
  return(time.days)
}