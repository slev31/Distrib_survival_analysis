##################  DATA AGGREGATION ######################
################ NON-UNIFORM INTERVALS ####################

## License: https://creativecommons.org/licenses/by-nc-sa/4.0/
## Copyright: GRIIS / Université de Sherbrooke

# Includes
library("survival")
library("survminer")
library("dplyr")

siteNb <- 2           # Input here the site number
nbDataGrouped <- 5    # Input here the number of subjects grouped together

### ALL VALUES HERE SHOULD BE THE SAME FOR EVERY SITE

lower_bound <- 0      # Input here the lower bound, or lowest value (leave a 0 if it is unknown)
upper_bound <- 100    # Input here the upper bound, or highest value

step <- 1             # Input here the step size (distance between values where intervals start)
interval_size <- 1    # Input here the initial interval_size
increase <- 1         # Input here the interval_size increase

# If you want to skip the automated working directory setting, input 1 here. 
# If you do so, make sure the working directory is set correctly manualy.
manualwd <- -1

if (manualwd != 1) {
  
  # Set working directory automatically
  
  # this.path package is available
  if (require(this.path)) {
    setwd(this.dir())
    
    # else if running in R studio and the rstudioapi is available, set the correct working directory
  } else if ((Sys.getenv("RSTUDIO") == "1") & (require("rstudioapi"))) {
    print("RSTUDIO")
    path <- dirname(rstudioapi::getActiveDocumentContext()$path)
    setwd(path)
    
    # no known means to automatically set working directory
  } else {
    stop("The required conditions to automatically set the working directory are not met. See R file")
  }
} else {
  print("The automated working directory setup has been bypassed. If there is an error, this might be the cause.")
}

### Code starts here

# Start by computing and sending interval size matrix
if (!file.exists("Global_intervals.csv")) {
  data1 <- read.csv(paste0("Data_site_", siteNb, ".csv"))
  data1 <- data1[order(data1$time), ]
  
  # Data initialization
  left_border <- lower_bound
  right_border <- lower_bound + interval_size
  initial_interval_size <- interval_size
  
  nbTypesOfIntervals <- floor(((upper_bound-lower_bound) - interval_size)/increase)+1
  maxNbOfIntervals <- floor(((upper_bound-interval_size)-lower_bound)/step)+1
  
  binary_output_site1 <- matrix(0, nrow = nbTypesOfIntervals, ncol = maxNbOfIntervals)
  
  j <- 1
  while (interval_size < (upper_bound-lower_bound) ){
    
    i <- 1
    while (right_border < upper_bound){
      
      if(sum(data1$time >= left_border & data1$time < right_border) >= nbDataGrouped) {
        binary_output_site1[j,i] <- 1
      }
      
      left_border <- left_border + step
      right_border <- right_border + step
      i <- i + 1
    }
    
    interval_size <- interval_size + increase
    left_border <- lower_bound
    right_border <- lower_bound + interval_size
    
    j <- j + 1
  }
  
  write.csv(binary_output_site1, file=paste0("Binary_output_site_", siteNb, ".csv"), row.names = FALSE)
  
} else {

  # Change times according to intervals sent by global server
  data1 <- read.csv(paste0("Data_site_", siteNb, ".csv"))
  intervals <- read.csv(paste0("Global_intervals.csv"))
  
  data1$time <- cut(data1$time, breaks = c(-Inf, intervals), labels = FALSE, right = FALSE)
  data1 <- data1[order(data1$time), ]
  
  write.csv(data1, file=paste0("Grouped_Data_site_", siteNb, ".csv"), row.names = FALSE)
  
}

## Remove all environment variables. 
## If you want to see the variable that were created, simply don't execute that line (and clear them manually after)
rm(list = ls())
