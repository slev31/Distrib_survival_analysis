##################  DATA AGGREGATION ######################
################ NON-UNIFORM INTERVALS ####################

## License: https://creativecommons.org/licenses/by-nc-sa/4.0/
## Copyright: GRIIS / Université de Sherbrooke

# Site number
siteNb <- ...   # Input here the site number

# Import parameters (do not edit)
# See Data_aggregation_Brief_Summary for explanation
params <- read.csv("Parameters.csv", header = FALSE)

nbDataGrouped <- params[params$V1 == "nbDataGrouped", "V2"] + 1
lower_bound <- params[params$V1 == "lower_bound", "V2"]
upper_bound <- params[params$V1 == "upper_bound", "V2"]
step <- params[params$V1 == "step", "V2"]
interval_size <- params[params$V1 == "interval_size", "V2"]
increase <- params[params$V1 == "increase", "V2"]

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

# ------------------------- CODE STARTS HERE ------------------------

if (step < interval_size){
  print("Warning: The value of 'step' is smaller than the value of 'interval_size', which may cause suboptimal partionning.")
}

# Start by computing and sending interval size matrix
if (!file.exists("Global_intervals.csv")) {
  
  # Read data
  data1 <- read.csv(paste0("Data_site_", siteNb, ".csv"))
  data1 <- data1[order(data1$time), ]
  
  # Data initialization
  left_border <- lower_bound
  right_border <- lower_bound + interval_size
  initial_interval_size <- interval_size
  
  # Calculate the number of different interval types and the maximum number of intervals
  # Different interval types depends of min, max, interval_size and increase
  # Maximum number of intervals depends on min, max, the smallest interval size and the step
  nbTypesOfIntervals <- floor(((upper_bound - lower_bound) - interval_size) / increase) + 1
  maxNbOfIntervals <- floor(((upper_bound - interval_size) - lower_bound) / step) + 1
  
  # Initialize a binary output matrix to store results
  binary_output_site1 <- matrix(0, nrow = nbTypesOfIntervals, ncol = maxNbOfIntervals)
  
  # Calculate the binary output matrix
  # Check small interval size for each position, then increase size and check for each position, then increase, etc.
  # Outer loop to iterate over different interval sizes
  j <- 1
  while (interval_size < (upper_bound - lower_bound)) {
    
    # Inner loop to iterate over different interval positions
    i <- 1
    while (right_border < upper_bound) {
      
      # Check if the number of data points within the interval meets the threshold
      if (sum(data1$time >= left_border & data1$time < right_border) >= nbDataGrouped) {
        binary_output_site1[j,i] <- 1
      }
      
      # Move the interval window to the right
      left_border <- left_border + step
      right_border <- right_border + step
      i <- i + 1
    }
    
    # Increase the interval size and reset the interval positions
    interval_size <- interval_size + increase
    left_border <- lower_bound
    right_border <- lower_bound + interval_size
    
    j <- j + 1
  }
  
  # Save the binary output matrix to a CSV file
  write.csv(binary_output_site1, file=paste0("Binary_output_site_", siteNb, ".csv"), row.names = FALSE)
  
} else {
  
  # If the global intervals file exists, change times according to intervals sent by global server
  data1 <- read.csv(paste0("Data_site_", siteNb, ".csv"))
  intervals <- read.csv(paste0("Global_intervals.csv"))
  
  # Bin the times into intervals specified by the global intervals file
  data1$time <- cut(data1$time, breaks = c(-Inf, intervals), labels = FALSE, right = FALSE)
  data1 <- data1[order(data1$time), ]
  
  # Save the grouped data to a new CSV file
  write.csv(data1, file=paste0("Grouped_Data_site_", siteNb, ".csv"), row.names = FALSE)
  
}

## Remove all environment variables. 
## If you want to see the variable that were created, simply don't execute that line (and clear them manually after)
rm(list = ls())
