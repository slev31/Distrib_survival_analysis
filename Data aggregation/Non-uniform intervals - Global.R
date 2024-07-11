##################  DATA AGGREGATION ######################
################ NON-UNIFORM INTERVALS ####################

## License: https://creativecommons.org/licenses/by-nc-sa/4.0/
## Copyright: GRIIS / Université de Sherbrooke

# Includes
library("survival")
library("survminer")
library("dplyr")

numberOfSites <- 4  # Input here the number of sites

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

for (i in 1:numberOfSites){
  binary_output_site <- as.matrix(read.csv(paste0("Binary_output_site_", i, ".csv")))
  if (i==1){
    binary_output_global <- binary_output_site
  } else {
    binary_output_global <- binary_output_global + binary_output_site
  }
}

# while initialization
intervals <- list()
initial_interval_size <- interval_size
value <- lower_bound
nbRows <- nrow(binary_output_global)
done <- FALSE
position <- 1

while (position < ncol(binary_output_global)) {
  for (i in 1:nbRows) {
    if (done == FALSE){
      if (binary_output_global[i, position] == numberOfSites) {
        value <- value + (initial_interval_size + (i-1) * increase)
        intervals <- append(intervals, value)
        position <- floor((value - lower_bound)/step)
        done <- TRUE
      }
      if (i == nbRows){ # If nothing else, stop the loop
        position <- ncol(binary_output_global)
      }
    }
  }
  done <- FALSE
}

# Add last interval, and merge the last two together
intervals <- append(intervals,upper_bound+1)
intervals <- intervals[-(length(intervals) - 1)]

write.csv(intervals, file=paste0("Global_intervals.csv"), row.names = FALSE)

## Remove all environment variables. 
## If you want to see the variable that were created, simply don't execute that line (and clear them manually after)
rm(list = ls())
