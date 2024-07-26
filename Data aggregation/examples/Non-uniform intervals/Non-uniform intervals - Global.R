##################  DATA AGGREGATION ######################
################ NON-UNIFORM INTERVALS ####################

## License: https://creativecommons.org/licenses/by-nc-sa/4.0/
## Copyright: GRIIS / Université de Sherbrooke

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

# Import parameters (do not edit)
# See Data_aggregation_Brief_Summary for explanation
params <- read.csv("Parameters.csv", header = FALSE)

lower_bound <- params[params$V1 == "lower_bound", "V2"]
upper_bound <- params[params$V1 == "upper_bound", "V2"]
step <- params[params$V1 == "step", "V2"]
interval_size <- params[params$V1 == "interval_size", "V2"]
increase <- params[params$V1 == "increase", "V2"]

# ------------------------- CODE STARTS HERE ------------------------

# Calculate number of data nodes from files fiting the pattern in the working directory
# This assumes unique event times outputs have a name like Times_[[:digit:]]+_output.csv
K=length(list.files(pattern="Binary_output_site_[[:digit:]]+.csv"))

if (step > interval_size){
  print("Warning: The value of 'step' is bigger than the value of 'interval_size', which may cause suboptimal partionning.")
}

# Aggregate binary output matrices from all sites
for (i in 1:K) {
  binary_output_site <- as.matrix(read.csv(paste0("Binary_output_site_", i, ".csv")))
  if (i == 1) {
    binary_output_global <- binary_output_site
  } else {
    binary_output_global <- binary_output_global + binary_output_site
  }
}

write.csv(binary_output_global, file=paste0("Binary_output_site_global.csv"), row.names = FALSE)

# Initialize variables for finding intervals
intervals <- list()
initial_interval_size <- interval_size
value <- lower_bound
nbRows <- nrow(binary_output_global)
done <- FALSE
position <- 1

# Loop through positions in the binary output matrix to find valid intervals
# Check Data_aggregation_Brief_Summary for a clearer explanation of the algorithm
while (position < ncol(binary_output_global)) {
  for (i in 1:nbRows) {
    if (!done) {
      # Check if the interval is valid across all sites
      if (binary_output_global[i, position] == K) {
        # Calculate the interval start value and append it to the list
        intervals <- append(intervals, value)
        value <- value + (initial_interval_size + (i - 1) * increase)
        
        # Update the next position to check for the next interval
        position <- floor((value - lower_bound) / step) + 1
        done <- TRUE
      }
      # If reached the last row without finding a valid interval, exit the loop
      if (i == nbRows) {
        position <- ncol(binary_output_global)
      }
    }
  }
  done <- FALSE
}

# Add the last interval and merge the last two together
intervals <- append(intervals, upper_bound + 1)
intervals <- intervals[-(length(intervals) - 1)]

write.csv(intervals, file=paste0("Global_intervals.csv"), row.names = FALSE)

## Remove all environment variables. 
## If you want to see the variable that were created, simply don't execute that line (and clear them manually after)
#rm(list = ls())
