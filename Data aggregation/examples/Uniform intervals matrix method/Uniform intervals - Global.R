##################  DATA AGGREGATION ######################
################## UNIFORM INTERVALS ######################

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
increase <- params[params$V1 == "increase", "V2"]
interval_size <- increase

# ------------------------- CODE STARTS HERE ------------------------

# Calculate number of data nodes from files fitting the pattern in the working directory
# This assumes cutoff times outputs have a name like Cutoff_site_[[:digit:]]+.csv
K=length(list.files(pattern="Cutoff_site_[[:digit:]]+.csv"))

# Find the lowest cutoff value from all sites
if (file.exists(paste0("Cutoff_site_", K, ".csv")) && !file.exists(paste0("Binary_output_site_", K, ".csv"))) {
  
  # Loop over sites
  min_cutoff <- Inf
  for(k in 1:K){
    cutoff_local <- read.csv(paste0("Cutoff_site_", K, ".csv"))
    if (cutoff_local < min_cutoff){
      min_cutoff <- cutoff_local
    }
  }
  
  # Write cutoff in CSV
  write.csv(min_cutoff, file="Global_cutoff.csv", row.names = FALSE)
  
  # Find the biggest interval size from all sites
} else {
  
  # Aggregate binary output matrices from all sites
  for (i in 1:K) {
    binary_output_site <- as.matrix(read.csv(paste0("Binary_output_site_", i, ".csv")))
    if (i == 1) {
      binary_output_global <- binary_output_site
    } else {
      binary_output_global <- binary_output_global + binary_output_site
    }
  }
  
  # Initialize variables for finding intervals
  first_one_index <- which(binary_output_global == K)[1]
  value <- lower_bound + interval_size + (first_one_index - 1) * increase
  
  cutoff_value <- as.integer(read.csv("Global_cutoff.csv"))
  intervals <- seq(from = lower_bound, to = cutoff_value, by = value)
  
  write.csv(intervals, file=paste0("Global_intervals.csv"), row.names = FALSE)
}

## Remove all environment variables. 
## If you want to see the variable that were created, simply don't execute that line (and clear them manually after)
rm(list = ls())
