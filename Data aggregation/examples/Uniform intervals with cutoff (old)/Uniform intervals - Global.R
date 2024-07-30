###############  DATA AGGREGATION ####################
############### UNIFORM INTERVALS ####################

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

# ------------------------- CODE STARTS HERE ------------------------

# Calculate number of data nodes from files fitting the pattern in the working directory
# This assumes cutoff times outputs have a name like Cutoff_site_[[:digit:]]+.csv
K=length(list.files(pattern="Cutoff_site_[[:digit:]]+.csv"))

# Find the lowest cutoff value from all sites
if (file.exists(paste0("Cutoff_site_", K, ".csv")) && !file.exists(paste0("Interval_size_site_", K, ".csv"))) {
  
  # Before anything else, check if values are compatible (overlap is present between sites)
  min_values <- numeric(K)
  max_values <- numeric(K)
  for (k in 1:K) {
    min_max <- read.csv(paste0("Start_end_values_site_", k, ".csv"))
    min_values[k] <- min_max$start
    max_values[k] <- min_max$end
  }
  
  error_flag <- any(outer(min_values, max_values, ">"))
  if (error_flag) {
    message("WARNING: Distributions seem to be non homogeneous. Proceed with caution.")
  }
  
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
} else if (file.exists(paste0("Interval_size_site_", K, ".csv"))){
  
  # Loop over sites
  max_size <- 0
  for(k in 1:K){
    size_local <- read.csv(paste0("Interval_size_site_", K, ".csv"))
    if (size_local > max_size){
      max_size <- size_local
    }
  }
  
  # Write interval size in CSV
  write.csv(max_size, file="Global_interval_size.csv", row.names = FALSE)
  
}

## Remove all environment variables. 
## If you want to see the variable that were created, simply don't execute that line (and clear them manually after)
rm(list = ls())
