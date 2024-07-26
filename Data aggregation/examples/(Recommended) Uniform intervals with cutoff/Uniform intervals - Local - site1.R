###############  DATA AGGREGATION ####################
############### UNIFORM INTERVALS ####################

## License: https://creativecommons.org/licenses/by-nc-sa/4.0/
## Copyright: GRIIS / Université de Sherbrooke

siteNb <- 1               # Input here the site number

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

nbDataGrouped <- params[params$V1 == "nbDataGrouped", "V2"]
lower_bound <- params[params$V1 == "lower_bound", "V2"]
upper_bound <- params[params$V1 == "upper_bound", "V2"]
percent_excluded <- params[params$V1 == "percent_excluded", "V2"]

# ------------------------- CODE STARTS HERE ------------------------

#' This function updates the values of time by determining in which interval each value falls 
#' into and then replacing the original time value with the corresponding interval number.
#'
#' @param data A data frame containing the site data.
#' @param intervals A list containing the interval endpoints.
#' 
#' @return A data frame containing the site data where the time values have been modified.

modify_time <- function(data, intervals) {
  interval_values <- 1:(length(intervals) - 1)
  data$time <- sapply(data$time, function(time_value) {
    for (i in 1:(length(intervals) - 1)) {
      if (time_value >= intervals[i] && time_value < intervals[i + 1]) {
        return(interval_values[i])
      }
    }
    return(interval_values[length(interval_values)])
  })
  return(data)
}

# Read the data for the specified site number and sort it by 'time'
data1 <- read.csv(paste0("Data_site_", siteNb, ".csv"))
data1 <- data1[order(data1$time), ]
array1 <- data1$time

# First step: choose cutoff
if (!file.exists("Global_cutoff.csv")) {
  
  # First check: make sure values are compatible
  limits <- data.frame(start = array1[6], end = array1[length(array1) - 5])
  write.csv(limits, paste0("start_end_values_site_", siteNb, ".csv"), row.names = FALSE)
  
  # Exclude the last values based on the percentage to be excluded
  num_rows <- nrow(data1)
  exclude_index <- floor(num_rows * (1 - percent_excluded / 100))
  
  # Filter the data to exclude the last rows
  data1_filtered <- data1[1:exclude_index, ]
  array1_filtered <- data1_filtered$time
  
  # Write the cutoff value to a CSV file
  write.csv(tail(array1_filtered, n = 1), file=paste0("Cutoff_site_", siteNb, ".csv"), row.names = FALSE)
  
  # Second step: choose interval size
} else if (!file.exists("Global_interval_size.csv")) { 
 
  # Read the global cutoff value
  cutoff_value <- as.integer(read.csv("Global_cutoff.csv"))
  position <- which.min(abs(array1 - cutoff_value))
  
  data1 <- data1[1:position,]
  array1 <- data1$time[data1$status == 1]
  
  # Calculate the interval size based on the maximum difference between points
  differences1 <- abs(array1[1:(length(array1) - nbDataGrouped)] - array1[(nbDataGrouped + 1):length(array1)])
  
  # Biggest difference is either the largest gap between values or gap between lower_bound and the first value
  max_difference1 <- max(differences1, (array1[1+nbDataGrouped]-as.integer(lower_bound)))
  
  # Write the interval size to a CSV file
  write.csv(max_difference1, file=paste0("Interval_size_site_", siteNb, ".csv"), row.names = FALSE)
  
  # Last step: split into intervals
} else { 
  
  # Read the global cutoff and interval size values
  cutoff_value <- as.integer(read.csv("Global_cutoff.csv"))
  position <- which.min(abs(array1 - cutoff_value))
  array1 <- array1[1:position]
  
  interval_size <- as.integer(read.csv("Global_interval_size.csv"))
  
  # Split the time data into intervals based on the global interval size
  intervals <- seq(from = lower_bound, to = cutoff_value, by = interval_size)
  data1 <- modify_time(data1, intervals)
  
  # Write the modified data with intervals to a CSV file
  write.csv(data1, file=paste0("Grouped_Data_site_", siteNb, ".csv"), row.names = FALSE)
}

## Remove all environment variables. 
## If you want to see the variable that were created, simply don't execute that line (and clear them manually after)
rm(list = ls())
