###############  DATA AGGREGATION ####################
############### UNIFORM INTERVALS ####################

## License: https://creativecommons.org/licenses/by-nc-sa/4.0/
## Copyright: GRIIS / Université de Sherbrooke

# Includes
library("survival")
library("survminer")
library("dplyr")

siteNb <- ...             # Input here the site number
nbDataGrouped <- ...      # Input here the number of subjects grouped together
percent_excluded <- ...   # Input here the percent of values you want to exclude

lower_bound <- ...        # Input here the lower bound, or lowest value (leave a 0 if it is unknown)
upper_bound <- ...        # Input here the upper bound, or highest value

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

# Function to modify time (used to group data into intervals)
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

# Read data
data1 <- read.csv(paste0("Data_site_", siteNb, ".csv"))
data1 <- data1[order(data1$time), ]
array1 <- data1$time

# First step: choose cutoff
if (!file.exists("Global_cutoff.csv")) {

  # Exclude the last values
  num_rows <- nrow(data1)
  exclude_index <- floor(num_rows * (1-percent_excluded/100))

  data1_filtered <- data1[1:exclude_index, ]
  array1_filtered <- data1_filtered$time
  
  write.csv(tail(array1_filtered, n = 1), file=paste0("Cutoff_site_", siteNb, ".csv"), row.names = FALSE)
  
} else if (!file.exists("Global_interval_size.csv")){ # Second step: choose interval size
  
  cutoff_value <- as.integer(read.csv("Global_cutoff.csv"))
  position <- which.min(abs(array1 - cutoff_value))
  array1 <- array1[1:position]
  
  # Calculate interval size
  differences1 <- abs(array1[1:(length(array1)-nbDataGrouped)] - array1[(nbDataGrouped+1):length(array1)])
  max_difference1 <- max(differences1)

  write.csv(max_difference1, file=paste0("Interval_size_site_", siteNb, ".csv"), row.names = FALSE)
  
} else { # Last step: split into intervals
  
  cutoff_value <- as.integer(read.csv("Global_cutoff.csv"))
  position <- which.min(abs(array1 - cutoff_value))
  array1 <- array1[1:position]
  
  interval_size <- as.integer(read.csv("Global_interval_size.csv"))
  
  # Split into intervals
  intervals <- seq(from = lower_bound, to = cutoff_value, by = interval_size)
  data1 <- modify_time(data1, intervals)
  
  write.csv(data1, file=paste0("Grouped_Data_site_", siteNb, ".csv"), row.names = FALSE)
}

## Remove all environment variables. 
## If you want to see the variable that were created, simply don't execute that line (and clear them manually after)
rm(list = ls())
