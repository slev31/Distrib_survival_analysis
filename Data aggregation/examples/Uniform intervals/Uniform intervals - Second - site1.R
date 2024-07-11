###############  DATA AGGREGATION ####################
############### UNIFORM INTERVALS ####################

## License: https://creativecommons.org/licenses/by-nc-sa/4.0/
## Copyright: GRIIS / Université de Sherbrooke

# Includes
library("survival")
library("survminer")
library("dplyr")

siteNb <- 1           # Input here the site number
step <- 1             # Input here the interval size (should be the highest value obtained with "Uniform intervals - First.csv")

lower_bound <- 0      # Input here the lower bound, or lowest value (leave a 0 if it is unknown)
upper_bound <- 100    # Input here the upper bound, or highest value

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

data1 <- read.csv(paste0("Data_site_", siteNb, ".csv"))

intervals <- seq(from = lower_bound, to = upper_bound, by = step)

interval_averages <- sapply(1:(length(intervals) - 1), function(i) {
  mean(c(intervals[i], intervals[i + 1]))
})

modify_time <- function(data, intervals) {
  interval_averages <- sapply(1:(length(intervals) - 1), function(i) {
    mean(c(intervals[i], intervals[i + 1]))
  })
  data$time <- sapply(data$time, function(time_value) {
    for (i in 1:(length(intervals) - 1)) {
      if (time_value >= intervals[i] && time_value < intervals[i + 1]) {
        return(interval_averages[i])
      }
    }
    return(interval_averages[length(interval_averages)])
  })
  return(data)
}

data1 <- modify_time(data1, intervals)

write.csv(data1, file=paste0("Grouped_Data_site_", siteNb, ".csv"), row.names = FALSE)

## Remove all environment variables. 
## If you want to see the variable that were created, simply don't execute that line (and clear them manually after)
rm(list = ls())
