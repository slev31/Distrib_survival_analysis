###############  DATA AGGREGATION ####################
############### UNIFORM INTERVALS ####################

## License: https://creativecommons.org/licenses/by-nc-sa/4.0/
## Copyright: GRIIS / Université de Sherbrooke

# Includes
library("survival")
library("survminer")
library("dplyr")

siteNb <- 1           # Input here the site number
nbDataGrouped <- 5    # Input here the number of subjects grouped together

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

data1 <- data1[order(data1$time), ]
array1 <- data1$time
differences1 <- abs(array1[1:(length(array1)-nbDataGrouped)] - array1[(nbDataGrouped+1):length(array1)])
max_difference1 <- max(differences1)

print(paste0("Interval size must be at least ", max_difference1, " to ensure ", nbDataGrouped, " grouped data points."))
cat("----- RÉSUMÉ -----\n",
    "Minimum:       ", lower_bound, "\n",
    "Maximum:       ", upper_bound, "\n",
    "Interval size: ", max_difference1, "\n")


## Remove all environment variables. 
## If you want to see the variable that were created, simply don't execute that line (and clear them manually after)
rm(list = ls())
