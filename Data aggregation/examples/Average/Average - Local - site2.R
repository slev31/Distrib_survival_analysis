###############  DATA AGGREGATION #####################
############### AVERAGED INTERVALS ####################

## License: https://creativecommons.org/licenses/by-nc-sa/4.0/
## Copyright: GRIIS / Université de Sherbrooke

# Includes
library("survival")
library("survminer")
library("dplyr")

siteNb <- 2           # Input here the site number
nbDataGrouped <- 5    # Input here the number of subjects grouped together

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
data1 <- data1 %>%
  mutate(group = (row_number() - 1) %/% nbDataGrouped + 1)

group_counts <- data1 %>%
  group_by(group) %>%
  summarize(count = n()) %>%
  ungroup()

# Merge the last small group with the previous one if needed
last_group <- max(data1$group)
if (group_counts$count[last_group] < nbDataGrouped) {
  data1 <- data1 %>%
    mutate(group = if_else(group == last_group, last_group - 1, group))
}

data1 <- data1 %>%
  group_by(group) %>%
  mutate(time = mean(time)) %>%
  ungroup() %>%
  select(-group)

write.csv(data1, file=paste0("Grouped_Data_site_", siteNb, ".csv"), row.names = FALSE)

## Remove all environment variables. 
## If you want to see the variable that were created, simply don't execute that line (and clear them manually after)
rm(list = ls())