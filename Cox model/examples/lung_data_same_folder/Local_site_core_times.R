############### DISTRIBUTED COX MODEL ####################
############### Local site code ###########################

## License: https://creativecommons.org/licenses/by-nc-sa/4.0/
## Copyright: GRIIS / Université de Sherbrooke

# Loading packages and setting up core variables --------------------------
library("survival")
library("survminer")

# First function --- Calculate different event times
data_event_times <- function(man_wd=-1,nodeid=-1) {
  
  manualwd <- man_wd
  k <- nodeid
  
  if (k<0){
    stop
  }
  
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
  
  # Read data, get event times, write in csv
  node_data <- read.csv(paste0("Data_site_", k, ".csv"))
  event_times <- unique(node_data$time[node_data$status == 1])
  write.csv(event_times, file=paste0("Times_",k,"_output.csv"),row.names = FALSE,na="")

  rm(list = ls())
  
  return(TRUE)
}
