library(cdcForecastUtils)
library(dplyr)

# Verify national/Regional-level forecasts
natreg_files <- list.files(path="./nation-region-forecast-data", pattern="*.csv", full.names=TRUE, recursive=TRUE)

for(i in natreg_files){
    text <- paste("Testing", i, "...")
    print(text)
    verify_entry_file(i)
}

# Verify state forecasts
state_files <- list.files(path="./state-forecast-data", pattern="*.csv", full.names=TRUE, recursive=TRUE)

for(i in state_files){
    text <- paste("Testing", i, "...")
    print(text)
    verify_entry_file(i, challenge = "state_ili")
}

