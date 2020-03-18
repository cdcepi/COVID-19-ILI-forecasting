library(cdcForecastUtils)
library(dplyr)

# For national/Regional-level forecasts
files <- list.files(path="./nation-region-forecast-data", pattern="*.csv", full.names=TRUE, recursive=TRUE)

for(i in files){
    text <- paste("Testing", i, "...")
    print(text)
    verify_entry_file(i)
}


# For state-level forecasts
# weekly_forecast_filepath2 <- "/directory/EW10-2020-state_forecast_file.csv"
# verify_entry_file(weekly_forecast_filepath2, challenge = "state_ili")


# For national/Regional-level forecasts
# weekly_forecast_file1 <- read_entry( "./nation-region-forecast-data/UMassCoE-arimaT/metadata-UMassCoE-arimaT.txt")
# verify_entry(weekly_forecast_file1)
# For state-level forecasts
# weekly_forecast_file2 <- read_entry("/directory/EW10-2020-state_forecast_file.csv")
# verify_entry(weekly_forecast_file2)