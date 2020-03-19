## script to generate COVID-19 ILI forecasting template
## Nicholas Reich 
## Edited by Nutcha Wattanachit
## March 2020

library(dplyr)
library(readr)
library(MMWRweek)

## shared parameters/data across state/national templates
ili_targets <- c(paste(1:6, "wk ahead"), "Peak height")
date_targets <- c("Peak week", "First week below baseline")
binary_targets <- c("Below baseline for 3 weeks")
ili_bins <- as.character(sprintf("%.1f", seq(0, 25, by=.1)))
date_bins_df <- MMWRweek(seq.Date(as.Date("2020-03-01"), as.Date("2020-08-29"), by="1 week"))
date_bins <- paste0(date_bins_df$MMWRyear, "-ew", date_bins_df$MMWRweek)

### National/Regional template
locations <- c("US National", paste("HHS Region", 1:10))

## make ILI targets template
ili_bin_template <- expand.grid(location=locations, target=ili_targets, type="bin", bin=ili_bins, value = as.character(1/length(ili_bins)), stringsAsFactors = FALSE)
ili_points_template <- expand.grid(location=locations, target=ili_targets, type="point", value="1.234", stringsAsFactors = FALSE)

## make date targets template
date_bins_template <- expand.grid(location=locations, target=date_targets, type="bin", bin=date_bins, value = as.character(1/length(date_bins)), stringsAsFactors = FALSE)
date_points_template <- expand.grid(location=locations, target=date_targets, type="point", value = "2020-ew15", stringsAsFactors = FALSE)

## make binart target template
binary_bins_template <- expand.grid(location=locations, target=binary_targets, type="bin", bin="true", value = ".5", stringsAsFactors = FALSE)

## bind all together
natreg_template <- bind_rows(
    ili_bin_template, 
    ili_points_template, 
    date_bins_template, 
    date_points_template, 
    binary_bins_template
) %>%
    mutate(location = factor(location, levels=locations),
           type = factor(type, levels=c("point", "bin"))) %>%
    arrange(location, target, type, bin)

## sanity checking
natreg_template %>% group_by(target, type) %>% summarize(n()) %>% print(n=Inf)
natreg_template %>% filter(type=="bin") %>% group_by(location,target) %>% summarize(sum(as.numeric(value))) %>% print(n=Inf)

write_csv(natreg_template, path="templates-and-data/covid19-ili-forecast-national-regional-template.csv")

### State template
## note, this excludes Florida and includes Virgin Islands, Puerto Rico, and District of Columbia
states <- read_csv("https://raw.githubusercontent.com/cdcepi/State_FluSight_Forecasts/master/2017-2018_StateILI_Submission_Template.csv") %>%
    .$Location %>% unique() %>% append("New York City", after=32)
## make ILI targets template
ili_bin_template <- expand.grid(location=states, target=ili_targets, type="bin", bin=ili_bins, value = as.character(1/length(ili_bins)), stringsAsFactors = FALSE)
ili_points_template <- expand.grid(location=states, target=ili_targets, type="point", value="1.234", stringsAsFactors = FALSE)

## make date targets template
# date_bins_template <- expand.grid(location=states, target=date_targets, type="bin", bin=date_bins, value = as.character(1/length(date_bins)), stringsAsFactors = FALSE)
# date_points_template <- expand.grid(location=states, target=date_targets, type="point", value = "2020-03-02", stringsAsFactors = FALSE)
date_bins_template <- expand.grid(location=states, target=date_targets[1], type="bin", bin=date_bins, value = as.character(1/length(date_bins)), stringsAsFactors = FALSE)
date_points_template <- expand.grid(location=states, target=date_targets[1], type="point", value = "2020-ew15", stringsAsFactors = FALSE)

## make binart target template
# binary_bins_template <- expand.grid(location=states, target=binary_targets, type="bin", bin="true", value = ".5", stringsAsFactors = FALSE)

## bind all together
state_template <- bind_rows(
    ili_bin_template, 
    ili_points_template, 
    date_bins_template, 
    date_points_template
    # binary_bins_template
) %>%
    mutate(location = factor(location, levels=states),
           type = factor(type, levels=c("point", "bin"))) %>%
    arrange(location, target, type, bin)

## sanity checking
state_template %>% group_by(target, type) %>% summarize(n()) %>% print(n=Inf)
state_template %>% filter(type=="bin") %>% group_by(location,target) %>% summarize(sum(as.numeric(value))) %>% print(n=Inf)

write_csv(state_template, path="templates-and-data/covid19-ili-forecast-state-template.csv")


#### Create table with important dates

## assuming all Friday release-dates
ilinet_release_dates <- seq(as.Date("2020-03-13"), as.Date("2020-08-29"), by="1 week")
ilinet_data_thru <- ilinet_release_dates-6
ilinet_data_thru_ew <- paste0(
    MMWRweek(ilinet_data_thru)$MMWRyear, 
    "-ew",
    MMWRweek(ilinet_data_thru)$MMWRweek
)

forecasts_due <- ilinet_release_dates+3
forecasts_due_ew <- paste0(
    MMWRweek(forecasts_due)$MMWRyear, 
    "-ew",
    MMWRweek(forecasts_due)$MMWRweek
)
forecasts_1_wk_ahead <- paste0(
    MMWRweek(ilinet_data_thru+1)$MMWRyear, 
    "-ew",
    MMWRweek(ilinet_data_thru+1)$MMWRweek
)

forecast_info <- tibble(
    ilinet_release_dates, ilinet_data_thru, ilinet_data_thru_ew,
    forecasts_due, forecasts_due_ew, forecasts_1_wk_ahead
)

write_csv(forecast_info, path="templates-and-data/covid-19-forecast-dates.csv")


