## script to generate COVID-19 ILI forecasting template
## Nicholas Reich 
## Edited by Nutcha Wattanachit
## March 2020

library(dplyr)
library(readr)
library(tigris)
library(MMWRweek)

## load and save fips_codes for states and counties
tigris::fips_codes
write_csv(fips_codes, "templates-and-data/fips_codes.csv")

## shared parameters/data across state/national templates
hosp_targets <- c(paste(1:6, "wk ahead"), "Peak height")
date_targets <- c("Peak week")
hosp_bins <- as.character(c(
    seq(0, 500, by=50),
    seq(600, 10000, by=100),
    seq(11000, 40000, by=1000)
    ))
date_bins_df <- MMWRweek(seq.Date(as.Date("2020-03-01"), as.Date("2020-08-29"), by="1 week"))
date_bins <- paste0(date_bins_df$MMWRyear, "-ew", date_bins_df$MMWRweek)

### example locations
locations <- c("US", "25", "25011")

## make hosp targets template
hosp_bin_template <- expand.grid(location=locations, target=hosp_targets, type="bin", bin=hosp_bins, value = as.character(1/length(hosp_bins)), stringsAsFactors = FALSE)
hosp_points_template <- expand.grid(location=locations, target=hosp_targets, type="point", value="3000", stringsAsFactors = FALSE)

## make date targets template
date_bins_template <- expand.grid(location=locations, target=date_targets, type="bin", bin=date_bins, value = as.character(1/length(date_bins)), stringsAsFactors = FALSE)
date_points_template <- expand.grid(location=locations, target=date_targets, type="point", value = "2020-ew25", stringsAsFactors = FALSE)


## bind all together
hosp_template <- bind_rows(
    hosp_bin_template, 
    hosp_points_template, 
    date_bins_template, 
    date_points_template
) %>%
    mutate(location = factor(location, levels=locations),
           type = factor(type, levels=c("point", "bin"))) %>%
    arrange(location, target, type)

## sanity checking
hosp_template %>% group_by(target, type) %>% summarize(n()) %>% print(n=Inf)
hosp_template %>% filter(type=="bin") %>% group_by(location,target) %>% summarize(sum(as.numeric(value))) %>% print(n=Inf)

write_csv(hosp_template, path="templates-and-data/covid19-hospitalization-forecast-template.csv")

#### Create table with important dates

## due dates: Thursday
forecasts_due <- seq(as.Date("2020-04-09"), as.Date("2020-08-29"), by="1 week")
forecasts_due_ew <- paste0(
    MMWRweek(forecasts_due)$MMWRyear, 
    "-ew",
    MMWRweek(forecasts_due)$MMWRweek
)

## 1-week ahead
forecasts_1_wk_ahead <- paste0(
    MMWRweek(forecasts_due+3)$MMWRyear, 
    "-ew",
    MMWRweek(forecasts_due+3)$MMWRweek
)
forecast_1_wk_ahead_start <- forecasts_due+3

forecast_info <- tibble(
    forecasts_due, forecasts_due_ew, forecasts_1_wk_ahead, forecast_1_wk_ahead_start
)


write_csv(forecast_info, path="templates-and-data/covid-19-hospitalization-forecast-dates.csv")
