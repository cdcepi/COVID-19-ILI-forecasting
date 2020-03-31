# Data Step 2 - Read COVID-19 Cases

################################################################################
# Packages

require(reshape2)
require(MMWRweek)

################################################################################
# Constants

today_date <- as.Date("2020-03-28") # Sys.Date()
output_file <- paste0("COVID19_data_", today_date, ".RData")

################################################################################
# Functions

read_covid_data <- function(filenam, value.name)
{
  covid_url_base <- "https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  X <- read.csv(paste0(covid_url_base, filenam))
  X <- reshape2::melt(X, id.vars = 1:4, variable.name = "xdate", 
                      value.name = value.name)
  X$xdate <- as.character(X$xdate)
  X$date <- gsub("[.]", "-", gsub("X", "", X$xdate))
  X$date <- as.Date(strptime(X$date, format = "%m-%d-%y"))
  
  temp_mmwr <- MMWRweek::MMWRweek(X$date)
  X$mmwrweek <- temp_mmwr$MMWRweek
  X$mmwryear <- temp_mmwr$MMWRyear
  X$mmwrday <- temp_mmwr$MMWRday
  
  return(X)
}

read_covid_daily <- function(filenams, dates)
{
  # filenams <- paste0(strftime(as.Date("2020-01-22")+0:65, format = "%m-%d-%Y"), ".csv")
  # filenams <- "03-21-2020.csv"
  assertthat::assert_that(length(filenams) == length(dates))
  
  # read from github
  covid_url_base <- "https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
  X <- lapply(filenams, function(z) read.csv(paste0(covid_url_base, z), 
                                             stringsAsFactors = FALSE))
  # add the date
  for (i in 1:length(X))
  {
    X[[i]]$date <- dates[i]
  }
  # adjust the datasets
  Y <- lapply(X, function(z) {
    if (grepl("Province[.]State", names(z)[1]) & length(names(z)) == 7)
    {
      names(z) <- c("Province_State", "Country_Region", "Last_Update", "Confirmed",
                    "Deaths", "Recovered", "date")
    } else if (grepl("Province[.]State", names(z)[1]) & length(names(z)) == 9)
    {
      names(z) <- c("Province_State", "Country_Region", "Last_Update", "Confirmed",
                    "Deaths", "Recovered", "Lat", "Long_", "date")
    } else if (grepl("FIPS", names(z)[1])) 
    {
      names(z) <- c("FIPS", "Admin2", "Province_State", "Country_Region",
                    "Last_Update", "Lat", "Long_", "Confirmed", "Deaths",
                    "Recovered", "Active", "Combined_Key", "date")
    } else
    {
      print(head(z))
      stop("Unexpected names")
    }
    # Last_Update doesn't always conform to the date
    test1 <- strptime(z$Last_Update[1:3], "%m/%d/%y %H:%M")
    test2 <- strptime(z$Last_Update[1:3], "%m/%d/%Y %H:%M")
    test3 <- strptime(z$Last_Update[1:3], "%Y-%m-%dT%H:%M:%S")
    test4 <- strptime(z$Last_Update[1:3], "%Y-%m-%d %H:%M:%S")
    if (!any(is.na(test1)))
    {
      z$Last_Update <- strptime(z$Last_Update, "%m/%d/%y %H:%M")
    } else if (!any(is.na(test2)))
    {
      z$Last_Update <- strptime(z$Last_Update, "%m/%d/%Y %H:%M")
    } else if (!any(is.na(test3)))
    {
      z$Last_Update <- strptime(z$Last_Update, "%Y-%m-%dT%H:%M:%S")
    } else if (!any(is.na(test4)))
    {
      z$Last_Update <- strptime(z$Last_Update, "%Y-%m-%d %H:%M:%S")
    } else
    {
      print(head(z))
      stop("Unexpected date format")
    }
    
    #z$date <- as.Date(z$Last_Update)
    temp_mmwr <- MMWRweek::MMWRweek(z$date)

    z$mmwrweek <- temp_mmwr$MMWRweek
    z$mmwryear <- temp_mmwr$MMWRyear
    z$mmwrday <- temp_mmwr$MMWRday
    
    if (ncol(z) == 10) # 6 original + 4 new variables
    {
      z$FIPS <- as.integer(NA)
      z$Admin2 <- as.character(NA)
      z$Lat <- as.numeric(NA)
      z$Long_ <- as.numeric(NA)
      z$Active <- as.integer(NA)
      z$Combined_Key <- as.character(NA)
    } else if (ncol(z) == 12)
    {
      z$FIPS <- as.integer(NA)
      z$Admin2 <- as.character(NA)
      z$Active <- as.integer(NA)
      z$Combined_Key <- as.character(NA)
    }

    return(z)
  })

  return(Y)
}

state_abb_2_name <- function(abb)
{
  assertthat::assert_that(all(abb %in% state.abb), msg = "abb not in state.abb")
  sapply(abb, function(z) state.name[which(state.abb == z)], USE.NAMES = FALSE)
}
assertthat::assert_that(state_abb_2_name("OH") == "Ohio")

################################################################################
# Collect

covid_data <- read_covid_data("time_series_covid19_confirmed_global.csv", "confirmed")
covid_deaths <- read_covid_data("time_series_covid19_deaths_global.csv", "deaths")
covid_recovered <- read_covid_data("time_series_covid19_recovered_global.csv", "recovered")

covered_dates <- seq(as.Date("2020-01-22"), today_date, by = 1)
filenams <- paste0(strftime(covered_dates, format = "%m-%d-%Y"), ".csv")

covid_daily <- read_covid_daily(filenams, covered_dates)
covid_daily <- do.call("rbind", covid_daily)

# get the city, STATE entries
ind <- which(grepl("[,][ ][A-Z][A-Z]", covid_daily$Province_State) &
               covid_daily$Country_Region == "US")
temp <- covid_daily$Province_State[ind]
# drop the (From Diamond Princess)
temp <- gsub("[ ][(]From Diamond Princess[)]", "", temp)
# get the state abbreviation
temp <- sapply(temp, function(z) strsplit(z, "[,][ ]")[[1]][2], USE.NAMES = FALSE)
# create a New York City identifier
ind_nyc <- which(grepl("New York City", covid_daily$Province_State) |
                   grepl("New York County", covid_daily$Province_State) |
                   grepl("Kings County", covid_daily$Province_State) |
                   grepl("Queens County", covid_daily$Province_State) |
                   grepl("Bronx County", covid_daily$Province_State) |
                   grepl("Richmond County", covid_daily$Province_State) |
                   grepl("New York City", covid_daily$Admin2) |
                   (grepl("New York", covid_daily$Province_State) & 
                      covid_daily$date < as.Date("2020-03-22") & 
                      covid_daily$date > as.Date("2020-03-09")))
# move the city, STATE to Admin2
covid_daily$Admin2[ind] <- covid_daily$Province_State[ind]
# replace Province_State with the state
covid_daily$Province_State[ind] <- state_abb_2_name(trimws(temp))
# make the Combined_Key into the NYC identifier
covid_daily$Combined_Key[ind_nyc] <- "New York City, New York, US"

save(covid_data, covid_deaths, covid_recovered, covid_daily,
     file = output_file)
