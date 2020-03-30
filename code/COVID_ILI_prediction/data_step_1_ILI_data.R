# Data Step 1 - Obtain the recent ILI data

################################################################################
# Packages

# Install from GitHub
if (FALSE)
{
  require(devtools)
  devtools::install_github("hrbrmstr/cdcfluview")
}

require(cdcfluview)
require(reshape2)
require(assertthat)
require(magrittr)

################################################################################
# Output file

today_date <- as.Date("2020-03-28") # Sys.Date()
output_file <- paste0("ILI_data_", today_date, ".RData")

################################################################################
# Read Data

# National ILINet data for 1997/98 - 2019/20 seasons
usflu <- cdcfluview::ilinet(region = "national", years = 1997:2020)

# HHS Regional ILINet data for 1997/98 - 2019/20 seasons
regionflu <- cdcfluview::ilinet(region = "HHS", years = 1997:2020)

# States
stateflu <- cdcfluview::ilinet(region = "state", years = 1997:2020)

################################################################################
# HHS Regions

# Note:  HHS regions sum almost exactly to national, but states do not sum
#   to national because of florida
#   all states in a region sum to a region, except for florida

hhs_regions_map <- reshape2::melt(list(
  boston1 = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", 
              "Rhode Island", "Vermont"),
  newyork2 = c("New Jersey", "New York", "New York City", "Puerto Rico", 
               "Virgin Islands"),
  philadelphia3 = c("Delaware", "District of Columbia", "Maryland", 
                    "Pennsylvania", "Virginia", "West Virginia"),
  atlanta4 = c("Alabama", "Florida", "Georgia", "Kentucky", "Mississippi", 
               "North Carolina", "South Carolina", "Tennessee"),
  chicago5 = c("Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", 
               "Wisconsin"),
  dallas6 = c("Arkansas", "Louisiana", "New Mexico", "Oklahoma", "Texas"),
  kansascity7 = c("Iowa", "Kansas", "Missouri", "Nebraska"),
  denver8 = c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", 
              "Wyoming"),
  sanfrancisco9 = c("Arizona", "California", "Hawaii", "Nevada",
                    "Commonwealth of the Northern Mariana Islands"),
  seattle10 = c("Alaska", "Idaho", "Oregon", "Washington")
))
names(hhs_regions_map) <- c("state", "region")
hhs_regions_map$region_num <- as.integer(gsub("[a-z]*","", hhs_regions_map$region))
hhs_regions_map$region <- gsub("[0-9]*", "", hhs_regions_map$region)

assertthat::assert_that(all(stateflu$region %in% hhs_regions_map$state))

stateflu$region_num <- hhs_regions_map$region_num[match(stateflu$region, hhs_regions_map$state)]

################################################################################
# Florida data is not included in the state data

# check that all florida data are NA
assertthat::assert_that(all(is.na(subset(stateflu, region == "Florida", 
                                         select = c("ilitotal")))))

tempregion <- regionflu %>% 
  subset(week_start >= as.Date("2010-10-03") & region == "Region 4")

tempstate <- stateflu %>% 
  subset(region_num == 4) %>% 
  dplyr::group_by(year, week) %>%
  dplyr::summarise(ilitotal = sum(ilitotal, na.rm = TRUE),
                   num_of_providers = sum(num_of_providers, na.rm = TRUE),
                   total_patients = sum(total_patients, na.rm = TRUE),
                   age_0_4 = sum(age_0_4, na.rm = TRUE),
                   age_25_49 = sum(age_25_49, na.rm = TRUE),
                   age_25_64 = sum(age_25_64, na.rm = TRUE),
                   age_5_24 = sum(age_5_24, na.rm = TRUE),
                   age_50_64 = sum(age_50_64, na.rm = TRUE),
                   age_65 = sum(age_65, na.rm = TRUE))

assertthat::assert_that(all(tempregion$year == tempstate$year))
assertthat::assert_that(all(tempregion$week == tempstate$week))

temp <- data.frame(region_type = "States",
                   region = "Florida",
                   year = tempregion$year,
                   week = tempregion$week,
                   weighted_ili = NA,
                   unweighted_ili = NA,
                   age_0_4 = tempregion$age_0_4 - tempstate$age_0_4,
                   age_25_49 = tempregion$age_25_49 - tempstate$age_0_4,
                   age_25_64 = tempregion$age_25_64 - tempstate$age_25_64,
                   age_5_24 = tempregion$age_5_24 - tempstate$age_5_24,
                   age_50_64 = tempregion$age_50_64 - tempstate$age_50_64,
                   age_65 = tempregion$age_65 - tempstate$age_65,
                   ilitotal = tempregion$ilitotal - tempstate$ilitotal,
                   num_of_providers = tempregion$num_of_providers - tempstate$num_of_providers,
                   total_patients = tempregion$total_patients - tempstate$total_patients,
                   week_start = tempregion$week_start,
                   region_num = 4)
temp$unweighted_ili <- temp$ilitotal / temp$total_patients * 100

assertthat::assert_that(all(temp$ilitotal >= 0))

# Add florida to the state data
stateflu2 <- stateflu
ind <- which(stateflu2$region == "Florida")
assertthat::assert_that(length(ind) == nrow(temp))

stateflu2$unweighted_ili[ind] <- temp$unweighted_ili
stateflu2$age_0_4[ind] <- temp$age_0_4
stateflu2$age_25_49[ind] <- temp$age_25_49
stateflu2$age_25_64[ind] <- temp$age_25_64
stateflu2$age_5_24[ind] <- temp$age_5_24
stateflu2$age_50_64[ind] <- temp$age_50_64
stateflu2$age_65[ind] <- temp$age_65
stateflu2$ilitotal[ind] <- temp$ilitotal
stateflu2$num_of_providers[ind] <- temp$num_of_providers
stateflu2$total_patients[ind] <- temp$total_patients

stateflu <- stateflu2

save(usflu, stateflu, regionflu, hhs_regions_map,
     file = output_file)
