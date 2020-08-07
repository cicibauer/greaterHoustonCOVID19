#This script contains a set of functions to normalize Safegraph data for you.
#To add these functions, simply write:
#source(<address>/safegraph_normalization_functions.R)
#use the normBG(<patterns>,<home panel summary>) to get visit_counts column in your patterns data.

library(readr)
library(dplyr)
library(tidyverse)
library(tigris)
library(censusapi)
library(sf)
library(usmap)
library(mapview)
library(rjson)

options(
  tigris_class = "sf",
  tigris_use_cache = T # This stores tigris loads somewhere on your machine for much faster personal loading.
)

Sys.setenv(CENSUS_KEY="5e5c985bc80f5902a674b81c64ce28a377b4bea8")

# Importing the safegraph weekly patterns and home panel datasets
X2020_06_08_weekly_patterns_csv <- read_csv("C:\\Users\\roone\\Documents\\demoShiny\\safegraph\\2020-06-08-weekly-patterns.csv.gz")
X2020_06_08_home_panel_summary <- read_csv("C:\\Users\\roone\\Documents\\demoShiny\\safegraph\\2020-06-08-home-panel-summary.csv")
X2020_06_01_weekly_patterns_csv <- read_csv("C:\\Users\\roone\\Documents\\demoShiny\\safegraph\\2020-06-01-weekly-patterns.csv.gz")
X2020_06_01_home_panel_summary <- read_csv("C:\\Users\\roone\\Documents\\demoShiny\\safegraph\\2020-06-01-home-panel-summary.csv")


## Basic census data
#The first variable acs_year, may need to be updated as necessary.

#All data currently using acs_year 2018.
acs_year = 2018

#Names of Greater Houston Area counties
norm_houston_county_names <-
  c(
    "Harris",
    "Fort Bend",
    "Montgomery",
    "Brazoria",
    "Galveston",
    "Liberty",
    "Waller",
    "Chambers",
    "Austin"
  )

#FIPS code of Greater Houston Area counties
norm_Houston_counties <- unlist(lapply(norm_houston_county_names, function(x) {fips('TX',paste(x,'County'))}))

#Calculating Population of Greater Houston Area
greater_houston_pop <-
  getCensus(
    name = "acs/acs1", 
    vintage = acs_year,  
    region = "county:*", # Geography 
    regionin = "state:48", # State 48 is Texas
    vars = "B01003_001E" # Variables to obtain from census
  ) %>% 
  mutate(fips = paste0(state,county)) %>% 
  filter(fips %in% norm_Houston_counties) %>% 
  pull(B01003_001E) %>% 
  sum()

#Houston Area Blockgroups FIPS codes.
norm_houston_blockgroups <-
  norm_houston_county_names %>%
  map(function(x){
    block_groups("TX",x,progress_bar=T) %>%
      pull(GEOID)
  }) %>% unlist()

#Texas counties FIPS codes.
norm_tx_counties_fips <-
  counties("TX", cb = F, progress_bar=T) %>%
  pull(COUNTYFP)


#Texas population by blockgroup.
norm_tx_pop_blockgroup <-
  norm_tx_counties_fips %>%
  map_dfr(function(x){
    getCensus(
      name = "acs/acs5",
      vintage = acs_year,
      region = "block group:*",
      regionin = paste0("state:48+county:",x),
      vars = "B01003_001E"
    )
  }) %>%
  transmute(
    origin_census_block_group = paste0(
      state,county,tract,block_group
    ),
    pop = B01003_001E
  )


##Functions

#Function to do houston normalization.
#param patterns: Safegraph patterns dataset.
#pre patterns must have the columns: 'raw_visits_counts'.
#param home_summary: Safegraph home_panel_summary dataset.
#pre home_summary must have columns 'census_block_group' and 'number_devices_residing'.
#param houston_blockgroups: list of blockgroup GEOIDs in the houston area for filtering of the Safegraph datasets.
#param houston_pop: numeric. Population of houston Area.
#returns patterns dataset with column visit_counts that multiples raw visits based on ratio of houston area population to safegraph population.
normhouston <- function(patterns, home_summary, houston_blockgroups = norm_houston_blockgroups, houston_pop = greater_houston_pop)
{
  #Compute safegraph population from home_summary by takeing houston blockgroups and adding number of devices.
  houston_sg_pop <-
    home_summary %>%
    filter(census_block_group %in% houston_blockgroups) %>%
    pull(number_devices_residing) %>%
    sum()

  #Create houston Area-wide multiplier.
  houston_multiplier <- houston_pop/houston_sg_pop
  houston_multiplier

  return(patterns %>%  mutate(visit_counts = raw_visit_counts*houston_multiplier))
}

summary(X2020_06_08_weekly_patterns_csv)
X2020_06_08_weekly_patterns_csv <- filter(X2020_06_08_weekly_patterns_csv, region == "TX")
X2020_06_01_weekly_patterns_csv <- filter(X2020_06_01_weekly_patterns_csv, region == "TX")
general_pop_normalization1 <- normhouston(X2020_06_08_weekly_patterns_csv, X2020_06_08_home_panel_summary, norm_houston_blockgroups, greater_houston_pop)
general_pop_normalization2 <- normhouston(X2020_06_01_weekly_patterns_csv, X2020_06_01_home_panel_summary, norm_houston_blockgroups, greater_houston_pop)
general_pop_normalization <- rbind(general_pop_normalization1, general_pop_normalization2)
head(general_pop_normalization %>% dplyr::select(location_name, city, raw_visit_counts, visit_counts, region))



# #The following chunk of code provides a more granular method of normalization on a census block level 
# #but takes a much longer time to execute
# 
# #Function to expand patterns dataset by blockgroup.
# #param patterns_data: Safegraph patterns dataset.
# #pre It should have a column visitor_home_cbgs with a JSON dictionary of blockgroup GEOIDs and number of visitors.
# #returns patterns_data with breakdown of visitors by blockgroup or unrecorded.
# #       This breakdown is given
# expandOrigins <- function(patterns_data)
# {
# 
# 
#   expanded_patterns <- NULL
# 
#   #Loop over each row so that we can convert each row to multiple rows, one for each origin census block group of visitors.
#   for(row in 1:nrow(patterns_data)){
# 
#     #If no recorded blockgroup, then just save as one row with no origin_census_block_group.
#     if(patterns_data$visitor_home_cbgs[row] == "{}") {
# 
#       data_row <-
#         data.frame(
#           safegraph_place_id = patterns_data$safegraph_place_id[row],
#           origin_census_block_group = NA, #origin block group is not available.
#           origin_raw_visitor_counts_high = patterns_data$raw_visitor_counts[row],
#           origin_raw_visitor_counts_low = patterns_data$raw_visitor_counts[row]
#         )
# 
#     }
#     # Else, need to unpack the JSON dictionary.
#     else {
#       #Unpack, unlist, rename.
#       json <-
#         patterns_data$visitor_home_cbgs[row] %>%
#         fromJSON() %>%
#         unlist() %>%
#         as.data.frame() %>%
#         rownames_to_column() %>%
#         rename(
#           origin_census_block_group = "rowname",
#           origin_raw_visitor_counts_high = "."
#         ) %>%
#         mutate( # Dealing with the 2-4 visitor issue
#           origin_raw_visitor_counts_low =
#             ifelse(
#               origin_raw_visitor_counts_high == 4,
#               2,
#               origin_raw_visitor_counts_high
#             )
#         )
# 
#       #Compute the unrecorded raw visitors by subtracting blockgroup-mapped origin_raw_visitor_counts from raw_visitor_counts. Special cases need to be considered because of the 2-4 visitor issue
#       if(patterns_data$raw_visitor_counts[row] > sum(json$origin_raw_visitor_counts_high)){
# 
#         unrecorded_raw_visitor_counts_high <-
#           patterns_data$raw_visitor_counts[row] - sum(json$origin_raw_visitor_counts_high)
# 
#         unrecorded_raw_visitor_counts_low <-
#           patterns_data$raw_visitor_counts[row] - sum(json$origin_raw_visitor_counts_low)
# 
#       } else {
# 
#         #In this rare case, there were enough origins overcounted as having 4 raw visitors that the total count for recorded origins exceeded that actual raw_visitor_count. We downscale all recorded origin visitor counts so that the sum of recorded origin visitor counts equals actual raw_visitor_count exactly.
#         json$origin_raw_visitor_counts_high <-
#           json$origin_raw_visitor_counts_high/sum(json$origin_raw_visitor_counts_high)*patterns_data$raw_visitor_counts[row]
# 
#         unrecorded_raw_visitor_counts_high <- 0
# 
#         unrecorded_raw_visitor_counts_low <-
#           max((patterns_data$raw_visitor_counts[row] - sum(json$origin_raw_visitor_counts_low)),0)
# 
#       }
# 
#       #Add one more row which contains unrecorded raw_visitor_counts.
#       data_row <-
#         json %>%
#         rbind(
#           data.frame(
#             origin_census_block_group = NA,
#             origin_raw_visitor_counts_high = unrecorded_raw_visitor_counts_high,
#             origin_raw_visitor_counts_low = unrecorded_raw_visitor_counts_low
#           )
#         ) %>%
#         mutate(
#           safegraph_place_id = patterns_data$safegraph_place_id[row]
#         )
# 
#     }
#     #Put all the rows together.
#     expanded_patterns <-
#       expanded_patterns %>%
#       rbind(data_row)
#   }
# 
#   #Join the rest of the patterns data using safegraph place id.
#   expanded_patterns <-
#     expanded_patterns %>% left_join(patterns_data, by = 'safegraph_place_id')
# 
#   return(expanded_patterns)
# }
# 
# 
# 
# #Function to do block-groupwise normalization.
# #param patterns: Safegraph patterns dataset.
# #pre patterns must have the columns: 'raw_visits_counts'.
# #param home_summary: Safegraph home_panel_summary dataset.
# #pre home_summary must have columns 'census_block_group' and 'number_devices_residing'.
# #param houston_blockgroups: list of blockgroup GEOIDs in the houston area for filtering of the Safegraph datasets.
# #param tx_pop_blockgroup: dataframe. Censis population count for each blockgroup in TX
# #returns patterns dataset with column visit_counts that multiples raw visits based on ratio of houston area population to safegraph population.
# normBG <- function(patterns,
#                    home_summary,
#                    houston_blockgroups = norm_houston_blockgroups,
#                    tx_pop_blockgroup = norm_tx_pop_blockgroup)
# {
# 
#   #Expand and categorize visitors by origin_census_block_group.
#   #Also join population and home summary data.
#   patterns <- expandOrigins(patterns) %>%
#     left_join(home_summary, by = c('origin_census_block_group' = 'census_block_group')) %>%
#     left_join(tx_pop_blockgroup, by = 'origin_census_block_group')
# 
#   #Potential cleaning of lack of population data if not present within California.
#   # lack_of_pop <- which(is.na(patterns$pop) & !is.na(patterns$origin_census_block_group))
#   # for (i in lack_of_pop) {
#   #   geoid = patterns[i,'origin_census_block_group']
#   #   state <- substring(geoid,1,2)
#   #   county <- substring(geoid,3,5)
#   #   tract <- substring(geoid,6,11)
#   #   bg <- substring(geoid,12,12)
#   #   #Get population data for non-California blockgroups.
#   #   patterns[i,'pop'] = getCensus(
#   #     name = "acs/acs5",
#   #     vintage = acs_year,
#   #     region = paste0("block group:", bg),
#   #     regionin = paste0("state:",state,"+county:",county,"+tract:",tract),
#   #     vars = "B01003_001E"
#   #   ) %>% pull('B01003_001E')
#   # }
# 
#   #To estimate number of visitors from unrecorded block groups, use the visitors-to-devices ratio generated by the aggregate recorded block groups.
#   recorded_pop <-
#     tx_pop_blockgroup %>%
#     filter(
#       origin_census_block_group %in% patterns$origin_census_block_group
#     ) %>%
#     pull(pop) %>%
#     sum()
# 
#   recorded_sg_pop <-
#     home_summary %>%
#     filter(census_block_group %in% patterns$origin_census_block_group) %>%
#     pull(number_devices_residing) %>%
#     sum()
# 
#   #Compute ratio
#   recorded_ratio <- recorded_pop/recorded_sg_pop
# 
#   patterns <-
#     patterns %>%
#     mutate(
#       origin_visitor_counts_high =
#         ifelse(
#           !is.na(origin_census_block_group),
#           origin_raw_visitor_counts_high*pop/number_devices_residing,
#           origin_raw_visitor_counts_high*recorded_ratio
#         ),
#       origin_visitor_counts_low =
#         ifelse(
#           !is.na(origin_census_block_group),
#           origin_raw_visitor_counts_low*pop/number_devices_residing,
#           origin_raw_visitor_counts_low*recorded_ratio
#         )
#     )
# 
#   #Finally get visit_counts by multiplying by ratio of raw visit:visitor ratio.
#   patterns <-
#     patterns %>%
#     mutate(
#       visit_counts_high = origin_visitor_counts_high*raw_visit_counts/raw_visitor_counts,
#       visit_counts_low = origin_visitor_counts_low*raw_visit_counts/raw_visitor_counts,
#       visit_counts = visit_counts_high
#     )
# 
#   head(patterns)
#   patterns
# }
# X2020_06_08_weekly_patterns_csv <- filter(X2020_06_08_weekly_patterns_csv, postal_code == 77005)
# block_method <- normBG(X2020_06_08_weekly_patterns_csv, X2020_06_08_home_panel_summary, norm_houston_blockgroups, norm_tx_pop_blockgroup)
# head(block_method %>% select(location_name, city, raw_visit_counts, visit_counts))
