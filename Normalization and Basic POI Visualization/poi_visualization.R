library(dplyr)
library(tidycensus)
library(tidyverse)
library(sf)
library(mapview)
library(usmap)
library(rjson)
library(censusapi)
library(leaflet)
library(shiny)

source("C:/Users/roone/Documents/normalization.R")

# # Importing Weekly Patterns & Home Summary
# X2020_06_08_weekly_patterns_csv <- read_csv("C:/Users/roone/Downloads/FireShot/safegraph/2020-06-08-weekly-patterns.csv.gz")
# X2020_06_08_home_panel_summary <- read_csv("C:/Users/roone/Downloads/FireShot/safegraph/2020-06-08-home-panel-summary.csv")

# Combining core-POI from SG by binding the rows 
core_poi_part1_csv <- read_csv("C:/Users/roone/Downloads/FireShot/safegraph/Core-USA-June2020-Release-CORE_POI-2020_05-2020-06-06/core_poi-part1.csv.gz")
core_poi_part2_csv <- read_csv("C:/Users/roone/Downloads/FireShot/safegraph/Core-USA-June2020-Release-CORE_POI-2020_05-2020-06-06/core_poi-part2.csv.gz")
core_poi_part3_csv <- read_csv("C:/Users/roone/Downloads/FireShot/safegraph/Core-USA-June2020-Release-CORE_POI-2020_05-2020-06-06/core_poi-part3.csv.gz")
core_poi_part4_csv <- read_csv("C:/Users/roone/Downloads/FireShot/safegraph/Core-USA-June2020-Release-CORE_POI-2020_05-2020-06-06/core_poi-part4.csv.gz")
core_poi_part5_csv <- read_csv("C:/Users/roone/Downloads/FireShot/safegraph/Core-USA-June2020-Release-CORE_POI-2020_05-2020-06-06/core_poi-part5.csv.gz")
poi <- bind_rows(core_poi_part1_csv, core_poi_part2_csv, core_poi_part3_csv, core_poi_part4_csv, core_poi_part5_csv)


# Filtering the POI for TX
poi_tx <- filter(poi, region == "TX")

options(
  tigris_class = "sf",
  tigris_use_cache = TRUE
)

# https://api.census.gov/data/key_signup.html
census_api_key("5e5c985bc80f5902a674b81c64ce28a377b4bea8", install = TRUE)
Sys.setenv(CENSUS_KEY="5e5c985bc80f5902a674b81c64ce28a377b4bea8")


greater_houston_county_names <-
  c(
    "Harris County",
    "Fort Bend County",
    "Montgomery County",
    "Brazoria County",
    "Galveston County",
    "Liberty County",
    "Waller County",
    "Chambers County",
    "Austin County"
  )
greater_houston_county_names_l <-
  c(
    "harris",
    "fort bend",
    "montgomery",
    "brazoria",
    "galveston",
    "liberty",
    "waller",
    "chambers",
    "austin"
  )


# The safegraph data doesn't contain county data for a POI so we go around that by
# using this block of code to get cities contained within our specified counties 
# and then filter out the safegraph weekly patterns POI with these cities to get
# the poi within the greater houston area
temp <- tempfile()
download.file("http://download.geonames.org/export/zip/US.zip",temp)
con <- unz(temp, "US.txt")
US <- read.delim(con, header=FALSE)
unlink(temp)

## Find state and county

colnames(US)[c(3,5,6)] <- c("city","state","county")
US$county <- tolower(US$county)
US <- US %>% filter(state == "TX")
US <- US %>% filter(is.element(county, greater_houston_county_names_l))
US <- US[!duplicated(US$city),]


# Converting the city column into a vector
greater_houston_cities <- US[['city']]
greater_houston_cities

# Using Safegraph ID on every POI, we can bind the weekly patterns data for a POI
# to it's geographical location
estimated_visits <- 
  general_pop_normalization %>% 
  filter(city %in% greater_houston_cities) %>% 
  left_join(
    poi_tx %>% 
      dplyr::select(
        safegraph_place_id,
        latitude,
        longitude,
        top_category,
        sub_category,
        naics_code
      ),
    by = "safegraph_place_id"
  )

estimated_visits
# weekly_pattern_w_geometry <- arrange(weekly_pattern_w_geometry, desc(raw_visit_counts))
# head(weekly_pattern_w_geometry[,c("location_name", "raw_visit_counts", "date_range_start", "date_range_end")])

# Using the normalization function to get the estimated visits column for a POI
# estimated_visits <- normhouston(weekly_pattern_w_geometry, X2020_06_08_home_panel_summary, norm_houston_blockgroups, greater_houston_pop)
# head(estimated_visits[,c("location_name", "raw_visit_counts", "visit_counts", "naics_code")])


# Creating polygonal boundaries based on greater houston cities 
# houston_boundary <-
#   places("TX",cb=F,progress_bar=F) %>%
#   filter(NAME %in% greater_houston_cities) %>%
#   st_transform(4326)

# Creating polygonal boundaries based on counties 
houston_boundary <- 
  counties(state = "TX") %>% filter(NAMELSAD %in% greater_houston_county_names) %>%  st_transform(4326)
US <- select(US, city, county)
head(US)

# This adds a county column to a POI
estimated_visits <- left_join(estimated_visits,
  US,
  by = c("city" = "city")
)

estimated_visits
estimated_visits <- mutate(estimated_visits, date = as.Date(date_range_start))

## You can use this to filter out a certain POI type based on the NAICS code of the POI
# estimated_visits <- estimated_visits %>% filter(round(naics_code/10000, 0) == 62)


estimated_visits <- estimated_visits %>% dplyr::filter(region == "TX") %>%
  group_by(county) %>% top_n(20,visit_counts)

# This groups the data into the top X for a certain geography of your location.
# In this case we're getting the top 5 POI for estimated visits per postal code
# estimated_visits <- estimated_visits %>% group_by(county) %>% top_n(20,visit_counts)

# # Example code to have a two color graph 
#estimated_visits$color <- if else(estimated_visits$visit_counts>=10000, "red", "blue")

pal <- colorNumeric(
  palette = "plasma",
  domain = estimated_visits$visit_counts)

ui <- fluidPage(
  p(),
  selectInput("select", "Traffic for the week of:", choices = c("2020-06-01", "2020-06-08"), selected = "2020-06-01"),
  textOutput("selected_var"),
  # dateRangeInput("dates", "Weekly Patterns", start = "2020-06-01", end = "2020-06-08"),
  leafletOutput("mymap"),
)

server <- function(input, output, session) {
  output$selected_var <- renderText({ 
    paste("You have selected", input$select)
  })
  
  # estimated_visits <- estimated_visits %>% dplyr::filter(date == input$select) %>%
  #   group_by(county) %>% top_n(20,visit_counts)
  
  output$mymap <- renderLeaflet({
    # Visualization using leaflet:
    estimated_visits %>% dplyr::filter(date == input$select) %>%
      group_by(county) %>% top_n(20,visit_counts) %>%
      # mutate(
      #   log_visit_counts = 
      #     pmax(
      #       log(visit_counts,10),
      #       0
      #     )
      # ) %>% 
      dplyr::select(
        location_name,
        street_address,
        city,
        region,
        postal_code,
        brands,
        naics_code,
        longitude,
        latitude,
        raw_visit_counts,
        visit_counts
      ) %>% 
      leaflet() %>% 
      # addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = houston_boundary,
        fill = F,
        color = "black",
        weight = 1,
        label = ~NAME
      ) %>% 
      addCircles(
        lng = ~longitude,
        lat = ~latitude,
        color = ~pal(estimated_visits$visit_counts),
        weight = .5,
        radius = ~sqrt(visit_counts)*4,
        fillOpacity = .5,
        label = ~paste0(location_name, ": ", round(visit_counts,-1)," visits in week of 06/08/20"),
        highlightOptions = highlightOptions(
          weight = 2,
          opacity = 1
        )
      ) %>% 
      addLegend("bottomright", pal = pal, values = ~visit_counts,
                title = "Estimated visits to the Top 20 POI in each county",
                # labFormat = labelFormat(prefix = "",
                opacity = 1
      )
    
  })
}

shinyApp(ui, server)

# # Visualization using leaflet:
# estimated_visits %>% 
#   # mutate(
#   #   log_visit_counts = 
#   #     pmax(
#   #       log(visit_counts,10),
#   #       0
#   #     )
#   # ) %>% 
#   dplyr::select(
#     location_name,
#     street_address,
#     city,
#     region,
#     postal_code,
#     brands,
#     naics_code,
#     longitude,
#     latitude,
#     raw_visit_counts,
#     visit_counts
#   ) %>% 
#   leaflet() %>% 
#   # addTiles() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(
#     data = houston_boundary,
#     fill = F,
#     color = "black",
#     weight = 1,
#     label = ~NAME
#   ) %>% 
#   addCircles(
#     lng = ~longitude,
#     lat = ~latitude,
#     color = ~pal(estimated_visits$visit_counts),
#     weight = .5,
#     radius = ~sqrt(visit_counts)*4,
#     fillOpacity = .5,
#     label = ~paste0(location_name, ": ", round(visit_counts,-1)," visits in week of 06/08/20"),
#     highlightOptions = highlightOptions(
#       weight = 2,
#       opacity = 1
#     )
#   ) %>% 
#   addLegend("bottomright", pal = pal, values = ~visit_counts,
#             title = "Estimated visits to the Top 20 POI in each county",
#             # labFormat = labelFormat(prefix = "",
#             opacity = 1
#   )



# # Working on creating the Cholorgraph
# colnames(estimated_visits)
# cbgs_visits <-
#   estimated_visits %>% select(poi_cbg, visit_counts, ) %>% 
#   group_by(poi_cbg) %>% 
#   summarize(
#     # visitor_counts = sum(origin_visitor_counts, na.rm=T),
#     visit_counts = sum(visit_counts, na.rm=T)
#   )
# 
#   tx_blockgroups <-
#     block_groups("TX",cb=F,progress_bar=F) %>% 
#     st_transform(4326) %>% 
#     dplyr::select(GEOID)
#   
#   blue_pal <- colorNumeric(
#     palette = "Blues",
#     domain = 
#       cbgs_visits %>% 
#       pull(visit_counts),
#   )
#   
#   top_visited_park_sum <-
#     cbgs_visits %>% 
#     left_join(
#       tx_blockgroups,
#       by = c("poi_cbg" = "GEOID")
#     ) %>%
#     st_as_sf()
#   
#   top_visited_park_sum
#   
#   # m <- leaflet(states) %>%
#   #   setView(-96, 37.8, 4)
#   
#   leaflet() %>% 
#     addProviderTiles(providers$CartoDB.Positron) %>% 
#     addPolygons(
#       data = top_visited_park_sum,
#       fillColor = ~blue_pal(visit_counts),
#       color = "blue",
#       weight = 1,
#       opacity = 0.5,
#       fillOpacity = 0.75,
#       label = ~paste0(round(visit_counts,-1)," visits to ",poi_cbg," in week of 4/5/20"),
#       highlightOptions = highlightOptions(
#         weight = 2,
#         opacity = 0.75
#       )
#     ) %>% 
#     addPolygons(
#       data = houston_boundary,
#       fill = F,
#       color = "white",
#       weight = 1,
#       label = ~NAME
#     ) %>% 
#     addLegend("bottomright", pal = blue_pal, values = cbgs_visits$visit_counts,
#               title = "Number of visits to an CBGS",
#               # labFormat = labelFormat(prefix = "",
#               opacity = 1
#     )
  