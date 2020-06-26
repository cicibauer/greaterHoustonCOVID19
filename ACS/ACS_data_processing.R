###Download census data using API in R and clean the dataset###

#Data source: U.S. Census Bureau, 2011-2015 American Community Survey 5-Year Estimates
#https://www.census.gov/data.html
#The first step is to get a Census API key
# More information about the Census Bureau's API, including where to request an API key, can be found at https://www.census.gov/developers/.
# An API key is not required to use the Census Bureau API unless you are making more than 500 API calls per day. 
#However, having your own API key is a good practice to use and keyholders have access to the latest Census Bureau API updates and other communications.


#Load required libraries
library(tidyverse)
library(tidycensus)
# Replace "API Key" with your API key once you get it
census_api_key("49626ba8c2a4dc3fc0fa50c345c5f2ba8078e3ff", install="TRUE")

#View all the possible variables we can use in the dataset
acsvar15 <- load_variables(year= 2015, dataset="acs5/profile", cache=TRUE)
View(acsvar15)
#View tha loaded variables and filter to choose the variables of interest
varlist<- c("DP02_0071P", "DP02_0121P", "DP02_0007P", "DP02_0009P", "DP02_0092P", "DP02_0059P", "DP02_0060P", "DP02_0113P", "DP03_0005PE", 
            "DP03_0099PE", "DP03_0062E", "DP03_0088E", "DP03_0025E", "DP03_0128E", "DP04_0077P", "DP04_0003P", "DP04_0089", "DP04_0047P", 
            "DP04_0058P", "DP04_0141P", "DP04_0141P", "DP05_0001E", "DP05_0018P", "DP05_0021P", "DP05_0066P", "DP05_0072P")
            
#Get the dataset for year 2015
acsdata15 <- get_acs(geography = "tract",  variables=varlist, survey="acs5", year=2015)

#Clean the datsaet
cleandata <- acsdata15 %>%  spread(key="variable", value="estimate") %>%
  mutate(Crowded_hs= 100- DP04_0077P,                 #Define variable crowded housing
         No_high_school= DP02_0059P+DP02_0060P,  #Define variable No high school diploma
         Rent_burden <- DP04_0141P+ DP04_0142P,    #Define variable rent burden 
         Under18 <- 100-DP05_0018P,               #Define variable Under18
         Racial_minority <- 100 - DP05_0072P,     #Define variable Racial minority
         Single_parent_household= DP02_0007PE+ DP02_0009PE,
         Year=2015) %>%                            #Define variable year
  mutate_each(funs(replace(., .<0, NA))) %>%        #Replace missing values with NA
  rename(Hispanic=DP05_0066P, Unemployed=DP03_0005P,  Renters=DP04_0047P, Per_capita_income=DP03_0088, Living_poverty=DP03_0119P, 
         Uninsured=DP03_0099P,  Total_population=DP05_0001, Disability=DP02_0071PE, limited_English=DP02_0113PE, Over65= DP05_0021PE)     #Rename the variables 
