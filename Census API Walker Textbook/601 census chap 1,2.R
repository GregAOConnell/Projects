
#601
install.packages("tidycensus")
library(tidycensus)
library(tidyverse)
census_api_key("39744f23b9d8667a749fffe6ddb09db187ac3f46", install = TRUE, overwrite=T)


#2.1 Get Census Data
#total population
total_population_10 <- get_decennial(
  geography = "state", 
  variables = "P001001",
  year = 2010
)

#native american population by state
aian_2020 <- get_decennial(
  geography = "state",
  variables = "P1_005N",
  year = 2020,
  sumfile = "pl"
)
detectCores()
#acs born in mexico
born_in_mexico <- get_acs(
  geography = "state", 
  variables = "B05006_150",
  year = 2020
)
born_in_mexico_1yr <- get_acs(
  geography = "state", 
  variables = "B05006_150", 
  survey = "acs1",
  year = 2019
)
#sex broken down by age 2016-2020
age_table <- get_acs(
  geography = "state", 
  table = "B01001",
  year = 2020
)

#core based statisitcal areas
cbsa_population <- get_acs(
  geography = "cbsa",
  variables = "B01003_001",
  year = 2020
)
#income by county wisco
wi_income <- get_acs(
  geography = "county", 
  variables = "B19013_001", 
  state = "WI",
  year = 2020
)

#income by census track in dane county wisco
dane_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001", 
  state = "WI", 
  county = "Dane",
  year = 2020
)

nrow(wi_income)

#ACS 1year only trakcs councties over 65k people so it only has 23 rows as opposed to 72 on regular income variable
wi_income_1yr <- get_acs(
  geography = "county", 
  variables = "B19013_001", 
  state = "WI",
  year = 2019,
  survey = "acs1"
)

nrow(wi_income_1yr)

v16 <- load_variables(2016, "acs5", cache = TRUE)
v16


#income groups by state, showing wide mode too
hhinc <- get_acs(
  geography = "state", 
  table = "B19001", 
  survey = "acs1",
  year = 2016
)

hhinc_wide <- get_acs(
  geography = "state", 
  table = "B19001", 
  survey = "acs1", 
  year = 2016,
  output = "wide"
)

#shows geoIDs which have lots of info to interpret
cimarron_blocks <- get_decennial(
  geography = "block",
  variables = "H1_001N",
  state = "OK",
  county = "Cimarron",
  year = 2020,
  sumfile = "pl"
)

#renaming variables
ga <- get_acs(
  geography = "county",
  state = "Georgia",
  variables = c(medinc = "B19013_001",
                medage = "B01002_001"),
  year = 2020
)

#exercise 1

exe1 <- get_acs(
  geography= "tract",
  state= "VA",
  county="Fairfax County",
  variables=c(medage="B01002_001"),
  year=2016
  
)

#exercise 2, median income in each county of Maryland in the past 12 months (from 2016) adjusted for inflation
View(load_variables(2016, "acs5"))

exe2 <- get_acs(
  geography= "county",
  state="MD",
  
  variables=c(medincome12="B06011_001"),
  year=2016
  
)

