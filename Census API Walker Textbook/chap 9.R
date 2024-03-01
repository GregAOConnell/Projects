###Census micro data chapter 9

library(tidycensus)
library(tidyverse)
library(tigris)

options(tigris_use_cache = TRUE)

##using get_pums() we can find micro data
wy_pums <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "WY",
  survey = "acs1",
  year = 2019
)

#playing with the data, this finds people that are 50
wy_age_50 <- filter(wy_pums, AGEP == 50)
#total population of WY
print(sum(wy_pums$PWGTP))
#proportion of the population that is 50
print(sum(wy_age_50$PWGTP))


##We can also find household level data
#selects specifc houshold
wy_hh_example <- filter(wy_pums, SERIALNO == "2019HU0456721")
#Tells you whos in the house
wy_hh_example

#find the total number of households in WY
wy_households <- filter(wy_pums, SPORDER == 1)

sum(wy_households$WGTP)

#This returns households that are vacant, with NA for people catagories
wy_with_vacant <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "WY",
  survey = "acs1",
  year = 2019,
  return_vacant = TRUE
) %>%
  arrange(VACS)


#shows all our vars with the levels
View(pums_variables)

#recode gives extra context on specific vars, in this case its labels
wy_pums_recoded <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "WY",
  survey = "acs1",
  year = 2019,
  recode = TRUE
)

#we can also filter PUMS data This one gets women in a specific age range
wy_pums_filtered <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "WY",
  survey = "acs5",
  variables_filter = list(
    SEX = 2,
    AGEP = 30:49
  ),
  year = 2019
)

#here is geometric data on the PUMAs in WY, 
#Public Use Microdata Area
wy_pumas <- pumas(state = "WY", cb = TRUE, year = 2019)

ggplot(wy_pumas) + 
  geom_sf() + 
  theme_void()
#we can find the Puma names
wy_pumas$NAME10

#in a place like NYC Pumas will show up in known areas
nyc_pumas <- pumas(state = "NY", cb = TRUE, year = 2019) %>%
  filter(str_detect(NAME10, "NYC"))

ggplot(nyc_pumas) + 
  geom_sf() + 
  theme_void()
nyc_pumas$NAME10[1:5]

#we can sort the age by PUMA
wy_age_by_puma <- get_pums(
  variables = c("PUMA", "AGEP"),
  state = "WY",
  survey = "acs5",
  year = 2019
)
#make subsets of specific PUMAs
wy_puma_subset <- get_pums(
  variables = "AGEP",
  state = "WY",
  survey = "acs5",
  puma = "00500",
  year = 2019
)
#and even do it in multiple states
twostate_puma_subset <- get_pums(
  variables = "AGEP",
  state = "multiple",
  survey = "acs5",
  puma = c("WY" = "00500", "UT" = "05001"),
  year = 2019
)


#############################Exercises####################
#1
md_puma <- get_pums(variables=c("AGEP", "PUMA", "SEX"),
                    state= "MD",
                    survey= "acs5",
                    year=2019
  
)
view(pums_variables)
