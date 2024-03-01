library(tidycensus)
library(tidyverse)
library(tigris)
library(tidyverse)
library(sf)
library(tidycensus)
library(mapview)
library(crsuggest)
library(spdep)
library(mapboxapi)
library(leaflet)
library(leafsync)
library(glue)
library(kableExtra)
library(kable)
options(tigris_use_cache = TRUE)


#get our median income data
income_data_md <- get_acs(geography = "county", 
                          state="Maryland",
                       variables = c(hhinc="B19013_001"),
                       year=2022,
                       geometry = TRUE)
#find what coords to use for a map with tigris
mdcounties <- counties("MD")
suggest_crs(mdcounties)
#6488 is the crs code

#descending order, run without alst line for ascending order
income_data_md%>%
  st_transform(6318) %>%
  mutate(NAME = str_remove(NAME, " County, Maryland"))%>%
  arrange(desc(estimate))

#here is the plot
ggplot(income_data_md) + 
  geom_sf(aes(fill = estimate), color = NA) + 
  scale_fill_viridis_c() + 
  theme_void()+  
  labs(fill = "Median Household Income",
                      title = "                 Median Household Income by County in MD in 2022",
                    
                      caption = "Data sources: US Census Bureau, 1 year ACS")

#Question 2, MO education levels


mo_education <- get_acs(geography = "county", 
                        variables = c(graduate="B07009_006"),
                        summary_var =c(totalpop="B01003_001") ,
                        state = "MO", 
                        survey = "acs5",
                        year = 2022)


mo_education %>%
  mutate(NAME = str_remove(NAME, " County, Missouri"))%>%
  mutate(percentage_grad_degree = estimate / summary_est * 100) %>%
  select(NAME, percentage_grad_degree)%>%arrange(desc(percentage_grad_degree))


#compare to NY and CA education levels

ny_education <- get_acs(geography = "county", 
                        variables = c(graduate="B07009_006"),
                        summary_var =c(totalpop="B01003_001") ,
                        state = "NY", 
                        survey = "acs5",
                        year = 2022)
ny_education %>%
  mutate(NAME = str_remove(NAME, " County, New York"))%>%
  mutate(percentage_grad_degree = estimate / summary_est * 100) %>%
  select(NAME, percentage_grad_degree)%>%arrange(desc(percentage_grad_degree))

ca_education <- get_acs(geography = "county", 
        variables = c(graduate="B07009_006"),
        summary_var =c(totalpop="B01003_001") ,
        state = "CA", 
        survey = "acs5",
        year = 2022)


ca_education %>%
  mutate(NAME = str_remove(NAME, " County, California"))%>%
  mutate(percentage_grad_degree = estimate / summary_est * 100) %>%
  select(NAME, percentage_grad_degree)%>%arrange(desc(percentage_grad_degree))


#making table to compare results in Word

