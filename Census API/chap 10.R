library(tidycensus)
library(tidyverse)

#basic micro data
ms_pums <- get_pums(
  variables = c("SEX", "AGEP"),
  state = "MS",
  survey = "acs5",
  year = 2020,
  recode = TRUE
)

#total num of people
sum(ms_pums$PWGTP)
#tverse tools can do this too
ms_pums %>% count(wt = PWGTP)
#count can do mutiple vars as well
ms_pums %>%
  count(SEX_label, AGEP, wt = PWGTP) 
#we can get more specific, here is over 65 by sex
ms_pums %>%
  filter(AGEP >= 65) %>%
  count(SEX, wt = PWGTP)
#just to be sure, use acs to check the answer
get_acs(geography = "state",
        state = "MS",
        variables = c("DP05_0030", "DP05_0031"),
        year = 2020)
#because PUMS is a smaller sample size, it uses more estiamtes than ACS and has more error

#we can use more tverse tools
#this will get rent burden based on race
hh_variables <- c("PUMA", "GRPIP", "RAC1P", 
                  "HISP", "HHT")

#raw data with no labels
ms_hh_data <- get_pums(
  variables = hh_variables, 
  state = "MS",
  year = 2020,
  variables_filter = list(
    SPORDER = 1,
    TEN = 3
  ),
  recode = TRUE
)

#we can recode data for better labels
ms_hh_recoded <- ms_hh_data %>%
  mutate(
    race_ethnicity = case_when(
      HISP != "01" ~ "Hispanic",
      HISP == "01" & RAC1P == "1" ~ "White",
      HISP == "01" & RAC1P == "2" ~ "Black",
      TRUE ~ "Other"
    ),
    married = case_when(
      HHT == "1" ~ "Married",
      TRUE ~ "Not married"
    )
  )
#now we filter and group by martial status and race
#this gives us the percentage of income that these different households pay in rent
#based on maritial status and race
ms_hh_summary <- ms_hh_recoded %>%
  filter(race_ethnicity != "Other") %>%
  group_by(race_ethnicity, married) %>%
  summarize(
    prop_above_40 = sum(WGTP[GRPIP >= 40]) / sum(WGTP)
  )

#now we can map it
library(tigris)
library(tmap)
options(tigris_use_cache = TRUE)

ms_pumas <- pumas("MS", year = 2020)

plot(ms_pumas$geometry)

#for our map we are going to edit the data to show
#the rent burden of unmarried black hosueholds

#make into percentages
ms_data_for_map <- ms_hh_recoded %>%
  group_by(race_ethnicity, married, PUMA) %>%
  summarize(
    percent_above_40 = 100 * (sum(WGTP[GRPIP >= 40]) / sum(WGTP))
  ) %>%
  filter(race_ethnicity == "Black",
         married == "Not married")

#join it to the geometry
joined_pumas <- ms_pumas %>%
  left_join(ms_data_for_map, by = c("PUMACE10" = "PUMA"))

tm_shape(joined_pumas) + 
  tm_polygons(col = "percent_above_40", 
              palette = "Reds",
              title = "% rent-burdened\nunmarried Black households") + 
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "right")


