library(tidycensus)
library(tidyverse)
library(tmap)
library(tigris)
usmedinc <-get_acs(
  variable="B19013_001",
  geography = "state",
  year = 2019,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry()
ggplot(data = usmedinc, aes(fill = estimate)) + 
  geom_sf()
ggplot(data = usmedinc, aes(fill = estimate)) + 
  geom_sf() + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1) + 
  labs(title = "  Median Age by State, 2019",
       caption = "Data source: 2019 1-year ACS, US Census Bureau",
       fill = "ACS estimate") + 
  theme_void()
