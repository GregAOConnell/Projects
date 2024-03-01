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
options(tigris_use_cache = TRUE)

mb_access_token("pk.eyJ1IjoiZ3JlZ29jIiwiYSI6ImNscDA1dmg0ejA0aGkycXAzaHZqcjE1OXoifQ.lLD4oWwvPtSETGFok_YaHw", install = TRUE)


################Census tract borders and metropolitan areas##################
# CRS used: NAD83(2011) Kansas Regional Coordinate System 
# Zone 11 (for Kansas City)

#This makes an empty map with tracts in it
ks_mo_tracts <- map_dfr(c("KS", "MO"), ~{
  tracts(.x, cb = TRUE, year = 2020)
}) %>%
  st_transform(8528)  


#finds your cbsa, need to be very specific with the string

kc_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Kansas City")) %>%
  st_transform(8528)




#makes the red line and plots tracts

ggplot() + 
  geom_sf(data = ks_mo_tracts, fill = "white", color = "grey") + 
  geom_sf(data = kc_metro, fill = NA, color = "red") + 
  theme_void()
kc_tracts <- ks_mo_tracts[kc_metro, ]

ggplot() + 
  geom_sf(data = kc_tracts, fill = "white", color = "grey") + 
  geom_sf(data = kc_metro, fill = NA, color = "red") + 
  theme_void()

#####################exe1 attempt, making a map the outlines DC Metro area##########################
#blank map with tracts
dmv_tracts <- map_dfr(c("DC", "MD", "VA", "WV"), ~{
  tracts(.x, cb = TRUE, year = 2020)
}) %>%
  st_transform(8528)  


#set metro as object
dmv_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Washington-Arlington-Alexandria, DC-VA-MD-WV"))%>%
  st_transform(8528)



#outline object in a red line
ggplot() + 
  geom_sf(data = dmv_tracts, fill = "white", color = "grey") + 
  geom_sf(data = dmv_metro, fill = NA, color = "red") + 
  theme_void()





#########################given specific data points, make a map around them###############

#We will find elderly people in Florida, and see how the healthcare is around them

#specific coordinates of patients we want to study in FL
gainesville_patients <- tibble(
  patient_id = 1:10,
  longitude = c(-82.308131, -82.311972, -82.361748, -82.374377, 
                -82.38177, -82.259461, -82.367436, -82.404031, 
                -82.43289, -82.461844),
  latitude = c(29.645933, 29.655195, 29.621759, 29.653576, 
               29.677201, 29.674923, 29.71099, 29.711587, 
               29.648227, 29.624037)
)

#this maps our coordinates in a map
# CRS: NAD83(2011) / Florida North
gainesville_sf <- gainesville_patients %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  st_transform(6440)

#views points in map and colors them red
mapview(
  gainesville_sf, 
  col.regions = "red",
  legend = FALSE
)


#now we find insurance coverage by area
alachua_insurance <- get_acs(
  geography = "tract",
  variables = "DP03_0096P",
  state = "FL",
  county = "Alachua",
  year = 2019,
  geometry = TRUE
) %>%
  select(GEOID, pct_insured = estimate, 
         pct_insured_moe = moe) %>%
  st_transform(6440)



#stick that insurance object on our map and add previous code
mapview(
  alachua_insurance,
  zcol = "pct_insured", 
  layer.name = "% with health<br/>insurance"
) + 
  mapview(
    gainesville_sf,
    col.regions = "red",
    legend = FALSE
  )

#join these two maps
patients_joined <- st_join(
  gainesville_sf,
  alachua_insurance
)





##############################Graphing Spatial Data, population density############################

#Here we will find the population density in relation to...
#the percent of the population that is hispanic in a given metro area


# CRS: NAD83(2011) / Texas Centric Albers Equal Area
#collect data on racial makeup in texas metropolitian areas
tx_cbsa <- get_acs(
  geography = "cbsa",
  variables = "B01003_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE
) %>%
  filter(str_detect(NAME, "TX")) %>%
  slice_max(estimate, n = 4) %>%
  st_transform(6579)



####Finds the percentage of hispanic population
pct_hispanic <- get_acs(  geography = "tract",
  variables = "DP05_0071P",
  state = "TX",
  year = 2019,
  geometry = TRUE
) %>%
  st_transform(6579)


#joins our datasets so we can map it
hispanic_by_metro <- st_join(
  pct_hispanic,
  tx_cbsa,
  join = st_within,
  suffix = c("_tracts", "_metro"),
  left = FALSE
)


#editing our tables so the graphs work
#this graph provides population desnity in metro areas in relation to the percent hispanic population
hispanic_by_metro %>%
  mutate(NAME_metro = str_replace(NAME_metro, ", TX Metro Area", "")) %>%
  ggplot() + 
  geom_density(aes(x = estimate_tracts), color = "navy", fill = "navy", 
               alpha = 0.4) + 
  theme_minimal() + 
  facet_wrap(~NAME_metro) + 
  labs(title = "Distribution of Hispanic/Latino population by Census tract",
       subtitle = "Largest metropolitan areas in Texas",
       y = "Kernel density estimate",
       x = "Percent Hispanic/Latino in Census tract")




#######################Erasing Water in NY###################################
#map of median income in Manhattan NY
ny <- get_acs(
  geography = "tract", 
  variables = "B19013_001", 
  state = "NY", 
  county = "New York", 
  year = 2020,
  geometry = TRUE
)

ggplot(ny) + 
  geom_sf(aes(fill = estimate)) + 
  scale_fill_viridis_c(labels = scales::label_dollar()) + 
  theme_void() + 
  labs(fill = "Median household\nincome")


#now a new map
ny2 <- get_acs(
  geography = "tract",
  variables = "B19013_001", 
  state = "NY", 
  county = "New York", 
  geometry = TRUE, 
  year = 2020,
  cb = FALSE
) %>%
  st_transform(6538)

#erase the water and remap
ny_erase <- erase_water(ny2)
ggplot(ny_erase) + 
  geom_sf(aes(fill = estimate)) + 
  scale_fill_viridis_c(labels = scales::label_dollar()) + 
  theme_void() + 
  labs(fill = "Median household\nincome")



##exe 2 erasing water in King County WA
#find King County
wamap <- get_acs(
  geography = "tract",
  variables = "B19013_001", 
  state = "Washington", 
  county = "King County", 
  geometry = TRUE, 
  year = 2020,
  cb = FALSE
)%>%
  st_transform(6318)

#locate CRS to map it on to
wacrs <- tracts("WA", "King County", cb=T)
suggest_crs(wacrs)

#erase the water
wamaper <- erase_water(wamap)


#plot it
ggplot(wamaper) + 
  geom_sf(aes(fill = estimate)) + 
  scale_fill_viridis_c(labels = scales::label_dollar()) + 
  theme_void() + 
  labs(fill = "Median household\nincome")



###################Lets find a distribution of weights###################

#Dallas Fortworth metro area
dfw <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Dallas")) %>%
  st_transform(32138)

#pull tracts and median age
dfw_tracts <- get_acs(
  geography = "tract",
  variables = "B01002_001",
  state = "TX",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(32138) %>%
  st_filter(dfw, .predicate = st_within) %>%
  na.omit()

#map tracts with median age distribution
ggplot(dfw_tracts) + 
  geom_sf(aes(fill = estimate), color = NA) + 
  scale_fill_viridis_c() + 
  theme_void()

#this finds tracts with neighbors, how many other tracts they're near
neighbors <- poly2nb(dfw_tracts, queen = TRUE)
summary(neighbors)


#pulls coordinates for each tract, so we can draw lines between neighbors
dfw_coords <- dfw_tracts %>%
  st_centroid() %>%
  st_coordinates()

#plots basic map with no adds
plot(dfw_tracts$geometry)

#adds lines between the coords to their neighbors
plot(neighbors, 
     coords = dfw_coords, 
     add = TRUE, 
     col = "blue", 
     points = FALSE)
weights <- nb2listw(neighbors, style = "W")

weights$weights[[1]]

####Okay now lets do spatial lags between neighbors
##spatial lag finds how neighbors are related to eahother
#in this case it finds spaitial clustering of median age in tracts that are near eachother

#finds alg estimate with weights over tracts
dfw_tracts$lag_estimate <- lag.listw(weights, dfw_tracts$estimate)

#scatterplots of these median ages
ggplot(dfw_tracts, aes(x = estimate, y = lag_estimate)) + 
  geom_point(alpha = 0.3) + 
  geom_abline(color = "red") + 
  theme_minimal() + 
  labs(title = "Median age by Census tract, Dallas-Fort Worth TX",
       x = "Median age",
       y = "Spatial lag, median age", 
       caption = "Data source: 2016-2020 ACS via the tidycensus R package.\nSpatial relationships based on queens-case polygon contiguity.")


#make a hotspot map of autocorrelation between neighbors

#Use Z scores to find statisically significant clusters
dfw_tracts <- dfw_tracts %>%
  mutate(hotspot = case_when(
    localG >= 2.576 ~ "High cluster",
    localG <= -2.576 ~ "Low cluster",
    TRUE ~ "Not significant"
  ))

#Graph it red is hold high clusters of age, blue is young clusters
ggplot(dfw_tracts) + 
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) + 
  scale_fill_manual(values = c("red", "blue", "grey")) + 
  theme_void()


####LISA version
#set seed so its recreatable with random Z scores
set.seed(1983)

#scales estiamtes of age into Z scores
dfw_tracts$scaled_estimate <- as.numeric(scale(dfw_tracts$estimate))

#Two sided tail test with a local moran figure
#Finds LISA numbers
dfw_lisa <- localmoran_perm(
  dfw_tracts$scaled_estimate, 
  weights, 
  nsim = 999L, 
  alternative = "two.sided"
) %>%
  as_tibble() %>%
  set_names(c("local_i", "exp_i", "var_i", "z_i", "p_i",
              "p_i_sim", "pi_sim_folded", "skewness", "kurtosis"))

#stick the LISA number on the data frame
dfw_lisa_df <- dfw_tracts %>%
  select(GEOID, scaled_estimate) %>%
  mutate(lagged_estimate = lag.listw(weights, scaled_estimate)) %>%
  bind_cols(dfw_lisa)

#Now this does quadrants
dfw_lisa_clusters <- dfw_lisa_df %>%
  mutate(lisa_cluster = case_when(
    p_i >= 0.05 ~ "Not significant",
    scaled_estimate > 0 & local_i > 0 ~ "High-high",
    scaled_estimate > 0 & local_i < 0 ~ "High-low",
    scaled_estimate < 0 & local_i > 0 ~ "Low-low",
    scaled_estimate < 0 & local_i < 0 ~ "Low-high"
  ))
#color them differently
color_values <- c(`High-high` = "red", 
                  `High-low` = "pink", 
                  `Low-low` = "blue", 
                  `Low-high` = "lightblue", 
                  `Not significant` = "white")
#This makes a scatter plot with the scaled and lagged estimates
ggplot(dfw_lisa_clusters, aes(x = scaled_estimate, 
                              y = lagged_estimate,
                              fill = lisa_cluster)) + 
  geom_point(color = "black", shape = 21, size = 2) + 
  theme_minimal() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_fill_manual(values = color_values) + 
  labs(x = "Median age (z-score)",
       y = "Spatial lag of median age (z-score)",
       fill = "Cluster type")

ggplot(dfw_lisa_clusters, aes(fill = lisa_cluster)) + 
  geom_sf(size = 0.1) + 
  theme_void() + 
  scale_fill_manual(values = color_values) + 
  labs(fill = "Cluster type")


###################Mapping Arizone weighted maricopa county shift in work from home#################
# CRS: NAD 83 / Arizona Central

#finds data on work from hope in Central Arizona in 2015
wfh_15 <- get_acs(
  geography = "tract",
  variables = "B08006_017",
  year = 2015,
  state = "AZ",
  county = "Maricopa",
  geometry = TRUE
) %>%
  select(estimate) %>%
  st_transform(26949)

#This finds data on work from home in C AZ in 2020
wfh_20 <- get_acs(
  geography = "tract",
  variables = "B08006_017",
  year = 2020,
  state = "AZ",
  county = "Maricopa",
  geometry = TRUE
) %>%
  st_transform(26949)

#we are combining these tables joining by GEOID
wfh_interpolate_aw <- st_interpolate_aw(
  wfh_15,
  wfh_20,
  extensive = TRUE
) %>%
  mutate(GEOID = wfh_20$GEOID)

#Finding census blocks in Mariope coutny
maricopa_blocks <- blocks(
  state = "AZ",
  county = "Maricopa",
  year = 2020
)
#weighting by the bloacks in Maricopa county
wfh_interpolate_pw <- interpolate_pw(
  wfh_15,
  wfh_20,
  to_id = "GEOID",
  extensive = TRUE, 
  weights = maricopa_blocks,
  weight_column = "POP20",
  crs = 26949
)

#We are subtracting the estimates and joining the table to find 
wfh_shift <- wfh_20 %>%
  left_join(st_drop_geometry(wfh_interpolate_pw), 
            by = "GEOID",
            suffix = c("_2020", "_2015")) %>%
  mutate(wfh_shift = estimate_2020 - estimate_2015)

maricopa_basemap <- layer_static_mapbox(
  location = wfh_shift,
  style_id = "dark-v9",
  username = "mapbox"
)

#now we plot and make it pretty

ggplot() + 
  maricopa_basemap + 
  geom_sf(data = wfh_shift, aes(fill = wfh_shift), color = NA, 
          alpha = 0.8) + 
  scale_fill_distiller(palette = "PuOr", direction = -1) + 
  labs(fill = "Shift, 2011-2015 to\n2016-2020 ACS",
       title = "Change in work-from-home population",
       subtitle = "Maricopa County, Arizona") + 
  theme_void()




#####################Accessibility measurement with spatial data##############################
#we are going to look at access to trauam level 1 and 2 hospitals in Iowa



# CRS: NAD83 / Iowa North
#start with tracts
ia_tracts <- tracts("IA", cb = TRUE, year = 2019) %>%
  st_transform(26975)

#DHS hospital location link
hospital_url <- "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Hospital/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"



#reads link, finds string we need for trauma hospitals
trauma <- st_read(hospital_url) %>%
  filter(str_detect(TRAUMA, "LEVEL I\\b|LEVEL II\\b|RTH|RTC")) %>%
  st_transform(26975) %>%
  distinct(ID, .keep_all = TRUE)
names(trauma)


##This finds some hospitals that are not in Iowa, but near it
#other states within 100km
ia_trauma <- trauma %>%
  st_filter(ia_tracts, 
            .predicate = st_is_within_distance,
            dist = 100000)
#plots the trauma hospitals and points them in red points
ggplot() + 
  geom_sf(data = ia_tracts, color = "NA", fill = "grey50") + 
  geom_sf(data = ia_trauma, color = "red") + 
  theme_void()

#This takes the distance from the centriod of census tracts and each hospital
dist <- ia_tracts %>%
  st_centroid() %>%
  st_distance(ia_trauma) 

#Only viewing 5 but its a huge matrix
dist[1:5, 1:5]

#Lets look at a historgram of the minimum distance to a hospital
min_dist <- dist %>%
  #apply iterates new vector on matrix apply((row=1,colum=2), what you want to make a new row of, in this case min)
  apply(1, min) %>%
  as.vector() %>%
  magrittr::divide_by(1000)

hist(min_dist)



####Lets do this a different way, with travel times now
#mapbox package has a travel times matrix, its the defailt of mb_matrix()
times <- mb_matrix(ia_tracts, ia_trauma)
times[1:5, 1:5]

#now we use apply again to make a new vector for minimum time of each row of data
min_time <- apply(times, 1, min)

#add this to tracts dataframe
ia_tracts$time <- min_time

#plot it, set custom color, and it produces a chloropleth densit ymap
ggplot(ia_tracts, aes(fill = time)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_void() + 
  labs(fill = "Time (minutes)",
       title = "Travel time to nearest Level I or Level II trauma hospital",
       subtitle = "Census tracts in Iowa",
       caption = "Data sources: US Census Bureau, US DHS, Mapbox")


############################Take the Iowa Hospital Example, and Localize it#######################
#Here is a specific hospital
iowa_methodist <- filter(ia_trauma, ID == "0009850308")

#First map, we can just do a distance, 5km in this case
buf5km <- st_buffer(iowa_methodist, dist = 5000) 

#now lets take mapboxes travel times and do within 10minutes
iso10min <- mb_isochrone(
  iowa_methodist, 
  time = 10, 
  profile = "driving-traffic",
)

#Put a red icon on the hospital
hospital_icon <- makeAwesomeIcon(icon = "ios-medical", 
                                 markerColor = "red",
                                 library = "ion")


# The Leaflet package requires data be in CRS 4326
#Make an interactive map, first with a circle of 5km around the hospital
map1 <- leaflet() %>% 
  addTiles() %>%
  addPolygons(data = st_transform(buf5km, 4326)) %>% 
  addAwesomeMarkers(data = st_transform(iowa_methodist, 4326),
                    icon = hospital_icon)

#This will find a border of areas within 10 minutes driving distance
map2 <- leaflet() %>% 
  addTiles() %>%
  addPolygons(data = iso10min) %>% 
  addAwesomeMarkers(data = st_transform(iowa_methodist, 4326),
                    icon = hospital_icon)

#puts these maps next to eachother and take a look at the difference
sync(map1, map2)

#####Now lets find poverty statistics in the delinated areas near the hospital

#poverty stats in polk county, iowa,
polk_poverty <- get_acs(
  geography = "block group",
  variables = c(poverty_denom = "B17010_001",
                poverty_num = "B17010_002"),
  state = "IA",
  county = "Polk",
  geometry = TRUE,
  output = "wide",
  year = 2020
) %>%
  select(poverty_denomE, poverty_numE) %>%
  st_transform(26975)

#Now we find blocks
polk_blocks <- blocks(
  state = "IA",
  county = "Polk",
  year = 2020
)
#Weighted by block, find poverty rates in the 5km buffer
buffer_pov <- interpolate_pw(
  from = polk_poverty, 
  to = buf5km,
  extensive = TRUE,
  weights = polk_blocks,
  weight_column = "POP20",
  crs = 26975
) %>%
  mutate(pct_poverty = 100 * (poverty_numE / poverty_denomE))
#weighted by block, find poverty rates within 10 minutes
iso_pov <- interpolate_pw(
  from = polk_poverty, 
  to = iso10min,
  extensive = TRUE,
  weights = polk_blocks,
  weight_column = "POP20",
  crs = 26975
) %>%
  mutate(pct_poverty = 100 * (poverty_numE / poverty_denomE))



############################exe 3#########################################3

tnjtract <- tracts("NJ")
suggest_crs(tnjtract)
#Trenton NJ metro area
tnj <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Trenton-Princeton, NJ")) %>%
  st_transform(6318)

#pull tracts and median age
tnj_inc_tract <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NJ",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(6318) %>%
  st_filter(tnj, .predicate = st_within) %>%
  na.omit()

#map tracts with median inc distribution
ggplot(tnj_inc_tract) + 
  geom_sf(aes(fill = estimate), color = NA) + 
  scale_fill_viridis_c() + 
  theme_void()

#this finds tracts with neighbors, how many other tracts they're near
neighbors <- poly2nb(tnj_inc_tract, queen = TRUE)
summary(neighbors)


#pulls coordinates for each tract, so we can draw lines between neighbors
tnj_coords <- tnj_inc_tract %>%
  st_centroid() %>%
  st_coordinates()

#plots basic map with no adds
plot(tnj_inc_tract$geometry)

#adds lines between the coords to their neighbors
plot(neighbors, 
     coords = tnj_coords, 
     add = TRUE, 
     col = "blue", 
     points = FALSE)
weights <- nb2listw(neighbors, style = "W")

weights$weights[[1]]




####Okay now lets do spatial lags between neighbors
##spatial lag finds how neighbors are related to eahother
#in this case it finds spaitial clustering of median age in tracts that are near eachother

#finds alg estimate with weights over tracts
tnj_inc_tract$lag_estimate <- lag.listw(weights, tnj_inc_tract$estimate)

#scatterplots of these median ages
ggplot(tnj_inc_tract, aes(x = estimate, y = lag_estimate)) + 
  geom_point(alpha = 0.3) + 
  geom_abline(color = "red") + 
  theme_minimal() + 
  labs(title = "Median income by Census tract, Trenton NJ metro area",
       x = "Median income",
       y = "Spatial lag, median income", 
       caption = "Data source: 2016-2020 ACS via the tidycensus R package.\nSpatial relationships based on queens-case polygon contiguity.")

localg_weights <- nb2listw(include.self(neighbors))

tnj_inc_tract$localG <- localG(tnj_inc_tract$estimate, localg_weights)

ggplot(dfw_tracts) + 
  geom_sf(aes(fill = localG), color = NA) + 
  scale_fill_distiller(palette = "RdYlBu") + 
  theme_void() + 
  labs(fill = "Local Gi* statistic")

#make a hotspot map of autocorrelation between neighbors

#Use Z scores to find statisically significant clusters
tnj_inc_tract <- tnj_inc_tract %>%
  mutate(hotspot = case_when(
    localG >= 2.576 ~ "High cluster",
    localG <= -2.576 ~ "Low cluster",
    TRUE ~ "Not significant"
  ))

#Graph it red is hold high clusters of age, blue is young clusters
ggplot(tnj_inc_tract) + 
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) + 
  scale_fill_manual(values = c("red", "blue", "grey")) + 
  theme_void()


####LISA version
#set seed so its recreatable with random Z scores
set.seed(1983)

#scales estiamtes of age into Z scores
tnj_inc_tract$scaled_estimate <- as.numeric(scale(tnj_inc_tract$estimate))

#Two sided tail test with a local moran figure
#Finds LISA numbers
tnj_lisa <- localmoran_perm(
  tnj_inc_tract$scaled_estimate, 
  weights, 
  nsim = 999L, 
  alternative = "two.sided"
) %>%
  as_tibble() %>%
  set_names(c("local_i", "exp_i", "var_i", "z_i", "p_i",
              "p_i_sim", "pi_sim_folded", "skewness", "kurtosis"))

#stick the LISA number on the data frame
tnj_lisa_df <- tnj_inc_tract %>%
  select(GEOID, scaled_estimate) %>%
  mutate(lagged_estimate = lag.listw(weights, scaled_estimate)) %>%
  bind_cols(tnj_lisa)

#Now this does quadrants
tnj_lisa_clusters <- tnj_lisa_df %>%
  mutate(lisa_cluster = case_when(
    p_i >= 0.05 ~ "Not significant",
    scaled_estimate > 0 & local_i > 0 ~ "High-high",
    scaled_estimate > 0 & local_i < 0 ~ "High-low",
    scaled_estimate < 0 & local_i > 0 ~ "Low-low",
    scaled_estimate < 0 & local_i < 0 ~ "Low-high"
  ))
#color them differently
color_values <- c(`High-high` = "red", 
                  `High-low` = "pink", 
                  `Low-low` = "blue", 
                  `Low-high` = "lightblue", 
                  `Not significant` = "white")
#This makes a scatter plot with the scaled and lagged estimates
ggplot(tnj_lisa_clusters, aes(x = scaled_estimate, 
                              y = lagged_estimate,
                              fill = lisa_cluster)) + 
  geom_point(color = "black", shape = 21, size = 2) + 
  theme_minimal() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_fill_manual(values = color_values) + 
  labs(x = "Median age (z-score)",
       y = "Spatial lag of median age (z-score)",
       fill = "Cluster type")

ggplot(tnj_lisa_clusters, aes(fill = lisa_cluster)) + 
  geom_sf(size = 0.1) + 
  theme_void() + 
  scale_fill_manual(values = color_values) + 
  labs(fill = "Cluster type")



