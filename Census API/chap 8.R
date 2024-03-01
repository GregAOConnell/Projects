#Chapter 8
install.packages("segregation")
library(segregation)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(units)
install.packages("car")
library(car)

###############################Segregation, Dissimilarity Indexing#######################################
# Get California tract data by race/ethnicity
ca_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ), 
  state = "CA",
  geometry = TRUE,
  year = 2019
) 

# Use tidycensus to get urbanized areas by population with geometry, 
# then filter for those that have populations of 750,000 or more
us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) %>%
  filter(estimate >= 750000) %>%
  transmute(urban_name = str_remove(NAME, 
                                    fixed(", CA Urbanized Area (2010)")))

# Compute an inner spatial join between the California tracts and the 
# urbanized areas, returning tracts in the largest California urban 
# areas with the urban_name column appended
ca_urban_data <- ca_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry()

ca_urban_data %>%
  filter(variable %in% c("white", "hispanic"),
         urban_name == "San Francisco--Oakland") %>%
  dissimilarity(
    group = "variable",
    unit = "GEOID",
    weight = "estimate"
  )
ca_urban_data %>%
  filter(variable %in% c("white", "hispanic")) %>%
  group_by(urban_name) %>%
  group_modify(~
                 dissimilarity(.x,
                               group = "variable",
                               unit = "GEOID",
                               weight = "estimate"
                 )
  ) %>% 
  arrange(desc(est))


mutual_within(
  data = ca_urban_data,
  group = "variable",
  unit = "GEOID",
  weight = "estimate",
  within = "urban_name",
  wide = TRUE
)
#more localized data, just LA
la_local_seg <- ca_urban_data %>%
  filter(urban_name == "Los Angeles--Long Beach--Anaheim") %>%
  mutual_local(
    group = "variable",
    unit = "GEOID",
    weight = "estimate", 
    wide = TRUE
  )

#seperating the seg vars by tract
la_tracts_seg <- tracts("CA", cb = TRUE, year = 2019) %>%
  inner_join(la_local_seg, by = "GEOID") 
#plot them all
la_tracts_seg %>%
  ggplot(aes(fill = ls)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26946) + 
  scale_fill_viridis_c(option = "inferno") + 
  theme_void() + 
  labs(fill = "Local\nsegregation index")



la_entropy <- ca_urban_data %>%
  filter(urban_name == "Los Angeles--Long Beach--Anaheim") %>%
  group_by(GEOID) %>%
  group_modify(~data.frame(entropy = entropy(
    data = .x,
    group = "variable",
    weight = "estimate",
    base = 4)))

la_entropy_geo <- tracts("CA", cb = TRUE, year = 2019) %>%
  inner_join(la_entropy, by = "GEOID")

library(mapboxapi)

la_city_hall <- mb_geocode("City Hall, Los Angeles CA")

minutes_to_downtown <- mb_matrix(la_entropy_geo, la_city_hall)

la_entropy_geo$minutes <- as.numeric(minutes_to_downtown)

ggplot(la_entropy_geo, aes(x = minutes_to_downtown, y = entropy)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess") + 
  theme_minimal() + 
  scale_x_continuous(limits = c(0, 80)) + 
  labs(title = "Diversity gradient, Los Angeles urbanized area",
       x = "Travel-time to downtown Los Angeles in minutes, Census tracts",
       y = "Entropy index")

#############################Exercise 1#####################################
#nj data race and ethnicity
nj_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ), 
  state = "NJ",
  geometry = TRUE,
  year = 2019
) 


# Use tidycensus to get urbanized areas by population with geometry, 
# then filter for those that have populations of 750,000 or more
us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) %>%
  filter(estimate >= 750000) %>%
  transmute(urban_name = str_remove(NAME, 
                                    fixed(", Urbanized Area (2010)")))
nj_urban_data <- nj_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry()

nj_urban_data %>%
  filter(variable %in% c("white", "hispanic"),
         urban_name == "New York--Newark, NY--NJ--CT Urbanized Area (2010)") %>%
  dissimilarity(
    group = "variable",
    unit = "GEOID",
    weight = "estimate"
  )



####################################Regression Modeling################################


library(sf)
#counties in dallas fort worth
dfw_counties <- c("Collin County", "Dallas", "Denton", 
                  "Ellis", "Hunt", "Kaufman", "Rockwall", 
                  "Johnson", "Parker", "Tarrant", "Wise")
##finding our vars of interest
variables_to_get <- c(
  median_value = "B25077_001",
  median_rooms = "B25018_001",
  median_income = "DP03_0062",
  total_population = "B01003_001",
  median_age = "B01002_001",
  pct_college = "DP02_0068P",
  pct_foreign_born = "DP02_0094P",
  pct_white = "DP05_0077P",
  median_year_built = "B25037_001",
  percent_ooh = "DP04_0046P"
)

dfw_data <- get_acs(
  geography = "tract",
  variables = variables_to_get,
  state = "TX",
  county = dfw_counties,
  geometry = TRUE,
  output = "wide",
  year = 2020
) %>%
  select(-NAME) %>%
  st_transform(32138) # NAD83 / Texas North Central

install.packages("patchwork")
library(patchwork)
#This plots the median home value stats
mhv_map <- ggplot(dfw_data, aes(fill = median_valueE)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(labels = scales::label_dollar()) + 
  theme_void() + 
  labs(fill = "Median home value ")


#This makes a histogram of that median home value
mhv_histogram <- ggplot(dfw_data, aes(x = median_valueE)) + 
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy",
                 bins = 100) + 
  theme_minimal() + 
  scale_x_continuous(labels = scales::label_number_si(accuracy = 0.1)) + 
  labs(x = "Median home value")

#displays them side by side
mhv_map + mhv_histogram

##Huge outliers in the regular distrivution, so lets log it to make it more normal
#same plot but logged
mhv_map_log <- ggplot(dfw_data, aes(fill = log(median_valueE))) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c() + 
  theme_void() + 
  labs(fill = "Median home\nvalue (log)")
#same plot but logged
mhv_histogram_log <- ggplot(dfw_data, aes(x = log(median_valueE))) + 
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy",
                 bins = 100) + 
  theme_minimal() + 
  scale_x_continuous() + 
  labs(x = "Median home value (log)")
#now we have a nice logged distribution
#its more normal
mhv_map_log + mhv_histogram_log




########################Transforming Variables through feature engineering####################

#Making a population density variable and making median structure age which gets
#median age in population tracts
dfw_data_for_model <- dfw_data %>%
  mutate(pop_density = as.numeric(set_units(total_populationE / st_area(.), "1/km2")),
         median_structure_age = 2018 - median_year_builtE) %>%
  select(!ends_with("M")) %>% 
  rename_with(.fn = ~str_remove(.x, "E$")) %>%
  na.omit()

#lets regress it
formula <- "log(median_value) ~ median_rooms + median_income + pct_college + pct_foreign_born + pct_white + median_age + median_structure_age + percent_ooh + pop_density + total_population"

model1 <- lm(formula = formula, data = dfw_data_for_model)

summary(model1)


##Test for collinearity in the model
install.packages("corrr")
library(corrr)

dfw_estimates <- dfw_data_for_model %>%
  select(-GEOID, -median_value, -median_year_built) %>%
  st_drop_geometry()

#here is a straightforward correlation matrix
correlations <- correlate(dfw_estimates, method = "pearson")
network_plot(correlations)

#we see predictors are correlated in some regard
#dig in further with the vif

vif(model1)

#new formula removing biggest beta, median income
formula2 <- "log(median_value) ~ median_rooms + pct_college + pct_foreign_born + pct_white + median_age + median_structure_age + percent_ooh + pop_density + total_population"

model2 <- lm(formula = formula2, data = dfw_data_for_model)

summary(model2)

vif(model2)


#if you cant take variables out of your model you can change them with dimension reduction
pca <- prcomp(
  formula = ~., 
  data = dfw_estimates, 
  scale. = TRUE, 
  center = TRUE
)
#all 10 components explain 100% of the variation, yay!
#this gives proportion of the variance of each

summary(pca)

#lets dig deeper into this
#large balues are most important, and not negative or positive direction

pca_tibble <- pca$rotation %>%
  as_tibble(rownames = "predictor")

pca_tibble %>%
  select(predictor:PC5) %>%
  pivot_longer(PC1:PC5, names_to = "component", values_to = "value") %>%
  ggplot(aes(x = value, y = predictor)) + 
  geom_col(fill = "darkgreen", color = "darkgreen", alpha = 0.5) + 
  facet_wrap(~component, nrow = 1) + 
  labs(y = NULL, x = "Value") + 
  theme_minimal()


########################Spatial Regression#########################

#Residuals on regression should be normally distributed like this
dfw_data_for_model$residuals <- residuals(model2)

ggplot(dfw_data_for_model, aes(x = residuals)) + 
  geom_histogram(bins = 100, alpha = 0.5, color = "navy",
                 fill = "navy") + 
  theme_minimal()

library(spdep)
#This gets tricky for spatial models because there is usually an auto correlated error term
#we can assess that with the Moran value

wts <- dfw_data_for_model %>%
  poly2nb() %>%
  nb2listw()

moran.test(dfw_data_for_model$residuals, wts)
#our value is modest, postitive and statistically significant
# we can visualize that
dfw_data_for_model$lagged_residuals <- lag.listw(wts, dfw_data_for_model$residuals)

ggplot(dfw_data_for_model, aes(x = residuals, y = lagged_residuals)) + 
  theme_minimal() + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red")
