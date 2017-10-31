
pacman::p_load(readr)
pacman::p_load(sf)
pacman::p_load(dplyr)
pacman::p_load(readxl)
pacman::p_load(ggplot2)
pacman::p_load(scales)
pacman::p_load(magrittr)
pacman::p_load(Cairo)
pacman::p_load(rgdal)


#### Match average number of rooms data from file 4

lsoa_population <- read_csv("/csv_data/lsoa/KS101EW.csv") #KS101EW - population

lsoa_population <- rename(lsoa_population, 
                          geography_code = `geography code`,
                          rural_urban = `Rural Urban`,
                          all_residents = `Variable: All usual residents; measures: Value`,
                          male = `Variable: Males; measures: Value`,
                          female = `Variable: Females; measures: Value`,
                          lives_in_household = `Variable: Lives in a household; measures: Value`,
                          lives_in_communal = `Variable: Lives in a communal establishment; measures: Value`,
                          schoolchild = `Variable: Schoolchild or full-time student aged 4 and over at their non term-time address; measures: Value`,
                          hectares = `Variable: Area (Hectares); measures: Value`,
                          density = `Variable: Density (number of persons per hectare); measures: Value`)

lsoa_household_comp <- read_csv("/csv_data/lsoa/KS105EW.csv") #    KS105EW - household composition

lsoa_household_comp <- rename(lsoa_household_comp, 
                              geography_code = `geography code`,
                              rural_urban = `Rural Urban`,
                              all_categories=`Household Composition: All categories: Household composition; measures: Value`,
                              one_person=`Household Composition: One person household; measures: Value`,
                              one_person_65_plus=`Household Composition: One person household: Aged 65 and over; measures: Value`,
                              one_person_other=`Household Composition: One person household: Other; measures: Value`,
                              one_family=`Household Composition: One family household; measures: Value`,
                              one_family_all_65_plus=`Household Composition: One family only: All aged 65 and over; measures: Value`,
                              one_family_married=`Household Composition: One family only: Married or same-sex civil partnership couple; measures: Value`,
                              one_family_married_no_children=`Household Composition: One family only: Married or same-sex civil partnership couple: No children; measures: Value`,
                              one_family_married_with_dependent=`Household Composition: One family only: Married or same-sex civil partnership couple: Dependent children; measures: Value`,
                              one_family_married_non_dependent=`Household Composition: One family only: Married or same-sex civil partnership couple: All children non-dependent; measures: Value`,
                              one_family_couple=`Household Composition: One family only: Cohabiting couple; measures: Value`,
                              one_family_couple_no_children=`Household Composition: One family only: Cohabiting couple: No children; measures: Value`,
                              one_family_couple_with_dependent=`Household Composition: One family only: Cohabiting couple: Dependent children; measures: Value`,
                              one_family_couple_non_dependent=`Household Composition: One family only: Cohabiting couple: All children non-dependent; measures: Value`,
                              one_family_lone_parent=`Household Composition: One family only: Lone parent; measures: Value`,
                              one_family_lone_parent_with_dependent=`Household Composition: One family only: Lone parent: Dependent children; measures: Value`,
                              one_family_lone_parent_non_dependent=`Household Composition: One family only: Lone parent: All children non-dependent; measures: Value`,
                              other_type=`Household Composition: Other household types; measures: Value`,
                              other_type_with_dependent=`Household Composition: Other household types: With dependent children; measures: Value`,
                              other_type_students=`Household Composition: Other household types: All full-time students; measures: Value`,
                              other_type_all_65_plus=`Household Composition: Other household types: All aged 65 and over; measures: Value`,
                              other_type_other=`Household Composition: Other household types: Other; measures: Value`)

lsoa_dwellings <- read_csv("/csv_data/lsoa/KS401EW.csv")# KS401EW - dwellings

lsoa_dwellings <- rename(lsoa_dwellings, 
                         geography_code = `geography code`,
                         rural_urban = `Rural Urban`,
                         all_dwellings=`Dwelling Type: All categories: Dwelling type; measures: Value`,
                         unshared_dwelling=`Dwelling Type: Unshared dwelling; measures: Value`,
                         shared_dwelling_two_households=`Dwelling Type: Shared dwelling: Two household spaces; measures: Value`,
                         shared_dwelling_three_plus_households=`Dwelling Type: Shared dwelling: Three or more household spaces; measures: Value`,
                         household_spaces=`Dwelling Type: All categories: Household spaces; measures: Value`,
                         household_spaces_with_usual_residents=`Dwelling Type: Household spaces with at least one usual resident; measures: Value`,
                         household_spaces_no_usual_residents=`Dwelling Type: Household spaces with no usual residents; measures: Value`,
                         detached=`Dwelling Type: Whole house or bungalow: Detached; measures: Value`,
                         semi_detached=`Dwelling Type: Whole house or bungalow: Semi-detached; measures: Value`,
                         terraced=`Dwelling Type: Whole house or bungalow: Terraced (including end-terrace); measures: Value`,
                         flat_purpose_built=`Dwelling Type: Flat, maisonette or apartment: Purpose-built block of flats or tenement; measures: Value`,
                         flat_converted=`Dwelling Type: Flat, maisonette or apartment: Part of a converted or shared house (including bed-sits); measures: Value`,
                         flat_commercial=`Dwelling Type: Flat, maisonette or apartment: In a commercial building; measures: Value`,
                         caravan=`Dwelling Type: Caravan or other mobile or temporary structure; measures: Value`)


lsoa_rooms <- read_csv("/csv_data/lsoa/KS403EW.csv")#     KS403EW - rooms, bedrooms, central heating

lsoa_rooms <- rename(lsoa_rooms, 
                     geography_code = `geography code`,
                     rural_urban = `Rural Urban`,
                     heating_all = `Central Heating: All categories: Type of central heating in household; measures: Value`,
                     no_central_heating = `Central Heating: Does not have central heating; measures: Value`,
                     central_heating = `Central Heating: Does have central heating; measures: Value`,
                     occupancy_rooms = `Central Heating: Occupancy rating (rooms) of -1 or less; measures: Value`,
                     occupancy_bedrooms = `Central Heating: Occupancy rating (bedrooms) of -1 or less; measures: Value`,
                     average_household_size = `Central Heating: Average household size; measures: Value`,
                     average_rooms_per_household = `Central Heating: Average number of rooms per household; measures: Value`,
                     average_bedrooms_per_household = `Central Heating: Average number of bedrooms per household; measures: Value`)




lsoa_crime <- read_csv("/csv_data/MPS_LSOA_Level_Crime.csv")

lsoa_crime <- rename(lsoa_crime,
                     geography_code = `LSOA Code`,
                     crime_category = `Major Category`)

lsoa_crime$total_crimes <- rowSums(lsoa_crime[c(5:28)])

lsoa_crime <- lsoa_crime %>%
  group_by(geography_code) %>%
      summarise(total_crimes=sum(total_crimes))

glimpse(lsoa_crime)

lsoa_data <- lsoa_dwellings %>% 
  left_join(lsoa_household_comp) %>% 
  left_join(lsoa_population) %>% 
  left_join(lsoa_rooms) %>% 
  left_join(lsoa_crime)

glimpse(lsoa_data)
 
#lsoa_data$crime_category <- as.factor(lsoa_data$crime_category)

lsoa_data$crimes_per_person <- lsoa_data$total_crimes/lsoa_data$all_residents

lsoa_data$flats <- lsoa_data$flat_commercial + lsoa_data$flat_converted + lsoa_data$flat_purpose_built

lsoa_data$detached_percent <- lsoa_data$detached/lsoa_data$household_spaces

lsoa_data$semi_detached_percent <- lsoa_data$semi_detached/lsoa_data$household_spaces

lsoa_data$terraced_percent <- lsoa_data$terraced/lsoa_data$household_spaces

lsoa_data$flats_percent <- lsoa_data$flats/lsoa_data$household_spaces

lsoa_2011 <- read_csv("/csv_data/London_postcode-ONS-postcode-Directory-May15.csv")

glimpse(lsoa_2011)

#lsoa_2011 <- lsoa_2011[c("oa11", "lsoa11")]

lsoa_2011 <- rename(lsoa_2011, 
                    geography_code = lsoa11,
                    postcode = pcd)

lsoa_2011$postcode <- gsub(" ", "", lsoa_2011$postcode)

fare_zone <- read_csv("/csv_data/MyLondon_fare_zone_OA.csv") # from here: https://data.london.gov.uk/dataset/mylondon

fare_zone <- rename(fare_zone,
                    fare_zone = Fare_Zone,
                    oa11=OA11CD)

travel_to_bank <- read_csv("/csv_data/MyLondon_traveltime_to_Bank_station_OA.csv")

travel_to_bank <- rename(travel_to_bank, 
                         oa11=OA11CD)

lsoa_2011 <- lsoa_2011 %>% left_join(fare_zone) %>% left_join(travel_to_bank)

glimpse(lsoa_2011)

pp_london_complete <- read_csv("/csv_data/pp-london-complete-2012-2017.csv")

pp_london_complete$postcode <- gsub(" ", "", pp_london_complete$postcode)

pp_london_complete$property_type <- as.factor(pp_london_complete$property_type)

pp_london_complete <- pp_london_complete[is.na(pp_london_complete$property_type)==FALSE
                                         & pp_london_complete$property_type!="O",]

pp_london_join <- pp_london_complete %>% left_join(lsoa_2011)

glimpse(pp_london_join)

pp_london_group <- pp_london_join %>%
  group_by(geography_code, property_type) %>%
  summarise(price = mean(price),
            number_sales = n_distinct(transaction),
            public_transport_time_mins = mean(public_transport_time_mins),
            fare_zone = round(mean(fare_zone)))

glimpse(pp_london_group)

tpal_lsoa2011 <- read_excel("LSOA2011 AvPTAI2015.xlsx")

names(tpal_lsoa2011)[names(tpal_lsoa2011)=="LSOA2011"] <- "geography_code"

names(tpal_lsoa2011)[names(tpal_lsoa2011)=="AvPTAI2015"] <- "ptal_score"

names(tpal_lsoa2011)[names(tpal_lsoa2011)=="PTAL"] <- "ptal_level"

glimpse(tpal_lsoa2011)

pp_london_2012_2017 <- left_join(tpal_lsoa2011, pp_london_group)

glimpse(pp_london_2012_2017)

pp_london_2012_2017$geography_code <- as.factor(pp_london_2012_2017$geography_code)

x <- pp_london_lsoa %>% 
  group_by(geography_code) %>%
  summarise(no_rows = length(geography_code))

lsoa_data$date <- NULL

lsoa_data$rural_urban <- NULL

pp_london_lsoa <- left_join(pp_london_2012_2017, lsoa_data)

pp_london_lsoa$property_type <- recode(pp_london_lsoa$property_type,
                                       "S" = "Semi-detached",
                                       "T" = "Terraced",
                                       "D" = "Detached",
                                       "F" = "Flat",
                                       "O" = "Other")
pp_london_lsoa$property_type <- factor(pp_london_lsoa$property_type, levels=c("Detached", "Semi-detached", "Terraced", "Flat", "Other"))

glimpse(pp_london_lsoa)

london_shape <- readOGR("london-boundary/London_lsoa.shp", layer = "london_lsoa")
proj4string(london_shape) <- CRS("+init=epsg:27700")
london_shape <- spTransform(london_shape, CRS("+init=epsg:4326"))

london_shape <- sf::st_as_sf(london_shape)

st_crs(london_shape)

#london_shape

names(london_shape)[names(london_shape)=="LSOA11CD"] <- "geography_code"

london_shape$geography_code <- as.character(london_shape$geography_code)

# london_shape$inner_outer <- ifelse(london_shape$LAD11CD %in% list("E09000001",
#                                                   "E09000007",
#                                                   "E09000011",
#                                                   "E09000012",
#                                                   "E09000013",
#                                                   "E09000019",
#                                                   "E09000020",
#                                                   "E09000022",
#                                                   "E09000023",
#                                                   "E09000028",
#                                                   "E09000030",
#                                                   "E09000032",
#                                                   "E09000033"), "Inner", "Outer")

travel <- lsoa_2011[c("geography_code", "fare_zone")]

travel <- travel %>% group_by(geography_code) %>% summarise(full_fare_zone=mean(fare_zone))

london_shape <- full_join(london_shape, travel)

write_rds(london_shape, "london_shape.rds")

test_map <- left_join(london_shape, pp_london_lsoa)

### Predicting average number of rooms-----------------------

pp_london_lsoa$predicted_rooms <- NA

lm_detached <- lm(average_rooms_per_household ~ detached_percent , data=subset(pp_london_lsoa,property_type=="Detached"))

pp_london_lsoa$predicted_rooms[pp_london_lsoa$property_type=="Detached"] <- predict(lm_detached, data=subset(pp_london_lsoa,property_type=="Detached"))

lm_semi_detached<- lm(average_rooms_per_household ~ semi_detached_percent, data=pp_london_lsoa)

pp_london_lsoa$predicted_rooms[pp_london_lsoa$property_type=="Semi-detached"] <- predict(lm_semi_detached, data=subset(pp_london_lsoa,property_type=="Semi-detached"))

lm_terraced <- lm(average_rooms_per_household ~ terraced_percent, data=subset(pp_london_lsoa,property_type=="Terraced"))

pp_london_lsoa$predicted_rooms[pp_london_lsoa$property_type=="Terraced"] <-  predict(lm_terraced, data=subset(pp_london_lsoa,property_type=="Terraced"))

lm_flats <- lm(average_rooms_per_household ~ flats_percent, data=subset(pp_london_lsoa,property_type=="Flat"))

pp_london_lsoa$predicted_rooms[pp_london_lsoa$property_type=="Flat"] <-  predict(lm_flats, data=subset(pp_london_lsoa,property_type=="Flat"))

glimpse(pp_london_lsoa)

pp_london_lsoa$property_type <- as.factor(as.character(pp_london_lsoa$property_type))


pp_london_lsoa$inner_outer <- ifelse(pp_london_lsoa$geography_code %in% list("E09000001",
                                                                  "E09000007",
                                                                  "E09000011",
                                                                  "E09000012",
                                                                  "E09000013",
                                                                  "E09000019",
                                                                  "E09000020",
                                                                  "E09000022",
                                                                  "E09000023",
                                                                  "E09000028",
                                                                  "E09000030",
                                                                  "E09000032",
                                                                  "E09000033"), "Inner", "Outer")

write_rds(pp_london_lsoa, "london_data.rds")

pp_london_detached_lsoa <- pp_london_lsoa[pp_london_lsoa$property_type=="Detached" & 
                                               is.na(pp_london_lsoa$property_type)==FALSE, ]

pp_london_semi_lsoa <- pp_london_lsoa[pp_london_lsoa$property_type=="Semi-detached" & 
                                           is.na(pp_london_lsoa$property_type)==FALSE, ]

pp_london_terraced_lsoa <- pp_london_lsoa[pp_london_lsoa$property_type=="Terraced" & 
                                               is.na(pp_london_lsoa$property_type)==FALSE, ]

pp_london_flats_lsoa <- pp_london_lsoa[pp_london_lsoa$property_type=="Flat" & 
                                           is.na(pp_london_lsoa$property_type)==FALSE, ]


# All London - Flats ------------------------------------------------------------

test_map <- left_join(london_shape, pp_london_lsoa)

test_map$fare_zone <- as.character(test_map$fare_zone)

write_rds(test_map, "london_map.rds")

london_shape_flats <- left_join(london_shape, pp_london_flats_lsoa)

london_shape_flats$price_per_room <- london_shape_flats$price/london_shape_flats$predicted_rooms_flats

london_shape_flats$bang_for_buck <- perc.rank(scale((london_shape_flats$price_per_room)*-1)*scale(london_shape_flats$ptal_score)) ## Need version of this that ignores NAs

london_shape_flats$bang_for_buck[is.na(london_shape_flats$price)] <- NA

london_shape_flats$number_sales[is.na(london_shape_flats$number_sales)] <- 0

#london_shape_flats$bang_for_buck <- rnorm(london_shape_flats$bang_for_buck) ## Normalised 

pal = colorNumeric("RdYlGn", london_shape_flats$bang_for_buck)

summary(london_shape_flats$bang_for_buck)

pacman::p_load(leaflet)

label_flats <- paste0(
  "</strong>Location: ", london_shape_flats$LSOA11NM, "</strong></br>",
  "Average Price: £", prettyNum(round(london_shape_flats$price), big.mark = ","), "</br>",
  "Average Price Per Room: £", prettyNum(round(london_shape_flats$price_per_room), big.mark = ","), "</br>",
  "Number of Sales 2012-2017: ", london_shape_flats$number_sales, "</br>",
  "PTAL Score: ", london_shape_flats$ptal_score, "</br>",
  "PTAL Level: ", london_shape_flats$ptal_level, "</br>",
  "Fare Zone: ", london_shape_flats$fare_zone, "</br>",
  "Bang for Buck: ", round(london_shape_flats$bang_for_buck, 2)
) %>% lapply(htmltools::HTML)

flats_bang_buck_map <- leaflet(options=leafletOptions(
  #dragging = FALSE, zoomControl = FALSE, tap = FALSE,
  #minZoom = 6, maxZoom = 6, maxBounds = list(list(2.5,-7.75), list(58.25,50.0)),
  attributionControl = FALSE),
  london_shape_flats) %>%
  addPolygons(
    color = "grey",
    weight=0.5,
    opacity = 0.5,
    fillOpacity = 1,
    fillColor = ~pal(bang_for_buck),
    label=label_flats) %>%
  addLegend("topright", pal = pal, values = ~bang_for_buck,
            title = "Bank for Buck (Flats)",
            opacity = 1)  %>% 
  htmlwidgets::onRender(
    "function(x, y) {
    var myMap = this;
    myMap._container.style['background'] = '#fff';
    }")

flats_bang_buck_map

# Inner London - Flats ------------------------------------------------------------

inner_london_shape_flats <- london_shape_flats[london_shape_flats$inner_outer=="Inner",]

inner_london_shape_flats$price_per_room <- inner_london_shape_flats$price/inner_london_shape_flats$predicted_rooms_flats

inner_london_shape_flats$bang_for_buck <- rnorm((scale(inner_london_shape_flats$price_per_room)*-1)*inner_london_shape_flats$ptal_score) ## Need better version of this

pal = colorNumeric("RdYlGn", inner_london_shape_flats$bang_for_buck)


summary(inner_london_shape_flats$bang_for_buck)

label_inner_flat <- paste0(
  "</strong>Location: ", inner_london_shape_flats$LSOA11NM, "</strong></br>",
  "Average Price: £", prettyNum(round(inner_london_shape_flats$price), big.mark = ","), "</br>",
  "Average Price Per Room: £",  prettyNum(round(inner_london_shape_flats$price_per_room), big.mark = ","), "</br>",
  "PTAL Score: ", inner_london_shape_flats$ptal_score, "</br>",
  "PTAL Level: ", inner_london_shape_flats$ptal_level, "</br>",
  "Bang for Buck: ", round(inner_london_shape_flats$bang_for_buck, 2)
) %>% lapply(htmltools::HTML)

flats_bang_buck_map_inner <- leaflet(options=leafletOptions(
  #dragging = FALSE, zoomControl = FALSE, tap = FALSE,
  #minZoom = 6, maxZoom = 6, maxBounds = list(list(2.5,-7.75), list(58.25,50.0)),
  attributionControl = FALSE),
  inner_london_shape_flats) %>%
  addPolygons(
    color = "grey",
    weight=0.5,
    opacity = 0.5,
    fillOpacity = 1,
    fillColor = ~pal(bang_for_buck),
    label=label_inner_flat) %>%
  addLegend("topright", pal = pal, values = ~bang_for_buck,
            title = "Bank for Buck",
            opacity = 1)  %>% 
  htmlwidgets::onRender(
    "function(x, y) {
    var myMap = this;
    myMap._container.style['background'] = '#fff';
    }")

flats_bang_buck_map_inner

# All London - terraced ------------------------------------------------------------

london_shape_terraced <- left_join(london_shape, pp_london_terraced_lsoa)

london_shape_terraced$price_per_room <- london_shape_terraced$price/london_shape_terraced$predicted_rooms_terraced

london_shape_terraced$bang_for_buck <- rnorm((scale(london_shape_terraced$price_per_room)*-1)*london_shape_terraced$ptal_score) ## Need version of this that ignores NAs

london_shape_terraced$bang_for_buck[is.na(london_shape_terraced$price)] <- NA

#london_shape_terraced$bang_for_buck <- rnorm(london_shape_terraced$bang_for_buck) ## Normalised 

pal = colorNumeric("RdYlGn", london_shape_terraced$bang_for_buck)

summary(london_shape_terraced$bang_for_buck)

pacman::p_load(leaflet)

label_terraced <- paste0(
  "</strong>Location: ", london_shape_terraced$LSOA11NM, "</strong></br>",
  "Average Price: £", prettyNum(round(london_shape_terraced$price), big.mark = ","), "</br>",
  "Average Price Per Room: £", prettyNum(round(london_shape_terraced$price_per_room), big.mark = ","), "</br>",
  "Number of Sales 2012-2017: ", london_shape_terraced$number_sales, "</br>",
  "PTAL Score: ", london_shape_terraced$ptal_score, "</br>",
  "PTAL Level: ", london_shape_terraced$ptal_level, "</br>",
  "Bang for Buck: ", round(london_shape_terraced$bang_for_buck, 2)
) %>% lapply(htmltools::HTML)

terraced_bang_buck_map <- leaflet(options=leafletOptions(
  #dragging = FALSE, zoomControl = FALSE, tap = FALSE,
  #minZoom = 6, maxZoom = 6, maxBounds = list(list(2.5,-7.75), list(58.25,50.0)),
  attributionControl = FALSE),
  london_shape_terraced) %>%
  addPolygons(
    color = "grey",
    weight=0.5,
    opacity = 0.5,
    fillOpacity = 1,
    fillColor = ~pal(bang_for_buck),
    label=label_terraced) %>%
  addLegend("topright", pal = pal, values = ~bang_for_buck,
            title = "Bank for Buck (Terraced)",
            opacity = 1)  %>% 
  htmlwidgets::onRender(
    "function(x, y) {
    var myMap = this;
    myMap._container.style['background'] = '#fff';
    }")

terraced_bang_buck_map

# All London - semi-detached -------------------------------------------------

london_shape_semi_detached <- left_join(london_shape, pp_london_semi_lsoa)

london_shape_semi_detached$price_per_room <- london_shape_semi_detached$price/london_shape_semi_detached$predicted_rooms_semi_detached

london_shape_semi_detached$bang_for_buck <- rnorm((scale(london_shape_semi_detached$price_per_room)*-1)*london_shape_semi_detached$ptal_score) ## Need version of this that ignores NAs

london_shape_semi_detached$bang_for_buck[is.na(london_shape_semi_detached$price)] <- NA

#london_shape_semi_detached$bang_for_buck <- rnorm(london_shape_semi_detached$bang_for_buck) ## Normalised 

pal = colorNumeric("RdYlGn", london_shape_semi_detached$bang_for_buck)

summary(london_shape_semi_detached$bang_for_buck)

pacman::p_load(leaflet)

label_semi_detached <- paste0(
  "</strong>Location: ", london_shape_semi_detached$LSOA11NM, "</strong></br>",
  "Average Price: £", prettyNum(round(london_shape_semi_detached$price), big.mark = ","), "</br>",
  "Average Price Per Room: £", prettyNum(round(london_shape_semi_detached$price_per_room), big.mark = ","), "</br>",
  "Number of Sales 2012-2017: ", london_shape_semi_detached$number_sales, "</br>",
  "PTAL Score: ", london_shape_semi_detached$ptal_score, "</br>",
  "PTAL Level: ", london_shape_semi_detached$ptal_level, "</br>",
  "Bang for Buck: ", round(london_shape_semi_detached$bang_for_buck, 2)
) %>% lapply(htmltools::HTML)

semi_detached_bang_buck_map <- leaflet(options=leafletOptions(
  #dragging = FALSE, zoomControl = FALSE, tap = FALSE,
  #minZoom = 6, maxZoom = 6, maxBounds = list(list(2.5,-7.75), list(58.25,50.0)),
  attributionControl = FALSE),
  london_shape_semi_detached) %>%
  addPolygons(
    color = "grey",
    weight=0.5,
    opacity = 0.5,
    fillOpacity = 1,
    fillColor = ~pal(bang_for_buck),
    label=label_semi_detached) %>%
  addLegend("topright", pal = pal, values = ~bang_for_buck,
            title = "Bank for Buck (Semi-detached)",
            opacity = 1)  %>% 
  htmlwidgets::onRender(
    "function(x, y) {
    var myMap = this;
    myMap._container.style['background'] = '#fff';
    }")

semi_detached_bang_buck_map


# All London - detached -------------------------------------------------

london_shape_detached <- left_join(london_shape, pp_london_detached_lsoa)

london_shape_detached$price_per_room <- london_shape_detached$price/london_shape_detached$predicted_rooms_detached

london_shape_detached$bang_for_buck <- rnorm((scale(london_shape_detached$price_per_room)*-1)*london_shape_detached$ptal_score) ## Need version of this that ignores NAs

london_shape_detached$bang_for_buck[is.na(london_shape_detached$price)] <- NA

#london_shape_detached$bang_for_buck <- rnorm(london_shape_detached$bang_for_buck) ## Normalised 

pal = colorNumeric("RdYlGn", london_shape_detached$bang_for_buck)

summary(london_shape_detached$bang_for_buck)

pacman::p_load(leaflet)

label_detached <- paste0(
  "</strong>Location: ", london_shape_detached$LSOA11NM, "</strong></br>",
  "Average Price: £", prettyNum(round(london_shape_detached$price), big.mark = ","), "</br>",
  "Average Price Per Room: £", prettyNum(round(london_shape_detached$price_per_room), big.mark = ","), "</br>",
  "Number of Sales 2012-2017: ", london_shape_detached$number_sales, "</br>",
  "PTAL Score: ", london_shape_detached$ptal_score, "</br>",
  "PTAL Level: ", london_shape_detached$ptal_level, "</br>",
  "Bang for Buck: ", round(london_shape_detached$bang_for_buck, 2)
) %>% lapply(htmltools::HTML)

detached_bang_buck_map <- leaflet(options=leafletOptions(
  #dragging = FALSE, zoomControl = FALSE, tap = FALSE,
  #minZoom = 6, maxZoom = 6, maxBounds = list(list(2.5,-7.75), list(58.25,50.0)),
  attributionControl = FALSE),
  london_shape_detached) %>%
  addPolygons(
    color = "grey",
    weight=0.5,
    opacity = 0.5,
    fillOpacity = 1,
    fillColor = ~pal(bang_for_buck),
    label=label_detached) %>%
  addLegend("topright", pal = pal, values = ~bang_for_buck,
            title = "Bank for Buck (Detached)",
            opacity = 1)  %>% 
  htmlwidgets::onRender(
    "function(x, y) {
    var myMap = this;
    myMap._container.style['background'] = '#fff';
    }")

detached_bang_buck_map
