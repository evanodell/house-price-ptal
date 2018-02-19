pacman::p_load(readr)
pacman::p_load(tibble)

hpi <- read_csv("csv_data/UK-HPI-full-file-2017-09.csv", 
                                     col_types = cols(Date = col_date(format = "%d/%m/%Y")))
glimpse(hpi)


x <- hpi[hpi$RegionName=="London",]





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