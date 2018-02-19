# server logic of this web app: https://evanodell.shinyapps.io/london-housing-bang-buck/

library(shiny)
library(shinyjs)
library(readr)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(scales)

perc.rank <- function(x) ifelse(is.na(x), NA, rank(x)/sum(!is.na(x)))

shinyServer(function(input, output, session) {
  
  london_shape <- read_rds("data/london_shape.rds")
  
  london_data <- read_rds("data/london_data.rds")
  
  london_data_subset <- reactive({
    
    #london_data <- london_data[c("geography_code", "ptal_score", "ptal_level", "price", "property_type", "fare_zone", "predicted_rooms", "inner_outer")]
    
    london_data[london_data$property_type %in% input$property_type 
              & london_data$fare_zone %in% input$fare_zone 
              & london_data$ptal_level %in% input$ptal_level 
              & london_data$price <= input$max_price
              & london_data$inner_outer %in% input$inner_outer_london
              & london_data$LAD11NM %in% input$local_authority_area, ]
    
  })
  
  ## Up next: Input to select variables that matter 
  # - Crime 
  # - Housing 
  # - Pollution 
  # - Schools 
  # - Age, etc
  
  ## Function to render map
  output$map <- renderLeaflet({
    
    london_map <- left_join(london_shape, london_data_subset())
    
    london_map$price_per_room <- london_map$price/london_map$predicted_rooms
    
    london_map$bang_for_buck <- as.numeric(perc.rank(((1/perc.rank(london_map$price_per_room)) *
                                                        perc.rank(london_map$ptal_score)) / 
                                                       (rescale(london_map$price, to = c(-1, 0), na.rm = TRUE) * -1)))
      
    pal <- colorNumeric(input$colour_scheme, domain = as.numeric(london_map$bang_for_buck))
    
    bang_buck_labels <- paste0("</strong>Location: ", london_map$LSOA11NM, "</strong></br>",
                               "Average Price: £", prettyNum(round(as.numeric(london_map$price),2), big.mark = ","), "</br>",
                               "Estimated Average Price Per Room: £", prettyNum(round(as.numeric(london_map$price_per_room),2), big.mark = ","), "</br>",
                               "Number of Sales: ", london_map$number_sales, "</br>",
                               "PTAL Score: ", london_map$ptal_score, "</br>",
                               "PTAL Level: ", london_map$ptal_level, "</br>",
                               "Travel Zone: ", london_map$full_fare_zone, "</br>",
                               "Bang for Buck: ", round(as.numeric(london_map$bang_for_buck), 2)) %>% lapply(htmltools::HTML)
    
    map_of_london <- leaflet(london_map, options = leafletOptions(minZoom = 10)) %>% 
      addPolygons(color = "grey",
                  weight = 0.4,
                  opacity = 0.5,
                  fillOpacity = 1,
                  fillColor = ~pal(as.numeric(bang_for_buck)),
                  label = bang_buck_labels,
                  highlight = highlightOptions(weight = 2,
                                               color = "#e0e0e0",
                                               dashArray = "",
                                               fillOpacity = 0.4,
                                               bringToFront = TRUE)) %>% 
      setMaxBounds(lng1 = -0.52,
                   lat1 = 51.70,
                   lng2 = 0.34,
                   lat2 = 51.28) %>%
      addLegend("topright",
                pal = pal,
                values = ~as.numeric(bang_for_buck),
                title = paste0("Bank for Buck Score"),
                opacity = 1)
    
    return(map_of_london)
    
  })
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

})