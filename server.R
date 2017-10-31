#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(readr)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(scales)

#perc.rank <- function(x) trunc(rank(x, na.last=NA))/length(x[!is.na(x)])

perc.rank <- function(x) ifelse(is.na(x),NA,rank(x)/sum(!is.na(x)))

# standardise <- function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
# 
# standardise2 <- function(x, ...) {(x - mean(x, ...)) / sd(x, ...)}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  london_shape <- read_rds("data/london_shape.rds")

  london_data <- read_rds("data/london_data.rds")
  
  london_data_subset <- reactive({
    
        london_data[london_data$property_type == input$property_type
                  & london_data$fare_zone %in% input$fare_zone
                  & london_data$ptal_level %in% input$ptal_level
                  & london_data$price <= input$max_price,]
  
   })
  
#   zoom <- 8
  
  ## Input to select variables that matter [NEED TO GET MORE OF THESE VARIABLES!]
  # - Crime
  # - Housing
  # - Pollution
  # - Schools
  # - Age, etc
  
  ## Function to render map
  
  # output$text <- renderText({
  #   
  #   london_map <- full_join(london_shape, london_data_subset())
  #   
  #   print(summary(london_map$price))
  #   
  # })
  

  output$map <- renderLeaflet({
  
    london_map <- left_join(london_shape, london_data_subset())
    
    london_map$price_per_room <- london_map$price/london_map$predicted_rooms
    
    #london_map$price_per_room[is.na(london_map$price)] <- NA

    london_map$bang_for_buck <- as.numeric(perc.rank(
      ((rescale(london_map$price_per_room, to=c(-1,1), na.rm=TRUE)*-1)*
        rescale(london_map$ptal_score, to=c(0,1), na.rm=TRUE))*
        (rescale(london_map$price, to=c(0,1), na.rm=TRUE)*-1)))
    
    #london_map$bang_for_buck[is.na(london_map$price)] <- NA
    
    pal <-  colorNumeric("RdYlGn", domain=as.numeric(london_map$bang_for_buck))
    
    # bounds <- sf::st_bbox(london_shape_subset())
    # lat <- mean(bounds[1],bounds[3])
    # lng <- mean(bounds[2],bounds[4]) 
    # zoom <- 8
 
     bang_buck_labels <-  paste0(
       "</strong>Location: ", london_map$LSOA11NM, "</strong></br>",
       "Average Price: £", prettyNum(round(as.numeric(london_map$price)), big.mark = ","), "</br>",
       "Average Price Per Room: £",  prettyNum(round(as.numeric(london_map$price_per_room)), big.mark = ","), "</br>",
       "PTAL Score: ", london_map$ptal_score, "</br>",
       "PTAL Level: ", london_map$ptal_level, "</br>",
       "Travel Zone: ", london_map$full_fare_zone, "</br>",
       "Bang for Buck: ", round(as.numeric(london_map$bang_for_buck), 2)) %>% lapply(htmltools::HTML)
    
    map_of_london <- leaflet(london_map) %>%
      #setView(lat=mean(bounds[1],bounds[3]), lng=mean(bounds[2],bounds[4]), zoom=zoom) %>%
      addPolygons(
        color = "grey",
        weight = 0.4,
        opacity = 0.5,
        fillOpacity = 1,
        fillColor = ~pal(as.numeric(bang_for_buck)),
        label=bang_buck_labels,
        highlight = highlightOptions(
          weight = 2,
          color = "#e0e0e0",
          dashArray = "",
          fillOpacity = 0.4,
          bringToFront = TRUE))  %>% 
      addLegend("topright", pal = pal, values = ~as.numeric(bang_for_buck),
                title = paste0("Bank for Buck (", input$property_type,")"),
                opacity = 1)
    
    return(map_of_london)
    
  })
  

   # observe({
   #   input$reset_button
   #   leafletProxy("map") %>% setView(lat = lat, lng = lng, zoom = zoom)
   # })     
  
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
  #bang_buck_map
  
  
})

