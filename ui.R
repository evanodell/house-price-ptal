
library(shiny)
library(RColorBrewer)
library(shinyjs)
library(leaflet)


# CSS ----------------------------------------------------------------------
appCSS <- "#loading-content {
position: absolute;
background: #FFF;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #000000;
}"

shinyUI(
  fluidPage(
    useShinyjs(),
    inlineCSS(appCSS),
    titlePanel("Bang for Buck on London House Prices"),
    
    fluidRow(
    
    column(5, offset = 1,
    
           checkboxGroupInput("property_type", 
                "Property Type",
                c("Flat",
                  "Terraced",
                  "Semi-detached",
                  "Detached"),
                selected = "Flat"
              ), 
    
    numericInput("max_price", 
                 "Max Price (in £)", 
                 37000000, 
                 min = 14000, 
                 max = 37000000, 
                 step = NA,
                 width = NULL)
              ),
    
    column(5, 
    checkboxGroupInput("fare_zone", 
                       "Travel Zone",
                       c(1, 2, 3, 4, 5, 6),
                       selected = c(1, 2, 3, 4, 5, 6), 
                       inline = TRUE
                       ),
    
    checkboxGroupInput("ptal_level", 
                       "PTAL Level (Higher is more accessible)",
                       c("1a", "1b", "2", "3", "4", "5", "6a", "6b"),
                       selected =  c("1a", "1b", "2", "3", "4", "5", "6a", "6b"), 
                       inline = TRUE
                       )
    )),
    
    em(p("Note, the map is very big and can take a long time to load. Please be patient.")),
    
    div(## Loading intro animation
      id = "loading-content",

      h2("Loading...")
    ),

       # hidden(
       #   div(
       #     id = "app-content",
    leafletOutput("map", height = "600px"),
     #   )
     # ),
    #actionButton("reset_button", "Reset view")
    
    # 
    # selectInput("color_scheme", "Color Scheme",
    #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
    # )
    
    includeMarkdown("housing.Rmd")
    
))
