
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
                 width = NULL),

    selectInput("colour_scheme", "Select Colour Scheme", row.names(RColorBrewer::brewer.pal.info), selected = "RdYlGn")
    
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
                       ),
    
    checkboxGroupInput("inner_outer_london",
                       "Inner or Outer London",
                       c("Inner", "Outer"),
                       selected =  c("Inner", "Outer"),
                       inline = TRUE
                      ),
    
    checkboxInput("select_local_authority", 
                  "Show Local Authority Options"),
  conditionalPanel(
  condition = "input.select_local_authority == true",
  selectInput("local_authority_area", 
                "Select Local Authority",
                c("Barking and Dagenham",
                  "Barnet",
                  "Bexley",
                  "Brent",
                  "Bromley",
                  "Camden",
                  "City of London",
                  "Croydon",
                  "Ealing",
                  "Enfield",
                  "Greenwich",
                  "Hackney",
                  "Hammersmith and Fulham",
                  "Haringey",
                  "Harrow",
                  "Havering",
                  "Hillingdon",
                  "Hounslow",
                  "Islington",
                  "Kensington and Chelsea",
                  "Kingston upon Thames",
                  "Lambeth",
                  "Lewisham",
                  "Merton",
                  "Newham",
                  "Redbridge",
                  "Richmond upon Thames",
                  "Southwark",
                  "Sutton",
                  "Tower Hamlets",
                  "Waltham Forest",
                  "Wandsworth",
                  "Westminster"),
                  selected = c("Barking and Dagenham",
                  "Barnet",
                  "Bexley",
                  "Brent",
                  "Bromley",
                  "Camden",
                  "City of London",
                  "Croydon",
                  "Ealing",
                  "Enfield",
                  "Greenwich",
                  "Hackney",
                  "Hammersmith and Fulham",
                  "Haringey",
                  "Harrow",
                  "Havering",
                  "Hillingdon",
                  "Hounslow",
                  "Islington",
                  "Kensington and Chelsea",
                  "Kingston upon Thames",
                  "Lambeth",
                  "Lewisham",
                  "Merton",
                  "Newham",
                  "Redbridge",
                  "Richmond upon Thames",
                  "Southwark",
                  "Sutton",
                  "Tower Hamlets",
                  "Waltham Forest",
                  "Wandsworth",
                  "Westminster"),
                multiple = TRUE)
)
    
    #checkboxInput("travel_to_bank", "Include Travel Time to Bank Station", value = FALSE, width = NULL)
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
    
    withMathJax(includeMarkdown("housing.Rmd"))
    
))
