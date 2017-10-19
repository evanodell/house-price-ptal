
pacman::p_load(readr)
pacman::p_load(dplyr)
pacman::p_load(readxl)
pacman::p_load(ggplot2)
pacman::p_load(scales)
pacman::p_load(magrittr)
pacman::p_load(Cairo)

pp_london_complete <- read_csv("pp-london-complete.csv")

## Recoding pp_london_complete$property_type
pp_london_complete$property_type <- recode(pp_london_complete$property_type,
                                "S" = "Semi-detached",
                                "T" = "Terraced",
                                "D" = "Detached",
                                "F" = "Flat",
                                "O" = "Other")
pp_london_complete$property_type <- factor(pp_london_complete$property_type, levels=c("Detached", "Semi-detached", "Terraced", "Flat", "Other"))

## Recoding pp_london_complete$ownership_type into pp_london_complete$ownership_type
pp_london_complete$ownership_type <- recode(pp_london_complete$ownership_type,
                                 "F" = "Freehold",
                                 "L" = "Leasehold")
pp_london_complete$ownership_type <- factor(pp_london_complete$ownership_type, levels = c("Freehold", "Leasehold"))

## Recoding pp_london_complete$new_build into pp_london_complete$new_build
pp_london_complete$new_build <- recode(pp_london_complete$new_build,
                            "N" = FALSE,
                            "Y" = TRUE)

england_postcodes <- read_csv("England postcodes.csv")

names(england_postcodes)[names(england_postcodes)=="Postcode"] <- "postcode"
names(england_postcodes)[names(england_postcodes)=="Longitude"] <- "longitude"
names(england_postcodes)[names(england_postcodes)=="Grid Ref"] <- "grid_ref"
names(england_postcodes)[names(england_postcodes)=="Ward"] <- "ward"
names(england_postcodes)[names(england_postcodes)=="Country"] <- "country"
names(england_postcodes)[names(england_postcodes)=="Introduced"] <- "introduced"
names(england_postcodes)[names(england_postcodes)=="National Park"] <- "national_park"
names(england_postcodes)[names(england_postcodes)=="Built up area"] <- "built_up_area"
names(england_postcodes)[names(england_postcodes)=="Rural/urban"] <- "rural_urban"
names(england_postcodes)[names(england_postcodes)=="London zone"] <- "london_zone"
names(england_postcodes)[names(england_postcodes)=="MSOA Code"] <- "msoa_code"
names(england_postcodes)[names(england_postcodes)=="In Use?"] <- "in_use"
names(england_postcodes)[names(england_postcodes)=="Easting"] <- "easting"
names(england_postcodes)[names(england_postcodes)=="County"] <- "county"
names(england_postcodes)[names(england_postcodes)=="District Code"] <- "district_code"
names(england_postcodes)[names(england_postcodes)=="County Code"] <- "county_code"
names(england_postcodes)[names(england_postcodes)=="Terminated"] <- "terminated"
names(england_postcodes)[names(england_postcodes)=="Population"] <- "population"
names(england_postcodes)[names(england_postcodes)=="Built up sub-division"] <- "built_up_sub-division"
names(england_postcodes)[names(england_postcodes)=="Region"] <- "region"
names(england_postcodes)[names(england_postcodes)=="LSOA Code"] <- "lsoa_code"
names(england_postcodes)[names(england_postcodes)=="Middle layer super output area"] <- "middle_layer_super_output_area"
names(england_postcodes)[names(england_postcodes)=="Latitude"] <- "latitude"
names(england_postcodes)[names(england_postcodes)=="Northing"] <- "northing"
names(england_postcodes)[names(england_postcodes)=="District"] <- "district"
names(england_postcodes)[names(england_postcodes)=="Ward Code"] <- "ward_code"
names(england_postcodes)[names(england_postcodes)=="Constituency"] <- "constituency"
names(england_postcodes)[names(england_postcodes)=="Parish"] <- "parish"
names(england_postcodes)[names(england_postcodes)=="Households"] <- "households"
names(england_postcodes)[names(england_postcodes)=="Lower layer super output area"] <- "lower_layer_super_output_area"
names(england_postcodes)[names(england_postcodes)=="Altitude"] <- "altitude"
names(england_postcodes)[names(england_postcodes)=="Local authority"] <- "local_authority"
names(england_postcodes)[names(england_postcodes)=="Parish Code"] <- "parish_code"

england_postcodes$district <- NULL
england_postcodes$county <- NULL

pp_england <- left_join(pp_london_complete, england_postcodes)

pp_england <- pp_england %>%
  group_by(lsoa_code, property_type, population, households, london_zone) %>%
  summarise(price = mean(price),
            number_sales = n_distinct(transaction))

tpal_lsoa2011 <- read_excel("LSOA2011 AvPTAI2015.xlsx")

names(tpal_lsoa2011)[names(tpal_lsoa2011)=="LSOA2011"] <- "lsoa_code"

names(tpal_lsoa2011)[names(tpal_lsoa2011)=="AvPTAI2015"] <- "ptal_score"

names(tpal_lsoa2011)[names(tpal_lsoa2011)=="PTAL"] <- "ptal_level"

pp_london_2012_2017 <- left_join(tpal_lsoa2011, pp_england)

pp_london_2012_2017 <- pp_london_2012_2017[pp_london_2012_2017$price <= 2000000 & 
                                   is.na(pp_london_2012_2017$property_type)==FALSE &
                                   pp_london_2012_2017$property_type!="Other",] #&
                                   #pp_london_2012_2017$number_sales<=50,]
pp_london_2012_2017$price <- pp_london_2012_2017$price/1000000

ptal_colour <- c("1a" = "#f80044",
                 "1b" = "#3fd000",
                 "2" = "#29007c",
                 "3" = "#c37900",
                 "4" = "#859aff",
                 "5" = "#d000a8",
                 "6a" = "#a1a2c7",
                 "6b" = "#002a5e")

p3 <- ggplot(pp_london_2012_2017, aes(x = ptal_score, y = price)) +
  geom_point(alpha=0.5, aes(group=ptal_level, col=ptal_level, size=number_sales)) + #shape=new_build,
  scale_size(breaks=c(1,10,25,50,100), range = c(1,4), trans = "log10") + 
  guides(col = guide_legend(title="PTAL Grade", nrow=1), size=guide_legend(title="Number of Sales")) + #,  
  scale_y_continuous() +
  scale_x_log10() + 
  geom_smooth(se=FALSE) + 
  scale_colour_manual(values=ptal_colour) + 
  labs(x = "PTAL Score", y = "Price (in £1,000,000)", caption = "Evan Odell") +
  theme(legend.position = "bottom", legend.box = "vertical") + 
  ggtitle("Housing Prices and PTAL Score, 1 September 2012 to 31 August 2017")

p3

ggsave(plot=p3, filename="p3-ptal-EO.png", width = 18, height = 12, units = "cm", type = "cairo-png")

#ggsave(plot=p3, filename="p3.eps", device=cairo_ps, width = 30, height = 24, units = "cm")

p4 <- p3 + 
  facet_grid(. ~ property_type)  + 
  ggtitle("Housing Prices and PTAL Score, 1 September 2012 to 31 August 2017, by Housing Type") +
  theme(legend.position = "bottom", legend.box = "horizontal") 

p4

ggsave(plot=p4, filename="p4-ptal-EO.png", width = 25, height = 15, units = "cm", type = "cairo-png")

#ggsave(plot=p4, filename="p4.eps", device=cairo_ps, width = 30, height = 18, units = "cm")

