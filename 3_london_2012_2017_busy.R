
pacman::p_load(readr)
pacman::p_load(dplyr)
pacman::p_load(readxl)
pacman::p_load(ggplot2)
pacman::p_load(scales)
pacman::p_load(magrittr)
pacman::p_load(Cairo)
pacman::p_load(lubridate)

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

#pp_england <- pp_england[pp_england$date >= "2006-01-01",]

pp_england$year <- factor(lubridate::year(pp_england$date))

summary(pp_england$year)

pp_england <- pp_england %>%
  group_by(lsoa_code, property_type, ownership_type, year, population, households, london_zone) %>%
  summarise(price = mean(price),
            number_sales = n_distinct(transaction))

tpal_lsoa2011 <- read_excel("LSOA2011 AvPTAI2015.xlsx")

names(tpal_lsoa2011)[names(tpal_lsoa2011)=="LSOA2011"] <- "lsoa_code"

names(tpal_lsoa2011)[names(tpal_lsoa2011)=="AvPTAI2015"] <- "ptal_score"

names(tpal_lsoa2011)[names(tpal_lsoa2011)=="PTAL"] <- "ptal_level"

pp_london_1995_2017 <- left_join(tpal_lsoa2011, pp_england)

pp_london_1995_2017 <- pp_london_1995_2017[pp_london_1995_2017$price <= 2000000 & 
                                             is.na(pp_london_1995_2017$property_type)==FALSE &
                                             is.na(pp_london_1995_2017$ownership_type)==FALSE &
                                             pp_london_1995_2017$property_type!="Other",]

pp_london_1995_2017$price <- pp_london_1995_2017$price/1000000

ptal_colour <- c("1a" = "#f80044",
                 "1b" = "#3fd000",
                 "2" = "#29007c",
                 "3" = "#c37900",
                 "4" = "#859aff",
                 "5" = "#d000a8",
                 "6a" = "#a1a2c7",
                 "6b" = "#002a5e")

year_colour= c("2006" = "#9100bf", 
               "2007" = "#8ae100", 
               "2008" = "#000655", 
               "2009" = "#d1c000", 
               "2010" = "#ff69e1", 
               "2011" = "#00c966", 
               "2012" = "#bb0047", 
               "2013" = "#e2ff94", 
               "2014" = "#003857", 
               "2015" = "#ff894b", 
               "2016" = "#305d00", 
               "2017" = "#ffccc8")


p5 <- ggplot(pp_london_1995_2017, aes(x = ptal_score, y = price)) +
  geom_point(alpha=0.5, aes(group=year, col=ptal_level, size=number_sales, shape=ownership_type)) + #,
  scale_size(breaks=c(1,10,25,50,100), range = c(1,4), trans = "log10") + 
  guides(col = guide_legend(title="PTAL Grade"),
         size=guide_legend(title="Number of Sales"),
         shape=guide_legend(title="Ownership Types")) + #,  
  scale_y_continuous() +
  scale_x_log10() + 
  geom_smooth(se=FALSE) + 
  scale_colour_manual(values=ptal_colour) + 
  labs(x = "PTAL Score", y = "Price (in £1,000,000)", caption = "Evan Odell") +
  theme(legend.position = "bottom", legend.box = "vertical", plot.title = element_text(size = 14)) + 
  ggtitle("Housing Prices and PTAL Score, 1 January 2006 to 31 August 2017")

#p5

ggsave(plot=p5, filename="p5-ptal-EO.png", width = 25, height = 16, units = "cm", type = "cairo-png")

#ggsave(plot=p5, filename="p5.eps", device=cairo_ps, width = 30, height = 24, units = "cm")

p6 <- p5 + 
  facet_wrap( ~ year)  + 
  ggtitle("Housing Prices and PTAL Score, 1 January 1995 to 31 August 2017, by year") +
  theme(legend.position = "right", legend.direction = "vertical",  plot.title = element_text(size = 14)) 

#p6

ggsave(plot=p6, filename="p6-ptal-EO.png", width = 30, height = 50, units = "cm", type = "cairo-png")

#ggsave(plot=p6, filename="p6.eps", device=cairo_ps, width = 30, height = 18, units = "cm")
