
pacman::p_load(readr)
pacman::p_load(dplyr)
pacman::p_load(readxl)
pacman::p_load(ggplot2)
pacman::p_load(scales)
pacman::p_load(magrittr)
pacman::p_load(Cairo)

pp_2017 <- read_csv("pp-2017.csv", col_names = FALSE)

pp_2017 <- rename(pp_2017, 
                  transaction = X1,
                  price = X2,
                  date = X3,
                  postcode = X4,
                  property_type = X5,
                  new_build = X6,
                  ownership_type = X7,
                  paon = X8,
                  saon = X9,
                  street = X10,
                  locality = X11,
                  city = X12,
                  district = X13,
                  county = X14,
                  category_type = X15,
                  record_status = X16)

pp_2017$county
  
## Recoding pp_2017$property_type
pp_2017$property_type <- recode(pp_2017$property_type,
               "S" = "Semi-detached",
               "T" = "Terraced",
               "D" = "Detached",
               "F" = "Flat",
               "O" = "Other")
pp_2017$property_type <- factor(pp_2017$property_type, levels=c("Detached", "Semi-detached", "Terraced", "Flat", "Other"))

## Recoding pp_2017$ownership_type into pp_2017$ownership_type
pp_2017$ownership_type <- recode(pp_2017$ownership_type,
                "F" = "Freehold",
               "L" = "Leasehold")
pp_2017$ownership_type <- factor(pp_2017$ownership_type, levels = c("Freehold", "Leasehold"))

## Recoding pp_2017$new_build into pp_2017$new_build
pp_2017$new_build <- recode(pp_2017$new_build,
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
    
pp_england_2017 <- left_join(pp_2017, england_postcodes)

pp_england_2017 <- pp_england_2017 %>%
  group_by(lsoa_code, property_type, population, households, london_zone) %>%
  summarise(price = mean(price),
            number_sales = n_distinct(transaction))

tpal_lsoa2011 <- read_excel("LSOA2011 AvPTAI2015.xlsx")

names(tpal_lsoa2011)[names(tpal_lsoa2011)=="LSOA2011"] <- "lsoa_code"

names(tpal_lsoa2011)[names(tpal_lsoa2011)=="AvPTAI2015"] <- "ptal_score"

names(tpal_lsoa2011)[names(tpal_lsoa2011)=="PTAL"] <- "ptal_level"

pp_london_2017 <- left_join(tpal_lsoa2011, pp_england_2017)

pp_london_2017 <- pp_london_2017[pp_london_2017$price <= 2000000 & 
                                is.na(pp_london_2017$property_type)==FALSE &
                                pp_london_2017$property_type!="Other",] #&
                                #pp_london_2017$number_sales<=50,]

pp_london_2017$price <- pp_london_2017$price/1000000

ptal_colour <- c("1a" = "#f80044",
"1b" = "#3fd000",
"2" = "#29007c",
"3" = "#c37900",
"4" = "#859aff",
"5" = "#d000a8",
"6a" = "#a1a2c7",
"6b" = "#002a5e")



p1 <- ggplot(pp_london_2017, aes(x = ptal_score, y = price)) +
  geom_point(alpha=0.5, aes(group=ptal_level, col=ptal_level, size=number_sales)) + #shape=new_build,
  scale_size(breaks=c(1,10,25,50,100), range = c(1,5), trans = "log10") + 
  guides(col = guide_legend(title="PTAL Grade", nrow=1), size=guide_legend(title="Number of Sales")) + #,  
  scale_y_continuous() +
  scale_x_log10() + 
  geom_smooth(se=FALSE) + 
  scale_colour_manual(values=ptal_colour) + 
  labs(x = "PTAL Score", y = "Price (in £1,000,000)", caption = "Evan Odell") +
  theme(legend.position = "bottom", legend.box = "vertical", plot.title = element_text(size = 14)) + 
  ggtitle("Housing Prices and PTAL Score, 1 January 2017 to 31 August 2017")

p1

ggsave(plot=p1, filename="p1-ptal-EO.png", width = 25, height = 16, units = "cm", type = "cairo-png", dpi = 300)

#ggsave(plot=p1, filename="p1.eps", device=cairo_ps, width = 30, height = 24, units = "cm")

p2 <- p1 + 
  facet_grid(. ~ property_type) + 
  ggtitle("Housing Prices and PTAL Score, 1 January 2017 to 31 August 2017, by Housing Type")  +
  theme(legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(size = 14)) 

p2

ggsave(plot=p2, filename="p2-ptal-EO.png", width = 25, height = 16, units = "cm", type = "cairo-png")

#ggsave(plot=p2, filename="p2.eps", device=cairo_ps, width = 30, height = 18, units = "cm")


v <- pp_london_2017[pp_london_2017$ptal_level=="6a" & pp_london_2017$property_type=="Detached",]

