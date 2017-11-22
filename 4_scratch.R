pacman::p_load(readr)
pacman::p_load(tibble)

hpi <- read_csv("csv_data/UK-HPI-full-file-2017-09.csv", 
                                     col_types = cols(Date = col_date(format = "%d/%m/%Y")))
glimpse(hpi)


x <- hpi[hpi$RegionName=="London",]
