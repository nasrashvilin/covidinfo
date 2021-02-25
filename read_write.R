library(tidyverse)
library(httr)
library(readxl)
library(rvest)

### Main data

setwd("/srv/shiny-server/")

url <- "https://www.dropbox.com/scl/fi/5f734v40u5t8pogvvcbpa/covid_data_georgia.xlsx?dl=1&rlkey=iwbmp1y34u30e9bri8n7y5hku"

GET(url, write_disk("www/data/src.xlsx", overwrite = T))

total <- read_excel("www//data//src.xlsx", "total")
detailed <- read_excel("www//data//src.xlsx", "detailed")
regions <- read_excel("www//data//src.xlsx", "regions")
hospitalization <- read_excel("www//data//src.xlsx", "hospitalization")

total <- write.csv(total, "www//data//total.csv", row.names = F)
detailed <- write.csv(detailed, "www//data//detailed.csv", row.names = F)
regions <- write.csv(regions, "www//data//regions.csv", row.names = F)
hospitalization <- write.csv(hospitalization, "www//data//hospitalization.csv", row.names = F)


### Facebook humanitarian mobility data

moving_url_part <- GET("https://data.humdata.org/dataset/movement-range-maps")%>%
  read_html()%>%
  html_nodes(".ga-download") %>%
  html_attr('href')

moving_url_part <- moving_url_part[2]

activity_url <- paste0("https://data.humdata.org", moving_url_part)

# Re-downloading takes time, thus I'm keeping already downloaded dat

GET(activity_url, write_disk(tf2 <- tempfile(fileext = ".zip")))

filename <- unzip(tf2, list = T)[2, 1]

# note that here I modified fyour original read.table() which did not work
fb_mov <- readr::read_delim(unz(tf2, filename), delim = "\t")%>%
  filter(polygon_name %in% c("Tbilisi", "Batumi", "Kutaisi"))

### https://www.dropbox.com/s/wsz5yd5ryemnju0/fb_mov.xlsx?dl=1
write.csv(fb_mov, "www/data/fb_mov.csv", row.names = F)

unlink(tf2)


# Google Mobility data
# Same here. takes too long
google_mobility <- readr::read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv") %>%
  # google_mobility <- readr::read_csv("www/google_mobility.csv")%>%
  filter(country_region=="Georgia")%>%
  select(country_region, metro_area, date, retail_and_recreation_percent_change_from_baseline,
         grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline,
         transit_stations_percent_change_from_baseline,
         workplaces_percent_change_from_baseline,
         residential_percent_change_from_baseline)

# https://www.dropbox.com/s/97uyhp7l197dz5l/gl_mov.xlsx?dl=1

write.csv(google_mobility, "www/data/gl_mov.csv", row.names = F)


tracking_r <- readr::read_csv("https://raw.githubusercontent.com/crondonm/TrackingR/main/Estimates-Database/database.csv") %>%
  filter(`Country/Region` == "Georgia")%>%
  filter(days_infectious == 7)

write.csv(tracking_r, "www/data/tracking_r.csv", row.names = F)
# https://www.dropbox.com/s/ngupupgcr9d3pre/tracking_r.csv?dl=1

stringency <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")

stringency <- stringency[stringency$CountryName == "Georgia", ]

write.csv(stringency, "www/data/stringency.csv", row.names = F)

# https://www.dropbox.com/s/g54h2p54hxzoq13/stringency.csv?dl=1

