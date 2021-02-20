library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggiraph)
library(rgdal)
library(leaflet)
library(zoo)

i18n <- Translator$new(translation_json_path='translations/translation.json')

i18n$set_translation_language('ქართული')

################################################ 
### Data setup
################################################

total <- readxl::read_excel("www/src.xlsx", sheet="total")
detailed <- readxl::read_excel("www/src.xlsx", sheet="detailed")
regions <- readxl::read_excel("www/src.xlsx", sheet="regions")
hospitalization <- readxl::read_excel("www/src.xlsx", sheet="hospitalization")

tracking_r <- readr::read_csv("https://raw.githubusercontent.com/crondonm/TrackingR/main/Estimates-Database/database.csv") %>%
  filter(`Country/Region` == "Georgia")%>%
  filter(days_infectious == 7)

stringency <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")

stringency <- stringency[stringency$CountryName == "Georgia", ]

## reactivePoll

server <- function(input, output, session) {
  observeEvent(input$selected_language, {
    # This print is just for demonstration
    print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })
  
################################################ 
### Section Home: 
################################################
### Extract newest observation for new cases

new_cases <-  renderText({detailed %>%
    mutate(lubridate::as_date(date))%>%
    arrange(desc(date))%>%
    slice(1)%>%
    pull(new_cases)})

output$new_cases <- shinydashboard::renderValueBox({
  valueBox(
    paste0(new_cases()), i18n$t("ახალი შემთხვევა"), icon = icon("virus"),
    color = "orange", width=1
  )
})

### Extract newest observation for new deaths
new_deaths <- renderText({detailed %>%
  mutate(lubridate::as_date(date))%>%
  arrange(desc(date))%>%
  slice(1)%>%
  pull(new_deaths)})

output$new_deaths <- renderValueBox({
  valueBox(
    paste0(new_deaths()), i18n$t("გარდაიცვალა"), icon = icon("ribbon"),
    color = "red"
  )
})

new_recoveries <- renderText({detailed %>%
  mutate(lubridate::as_date(date))%>%
  arrange(desc(date))%>%
  slice(1)%>%
  pull(new_recoveries)})
output$new_recoveries <- renderValueBox({
  valueBox(
    paste0(new_recoveries()), i18n$t("გამოჯანმრთელდა"), icon = icon("heartbeat"),
    color = "green"
  )
})

### Extract newest observation for hospitalizations

new_hospitalization <- renderText({hospitalization %>%
  mutate(lubridate::as_date(date))%>%
  arrange(desc(date))%>%
  slice(1)%>%
  pull(total_hospitalized)})

output$new_hospitalization <- renderValueBox({
  valueBox(
    paste0(new_hospitalization()), i18n$t("ჰოსპიტალიზაცია"), icon = icon("hospital-symbol"),
    color = "blue"
  )
})

### Extract newest observation for daily tests

new_tests <- renderText({total %>%
  mutate(lubridate::as_date(date))%>%
  arrange(desc(date))%>%
  slice(1)%>%
  pull(total_daily_tests)})

output$new_tests <- renderValueBox({
  valueBox(
    paste0(new_tests()), i18n$t("ტესტები"), icon = icon("vial"),
    color = "olive"
  )
})

### Extract newest observation for daily vaccination rate

vaccinated <-  renderText({detailed %>%
    mutate(lubridate::as_date(date))%>%
    arrange(desc(date))%>%
    slice(1)%>%
    pull(vaccinated)})

count_innoculated <- renderText({"0"}) # comment this if when at least one case of vaccination is available

output$new_innoculated <- renderValueBox({
  valueBox(
    paste0(count_innoculated()), i18n$t("აცრილი"), icon = icon("syringe"),
    color = "aqua"
  )
})

### Extract newest observation for active cases

new_active <- renderText({hospitalization %>%
    mutate(lubridate::as_date(date))%>%
    arrange(desc(date))%>%
    slice(1)%>%
    pull(active_patients)})

output$new_active <- renderValueBox({
  valueBox(
    paste0(new_active()), i18n$t("აქტიური შემთხვევები"), icon = icon("hand-paper"),
    color = "purple"
  )
})

### Extract newest observation for active cases

new_critical <- renderText({hospitalization %>%
    mutate(lubridate::as_date(date))%>%
    arrange(desc(date))%>%
    slice(1)%>%
    pull(critical_patients)})

output$new_critical <- renderValueBox({
  valueBox(
    paste0(new_critical()), i18n$t("კრიტიკული შემთხვევა"), icon = icon("exclamation-triangle"),
    color = "maroon"
  )
})

### Extract newest observation for Reproduction

new_r <- renderText({tracking_r %>%
    mutate(date=as.Date(Date))%>%
    arrange(desc(date))%>%
    slice(1)%>%
    pull(R)})

output$new_r <- renderValueBox({
  valueBox(
    paste0(new_r()), i18n$t("რეპროდუქციის ინდექსი"), icon = icon("registered"),
    color = "teal"
  )
})



################################################ 
### Section Total: 
################################################
### Extract newest observation for new cases

todays_date <- total %>%
  mutate(lubridate::as_date(date))%>%
  arrange(desc(date))%>%
  slice(1)%>%
  pull(date)

output$todays_date <- renderText({format(todays_date, format = "%d/%m/%Y")})



## Build charts

tooltip_css <- "background-color:gray;color:white;font-style:bold;
                padding:10px;border-radius:5px;font-family:BPG_lower"

daily_cases  <- 
  detailed %>%
  mutate(roll_7= rollmean(new_cases, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, new_cases, tooltip = paste0(date, ": ", new_cases), data_id = new_cases), size=0.4,
                         color=NA, fill = "orange", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="orange")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()
  

print(daily_cases)

output$daily_cases_chart <- renderGirafe(

  
  girafe(ggobj =   daily_cases <- daily_cases+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("ახალი შემთხვევების რ-ნობა")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(width = .7) ) 
         )
)

recov_ts  <- 
  detailed %>%
  mutate(roll_7= rollmean(new_recoveries, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, new_recoveries, tooltip = paste0(date, ": ", new_recoveries),
                           data_id = new_recoveries), size=0.4,
                       color=NA, fill = "darkgreen", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="darkgreen")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(recov_ts)

output$recov_ts_chart <- renderGirafe(
  girafe(ggobj = recov_ts+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("გამოჯანმრთელებულთა რ-ნობა")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(width = .7) ) 
  )
)

deaths_ts  <- 
  detailed %>%
  mutate(roll_7= rollmean(new_deaths, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, new_deaths, tooltip = paste0(date, ": ", new_deaths),
                           data_id = new_deaths), size=0.4,
                       color=NA, fill = "red", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="red")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(deaths_ts)

output$deaths_ts_chart <- renderGirafe(
  girafe(ggobj = deaths_ts+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("გარდაცვლილთა რ-ნობა")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(width = .7) ) 
  )
)

tests_ts  <- 
  total %>%
  mutate(roll_7= rollmean(total_daily_tests, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, total_daily_tests, tooltip = paste0(date, ": ", round(total_daily_tests, 0)),
                           data_id = total_daily_tests), size=0.4,
                       color=NA, fill = "#808000", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="#808000")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(deaths_ts)

output$tests_ts_chart <- renderGirafe(
  girafe(ggobj = tests_ts+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("ტესტების რ-ნობა")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(width = .7) ) 
  )
)

### Build maps

regs <- readOGR("shapefiles/administrative_regions.shp") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

ocr <- readOGR("shapefiles/occupied_regions.shp") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))


map <- read.csv("www/map.csv", encoding = "UTF-8")

regional_data <- regions %>%
  mutate(date=lubridate::as_date(date))%>%
    arrange(desc(date))%>%
    slice(1)%>%
    dplyr::select("date", "Adjara", "Tbilisi", "Imereti", "Samegrelo", "Shida_Kartli",
                  "Kvemo_Kartli", "Guria", "Kakheti", "Samtskhe_Javakheti",
                  "Mtskheta_Mtianeti", "Racha_Lechkhumi", "Abkhazia",  "Tskhinvali") %>%
    mutate_all( ~ as.numeric(.))%>%
    pivot_longer(-date, names_to="Name_EN", values_to= "cases")%>%
    left_join(map, by="Name_EN")%>%
    mutate(date=lubridate::as_date(date))

regs@data <- left_join(regs@data, regional_data, by="ABBR")%>%
  mutate(per_1000 = (cases*1000)/POPULATION)

labels_today <- sprintf(
  "<strong>%s</strong>: %g </br>",
  regs$date, regs$cases)%>%
  lapply(htmltools::HTML)

labels_pc <- sprintf(
  "<strong>%s</strong>: %g </br>",
  regs$date, regs$per_1000)%>%
  lapply(htmltools::HTML)

output$today_map <- renderLeaflet({
leaflet()%>%
  setView(lat=41.9894889, lng=43.5410215, zoom=7)%>%
  addPolygons(data=regs, color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", cases)(cases),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              label = labels_today,  labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  addPolygons(data=ocr, color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5, fillColor = "#999999",
              )%>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE)
  )

  })

output$today_map_pc <- renderLeaflet({
  leaflet()%>%
    setView(lat=41.9894889, lng=43.5410215, zoom=7)%>%
    addPolygons(data=regs, color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~colorQuantile("YlOrRd", per_1000)(per_1000),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                label = labels_pc,  labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))%>%
    addPolygons(data=ocr, color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5, fillColor = "#999999",
    )%>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)
    )
  
})

################################################ 
### Section Total: 
################################################

### Cumulative outputs

output$cumulative_cases <- renderValueBox({
  valueBox(
    paste0(count_total_cases()), i18n$t("დადასტურებული შემთხვევა"), icon = icon("virus"),
    color = "orange"
  )
})


output$cumulative_deaths <- renderValueBox({
  valueBox(
    paste0(count_total_deaths()), i18n$t("გარდაიცვალა"), icon = icon("ribbon"),
    color = "red"
  )
})

output$cumulative_recoveries <- renderValueBox({
  valueBox(
    paste0(count_total_recovered()), "გამოჯანმრთელდა", icon = icon("heartbeat"),
    color = "green"
  )
})

output$pop_share <- renderValueBox({
  valueBox(
    paste0(count_pop_share(), "%"), i18n$t("დაავადებულთა წილი მოსახლეობაში"), icon = icon("chart-pie"),
    color = "blue"
  )
})

output$cumulative_tests <- renderValueBox({
  valueBox(
    paste0(count_total_tests()), i18n$t("ტესტები"), icon = icon("vial"),
    color = "olive"
  )
})

output$cumulative_innoculated <- renderValueBox({
  valueBox(
    paste0(count_innoculated()), i18n$t("აცრილი"), icon = icon("syringe"),
    color = "aqua"
  )
})



count_total_cases <- renderText({
  sum(detailed$new_cases)
})

count_pop_share <- renderText({
  round(100*(sum(detailed$new_cases)/3716900), 1)
})

count_total_deaths <- renderText({
  sum(detailed$new_deaths)
})

count_total_recovered <- renderText({
  sum(detailed$new_recoveries)
})

count_total_hospitalized <- renderText({
  ifelse(!is.na(sum(hospitalization$total_hospitalized)), sum(hospitalization$total_hospitalized), "?")
})

count_total_tests <- renderText({
  sum(total$total_daily_tests, na.rm = T)
})

count_innoculated <- renderText({"0"})


#### Charts

tot_cumulative_cases  <- 
  detailed %>%
  mutate(roll_7= rollmean(total, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, total, tooltip = paste0(date, ": ", total),
                           data_id = total), size=0.4,
                       color=NA, fill = "orange", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="orange")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(tot_cumulative_cases)

output$tot_cumulative_cases_ch <- renderGirafe(
  girafe(ggobj = tot_cumulative_cases+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("შემთხვევების ჯამური რ-ნობა")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(width = .7) ) 
  )
)

tot_cumulative_recovered  <- 
  detailed %>%
  mutate(roll_7= rollmean(total_rec, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, total_rec, tooltip = paste0(date, ": ", total_rec),
                           data_id = total_rec), size=0.4,
                       color=NA, fill = "darkgreen", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="darkgreen")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(tot_cumulative_recovered)

output$tot_cumulative_recovered_ch <- renderGirafe(
  girafe(ggobj = tot_cumulative_recovered+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("გამოჯანმრთელებულთა ჯამური რ-ნობა")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(rescale = TRUE) ) 
  )
)

tot_cumulative_died  <- 
  detailed %>%
  mutate(roll_7= rollmean(total_deaths, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, total_deaths, tooltip = paste0(date, ": ", total_deaths),
                           data_id = total_deaths), size=0.4,
                       color=NA, fill = "red", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="red")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(tot_cumulative_died)

output$tot_cumulative_died_ch <- renderGirafe(
  girafe(ggobj = tot_cumulative_died+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("გარდაცვლილთა ჯამური რ-ნობა")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(rescale = TRUE) ) 
  )
)
tot_cumulative_tests  <- 
  total %>%
  mutate(roll_7= rollmean(total_test, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, total_test, tooltip = paste0(date, ": ", round(total_test, 0)),
                           data_id = total_test), size=0.4,
                       color=NA, fill = "#808000", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="#808000")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(tot_cumulative_tests)

output$tot_cumulative_tests_ch <- renderGirafe(
  girafe(ggobj = tot_cumulative_tests+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("ტესტების ჯამური რ-ნობა")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(rescale = TRUE) ) 
  )
)

##################################
#### Tests
#################################

tot_positive_ratio <-
  total %>%
  mutate(roll_7= rollmean(total_positive_share, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, total_positive_share, tooltip = paste0(date, ": ", round(total_positive_share, 0)),
                           data_id = total_positive_share), size=0.4,
                       color=NA, fill = "#808000", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="#808000")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(tot_positive_ratio)

output$tot_positive_ratio_ch <- renderGirafe(
  girafe(ggobj = tot_positive_ratio+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("დადებითი ტესტების წილი, %")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(rescale = TRUE) )
  )
)

tot_pcr_tests  <- 
  total %>%
  mutate(roll_7= rollmean(daily_PCR_tests, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, daily_PCR_tests, tooltip = paste0(date, ": ", round(daily_PCR_tests, 0)),
                           data_id = daily_PCR_tests), size=0.4,
                       color=NA, fill = "#808000", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="#808000")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(tot_pcr_tests)

output$tot_pcr_tests_ch <- renderGirafe(
  girafe(ggobj = tot_pcr_tests+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("PCR ტესტების ყოველდღიური რ-ნობა")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(rescale = TRUE) ) 
  )
)

tot_rapid_tests  <- 
  total %>%
  mutate(roll_7= rollmean(daily_rapid_test, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, daily_rapid_test, tooltip = paste0(date, ": ", round(daily_rapid_test, 0)),
                           data_id = daily_rapid_test), size=0.4,
                       color=NA, fill = "#808000", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="#808000")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(tot_rapid_tests)

output$tot_rapid_tests_ch <- renderGirafe(
  girafe(ggobj = tot_rapid_tests+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("სწრაფი ტესტების ყოველდღიური რ-ნობა")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(rescale = TRUE) ) 
  )
)

##################################
#### Hospitalization
#################################
cumul_hospitalized  <- 
  hospitalization %>%
  mutate(roll_7= rollmean(total_hospitalized, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, total_hospitalized, tooltip = paste0(date, ": ", round(total_hospitalized, 0)),
                           data_id = total_hospitalized), size=0.4,
                       color=NA, fill = "blue", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="blue")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(cumul_hospitalized)

output$tot_hospitalized_ch <- renderGirafe(
  girafe(ggobj = cumul_hospitalized+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("პაციენტების რ-ნობა საავადმყოფოებში")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(rescale = TRUE) ) 
  )
)

thous_hospitalized  <- 
  hospitalization %>%
  mutate(roll_7= rollmean(hospitalized_per_100k, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, hospitalized_per_100k, tooltip = paste0(date, ": ", round(hospitalized_per_100k, 0)),
                           data_id = hospitalized_per_100k), size=0.4,
                       color=NA, fill = "blue", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="blue")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(thous_hospitalized)

output$thous_hospitalized_ch <- renderGirafe(
  girafe(ggobj = thous_hospitalized+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("პაციენტების რ-ნობა საავადმყოფოებში, 1000 მოსახლეზე")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(rescale = TRUE) ) 
  )
)

critical  <- 
  hospitalization %>%
  mutate(roll_7= rollmean(critical_patients, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, critical_patients, tooltip = paste0(date, ": ", round(critical_patients, 0)),
                           data_id = critical_patients), size=0.4,
                       color=NA, fill = "blue", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="blue")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(critical)

output$critical_ch <- renderGirafe(
  girafe(ggobj = critical+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("მძიმე პაციენტები")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(rescale = TRUE) ) 
  )
)

ventil  <- 
  hospitalization %>%
  mutate(roll_7= rollmean(on_ventilator, 7, align = "left", fill = NA))%>%
  ggplot()+
  geom_col_interactive(aes(date, on_ventilator, tooltip = paste0(date, ": ", round(on_ventilator, 0)),
                           data_id = on_ventilator), size=0.4,
                       color=NA, fill = "blue", alpha=0.5)+
  geom_line(aes(date, roll_7), size=1, color="blue")+
  scale_x_datetime(date_labels = "%m/%Y")+
  theme_minimal()

print(ventil)

output$ventil_ch <- renderGirafe(
  girafe(ggobj = ventil+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("პაციენტები მართვით სუნთქვაზე")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(rescale = TRUE) ) 
  )
)
##################################
#### Other indicators
#################################

## R
tracking_r_rate  <- tracking_r %>%
  mutate(date=as.Date(Date))%>%
  ggplot()+
  geom_line_interactive(aes(date, R, tooltip=R), color = "red", alpha=0.5, size=1)+
  geom_hline(yintercept = 1, color="grey")+
  scale_x_date(date_labels = "%m/%Y")+
  theme_minimal()

print(tracking_r_rate)

output$tracking_r_output <- renderGirafe(
  girafe(ggobj = tracking_r_rate+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("რეპროდუქციის ინდექსი")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(width = .7) ) 
  )
)

## Stringency

tracking_stringency  <- stringency %>%
  mutate(
    Date = as.character(Date),
    Date = sub("(.{4})(.*)", "\\1-\\2", Date),
    Date = sub("(.{7})(.*)", "\\1-\\2", Date),
    date=lubridate::as_date(Date))%>%
  ggplot()+
  geom_col_interactive(aes(date, StringencyIndexForDisplay, tooltip = paste0(date, ": ", round(StringencyIndexForDisplay, 0)),
                           data_id = StringencyIndexForDisplay), size=0.4,
                       color=NA, fill = "darkblue", alpha=0.2)+
  geom_line(aes(date, StringencyIndexForDisplay), size=1, color="darkblue")+
  geom_hline(yintercept = 1, color="grey")+
  scale_x_date(date_labels = "%m/%Y")+
  theme_minimal()

print(tracking_stringency)

output$tracking_stringency_output <- renderGirafe(
  girafe(ggobj = tracking_stringency+
           xlab(i18n$t("თვეები"))+
           ylab(i18n$t("სიმკაცრის ინდექსი")),
         options = list(opts_tooltip(css = tooltip_css),
                        opts_sizing(width = .7) ) 
  )
)
## 




} ### This ends server part
