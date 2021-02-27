library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggiraph)
library(rgdal)
library(leaflet)
library(zoo)
library(scales)
library(httr)
library(rvest)
library(shinyBS)
library(readxl)

################################################ 
### Translation setup
################################################
options(shiny.sanitize.errors = FALSE)

pdf(NULL)

i18n <- Translator$new(translation_json_path='translations/translation.json')

i18n$set_translation_language('ქართული')

################################################ 
### Define ggplot theme
################################################

theme_nn <- function () { 
  theme_minimal(base_size=12) %+replace% # , base_family="Roboto"
    theme(
      plot.title = element_text(size=15,hjust=0),
      plot.subtitle = element_text(size=9, color="grey40", hjust=0),
      plot.caption = element_text(size=8),
      axis.title = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size=8),
      plot.title.position = "plot",
      panel.grid.major.y = element_line(color="white", 
                                        size=.5),
      panel.grid = element_blank(),
      legend.position = "none", #c(.15,.9),
      legend.direction = "horizontal",
      plot.background = element_rect(fill="#f0f0f0",
                                     color=NA),
      strip.text = element_text(hjust=0),
      axis.text.x = element_text(size=12,color="black", hjust=0),
      axis.text.y = element_text(size=12, color="black", hjust=1,vjust=.5)#,
                                 margin = margin(l = 0, 
                                                 r = -10)) #,family="Calibri")
    )
}



server <- function(input, output, session) {
  observeEvent(input$selected_language, {
    # This print is just for demonstration
    print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })
  
  ################################################ 
  ### Implement a scheduler
  ################################################
  
  observe({
    
    invalidateLater(3600000, session)
    
    ################################################ 
    ### Data setup
    ################################################
    
    
    total <- read.csv("www//data//total.csv")%>%mutate(date=lubridate::as_date(date))
    detailed <- read.csv("www//data//detailed.csv")%>%mutate(date=lubridate::as_date(date))
    regions <- read.csv("www//data//regions.csv")%>%mutate(date=lubridate::as_date(date))
    hospitalization <- read.csv("www//data//hospitalization.csv")%>%mutate(date=lubridate::as_date(date))
    #occupied territories
    occupied territories <- read.csv("www//data//occupied territories.csv")%>%mutate(date=lubridate::as_date(date))
    
    
    tracking_r <- read.csv("www//data//tracking_r.csv")%>%mutate(Date=lubridate::as_date(Date))
    
    stringency <- read.csv("www//data//stringency.csv")
    
    
    ### Facebook humanitarian mobility data
    
    fb_mov <- read.csv("www/data/fb_mov.csv")%>%mutate(ds=lubridate::as_date(ds))
    
    # Google Mobility data
    # Same here. takes too long
    
    google_mobility <- read.csv("www/data/gl_mov.csv")%>%mutate(date=lubridate::as_date(date))
    
    ################################################ 
    ### Timestamps
    ################################################
    
    todays_date <- total %>%
      mutate(lubridate::as_date(date))%>%
      arrange(desc(date))%>%
      slice(1)%>%
      pull(date)
    
    output$todays_date_out <- renderText({
      format(todays_date, format = "%d/%m/%Y")
    })
    
    output$updated_at <- renderText({
      paste0(i18n$t("ბოლო განახლება (თბილისის დროით)"), ": ", format(Sys.time(), "%d/%m/%Y %H:%M:%S", tz = "Asia/Tbilisi"))
    })
    
    ################################################ 
    ### Section Home: 
    ################################################
    ### Extract newest observation for new cases
    
    
    
    new_cases <-  renderText({
      detailed %>%
        mutate(lubridate::as_date(date))%>%
        arrange(desc(date))%>%
        slice(1)%>%
        pull(new_cases)
    })
    
    output$new_cases <- shinydashboard::renderValueBox({
      valueBox(
        paste0(new_cases()), i18n$t("ახალი შემთხვევა"), icon = icon("virus"),
        color = "orange", width=1
      )
    })
    
    ### Extract newest observation for new deaths
    new_deaths <- renderText({
      detailed %>%
        mutate(lubridate::as_date(date))%>%
        arrange(desc(date))%>%
        slice(1)%>%
        pull(new_deaths)
    })
    
    output$new_deaths <- renderValueBox({
      valueBox(
        paste0(new_deaths()), i18n$t("გარდაიცვალა"), icon = icon("ribbon"),
        color = "red"
      )
    })
    
    new_recoveries <- renderText({
      detailed %>%
        mutate(lubridate::as_date(date))%>%
        arrange(desc(date))%>%
        slice(1)%>%
        pull(new_recoveries)
    })
    output$new_recoveries <- renderValueBox({
      valueBox(
        paste0(new_recoveries()), i18n$t("გამოჯანმრთელდა"), icon = icon("heartbeat"),
        color = "green"
      )
    })
    
    ### Extract newest observation for hospitalizations
    
    new_hospitalization <- renderText({
      hospitalization %>%
        mutate(lubridate::as_date(date))%>%
        arrange(desc(date))%>%
        slice(1)%>%
        pull(total_hospitalized)
    })
    
    output$new_hospitalization <- renderValueBox({
      valueBox(
        paste0(new_hospitalization()), i18n$t("ჰოსპიტალიზაცია"), icon = icon("hospital-symbol"),
        color = "blue"
      )
    })
    
    ### Extract newest observation for daily tests
    
    new_tests <- renderText({
      total %>%
        mutate(lubridate::as_date(date))%>%
        arrange(desc(date))%>%
        slice(1)%>%
        pull(total_daily_tests)
    })
    
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
    
    new_r <- renderText({
      tracking_r %>%
        mutate(date=as.Date(Date))%>%
        arrange(desc(date))%>%
        slice(1)%>%
        pull(R)
    })
    
    output$new_r <- renderValueBox({
      valueBox(
        paste0(new_r()), i18n$t("რეპროდუქციის ინდექსი"), icon = icon("registered"),
        color = "teal"
      )
    })
    
    
    
    ################################################ 
    ### Section Total: 
    ################################################
    
    
    ## Build charts
    
    tooltip_css <- "background-color:gray;color:white;font-style:bold;
                padding:10px;border-radius:5px;font-family:BPG_lower"
    
    daily_cases  <- 
      detailed %>%
      mutate(roll_7= rollmean(new_cases, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, new_cases, tooltip = paste0(date, ": ", new_cases), data_id = new_cases), size=0.4,
                           color=NA, fill = "orange", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="orange")+
      scale_x_date(date_labels = "%m/%Y")+
      theme_nn()

    print(daily_cases)
    
    output$daily_cases_chart <- renderGirafe(
      
      
      girafe(ggobj =   daily_cases <- daily_cases+
               xlab(i18n$t("თვეები"))+
               ylab(i18n$t("ახალი შემთხვევების რ-ნობა")),
             options = list(opts_tooltip(css = tooltip_css),
                            opts_sizing(width = .7), width_svg=10 ) 
      )
    )
    
    recov_ts  <- 
      detailed %>%
      mutate(roll_7= rollmean(new_recoveries, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, new_recoveries, tooltip = paste0(date, ": ", new_recoveries),
                               data_id = new_recoveries), size=0.4,
                           color=NA, fill = "darkgreen", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="darkgreen")+
      scale_x_date(date_labels = "%m/%Y")+
      theme_nn()
    
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
      mutate(roll_7= rollmean(new_deaths, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, new_deaths, tooltip = paste0(date, ": ", new_deaths),
                               data_id = new_deaths), size=0.4,
                           color=NA, fill = "red", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="red")+
      scale_x_date(date_labels = "%m/%Y")+
      theme_nn()
    
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
      mutate(roll_7= rollmean(total_daily_tests, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, total_daily_tests, tooltip = paste0(date, ": ", round(total_daily_tests, 0)),
                               data_id = total_daily_tests), size=0.4,
                           color=NA, fill = "#808000", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="#808000")+
      scale_x_date(date_labels = "%m/%Y")+
      scale_y_continuous(label=comma)+
      theme_nn()
    
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
        paste0(count_total_recovered()), i18n$t("გამოჯანმრთელდა"), icon = icon("heartbeat"),
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
      mutate(roll_7= rollmean(total, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, total, tooltip = paste0(date, ": ", total),
                               data_id = total), size=0.4,
                           color=NA, fill = "orange", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="orange")+
      scale_x_date(date_labels = "%m/%Y")+
      scale_y_continuous(label=comma)+
      theme_nn()
    
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
      mutate(roll_7= rollmean(total_rec, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, total_rec, tooltip = paste0(date, ": ", total_rec),
                               data_id = total_rec), size=0.4,
                           color=NA, fill = "darkgreen", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="darkgreen")+
      scale_x_date(date_labels = "%m/%Y")+
      scale_y_continuous(label=comma)+
      theme_nn()
    
    print(tot_cumulative_recovered)
    
    output$tot_cumulative_recovered_ch <- renderGirafe(
      girafe(ggobj = tot_cumulative_recovered+
               xlab(i18n$t("თვეები"))+
               ylab(i18n$t("გამოჯანმრთელებულთა ჯამური რ-ნობა")),
             options = list(opts_tooltip(css = tooltip_css),
                            opts_sizing(rescale = TRUE), width_svg=10 ) 
      )
    )
    
    tot_cumulative_died  <- 
      detailed %>%
      mutate(roll_7= rollmean(total_deaths, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, total_deaths, tooltip = paste0(date, ": ", total_deaths),
                               data_id = total_deaths), size=0.4,
                           color=NA, fill = "red", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="red")+
      scale_x_date(date_labels = "%m/%Y")+
      theme_nn()
    
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
      mutate(roll_7= rollmean(total_test, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, total_test, tooltip = paste0(date, ": ", round(total_test, 0)),
                               data_id = total_test), size=0.4,
                           color=NA, fill = "#808000", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="#808000")+
      scale_x_date(date_labels = "%m/%Y")+
      scale_y_continuous(label=comma)+
      theme_nn()
    
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
      mutate(roll_7= rollmean(total_positive_share, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, total_positive_share, tooltip = paste0(date, ": ", round(total_positive_share, 0)),
                               data_id = total_positive_share), size=0.4,
                           color=NA, fill = "#808000", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="#808000")+
      scale_x_date(date_labels = "%m/%Y")+
      theme_nn()
    
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
      mutate(roll_7= rollmean(daily_PCR_tests, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, daily_PCR_tests, tooltip = paste0(date, ": ", round(daily_PCR_tests, 0)),
                               data_id = daily_PCR_tests), size=0.4,
                           color=NA, fill = "#808000", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="#808000")+
      scale_x_date(date_labels = "%m/%Y")+
      scale_y_continuous(label=comma)+
      theme_nn()
    
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
      mutate(roll_7= rollmean(daily_rapid_test, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, daily_rapid_test, tooltip = paste0(date, ": ", round(daily_rapid_test, 0)),
                               data_id = daily_rapid_test), size=0.4,
                           color=NA, fill = "#808000", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="#808000")+
      scale_x_date(date_labels = "%m/%Y")+
      scale_y_continuous(label=comma)+
      theme_nn()
    
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
      mutate(roll_7= rollmean(total_hospitalized, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, total_hospitalized, tooltip = paste0(date, ": ", round(total_hospitalized, 0)),
                               data_id = total_hospitalized), size=0.4,
                           color=NA, fill = "blue", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="blue")+
      scale_x_date(date_labels = "%m/%Y")+
      theme_nn()
    
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
      mutate(roll_7= rollmean(hospitalized_per_100k, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, hospitalized_per_100k, tooltip = paste0(date, ": ", round(hospitalized_per_100k, 0)),
                               data_id = hospitalized_per_100k), size=0.4,
                           color=NA, fill = "blue", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="blue")+
      scale_x_date(date_labels = "%m/%Y")+
      theme_nn()
    
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
      mutate(roll_7= rollmean(critical_patients, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, critical_patients, tooltip = paste0(date, ": ", round(critical_patients, 0)),
                               data_id = critical_patients), size=0.4,
                           color=NA, fill = "blue", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="blue")+
      scale_x_date(date_labels = "%m/%Y")+
      theme_nn()
    
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
      mutate(roll_7= rollmean(on_ventilator, 7, align = "right", fill = NA))%>%
      ggplot()+
      geom_col_interactive(aes(date, on_ventilator, tooltip = paste0(date, ": ", round(on_ventilator, 0)),
                               data_id = on_ventilator), size=0.4,
                           color=NA, fill = "blue", alpha=0.5)+
      geom_line(aes(date, roll_7), size=1, color="blue")+
      scale_x_date(date_labels = "%m/%Y")+
      theme_nn()
    
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
    #### Occipied territories
    #################################
    
    occupied_territories  <- occupied_territories %>%
      mutate(date=as.Date(date))%>%
      mutate(rolling_7=rollmean(new_cases, mean, k=7, fill=NA,
                              align = "right", partial=T)))%>%
      ggplot()+
      geom_line(aes(date, rolling_7), color = "red", size=1)+
      geom_col_interactive(aes(date, new_cases, tooltip = paste0(date, ": ", round(new_cases, 2)),
                               data_id = new_cases), size=0.4,
                           color=NA, fill = "red", alpha=0.2)+
      facet_grid(~region)+
      scale_x_date(date_labels = "%m/%Y")+
      theme_nn()
    
     print(occupied_territories)
    
     output$occupied_territories <- renderGirafe(
      girafe(ggobj = occupied_territories+
               xlab(i18n$t("თვეები"))+
               ylab(i18n$t("ახალი შემთხვევები")),
             options = list(opts_tooltip(css = tooltip_css),
                            opts_sizing(width = .7) ) 
      )
    )
    
    ##################################
    #### Other indicators
    #################################
    
    ## R
    tracking_r_rate  <- tracking_r %>%
      mutate(date=as.Date(Date))%>%
      ggplot()+
      geom_line(aes(date, R), color = "red", size=1)+
      geom_col_interactive(aes(date, R, tooltip = paste0(date, ": ", round(R, 2)),
                               data_id = R), size=0.4,
                           color=NA, fill = "red", alpha=0.2)+
      geom_hline(yintercept = 1, color="grey")+
      scale_x_date(date_labels = "%m/%Y")+
      theme_nn()
    
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
      theme_nn()
    
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
    
    ## Google mobility
    
    grocs <- google_mobility%>%
      mutate(date=as.Date(date))%>%
      arrange(desc(date))%>%
      mutate(average=rollmean(grocery_and_pharmacy_percent_change_from_baseline, mean, k=7, fill=NA,
                              align = "right", partial=T))%>%
      ggplot(aes(date, average))+
      geom_col_interactive(aes(tooltip = paste0(date, ": ", round(average, 2))), size=0.4,
                           color=NA, fill = "darkred", alpha=0.2, position="identity")+
      scale_y_continuous(labels = function(x) paste0(x, "%"))+
      geom_path(aes(date, average), col="darkred", size=1)+
      scale_x_date(date_labels = "%m/%y")+
      ylim(-100, 50)+
      theme_nn()
    
    output$grocs_ch <- renderGirafe(
      girafe(ggobj = grocs+
               xlab(i18n$t("თვეები"))+
               ylab(i18n$t("საკვები და მედიკამენტები")),
             options = list(opts_tooltip(css = tooltip_css),
                            opts_sizing(width = .7) ) 
      )
    )
    
    parks <- google_mobility%>%
      mutate(date=as.Date(date))%>%
      arrange(desc(date))%>%
      mutate(average=rollmean(parks_percent_change_from_baseline, mean, k=7, fill=NA,
                              align = "right", partial=T))%>%
      ggplot(aes(date, average))+
      geom_col_interactive(aes(tooltip = paste0(date, ": ", round(average, 2))), size=0.4,
                           color=NA, fill = "darkred", alpha=0.2, position="identity")+
      scale_y_continuous(labels = function(x) paste0(x, "%"))+
      geom_path(aes(date, average), col="darkred", size=1)+
      scale_x_date(date_labels = "%m/%y")+
      ylim(-100, 50)+
      theme_nn()
    
    output$parks_ch <- renderGirafe(
      girafe(ggobj = parks+
               xlab(i18n$t("თვეები"))+
               ylab(i18n$t("პარკები")),
             options = list(opts_tooltip(css = tooltip_css),
                            opts_sizing(width = .7) ) 
      )
    )
    
    ret_rec <- google_mobility%>%
      mutate(date=as.Date(date))%>%
      arrange(desc(date))%>%
      mutate(average=rollmean(retail_and_recreation_percent_change_from_baseline, mean, k=7, fill=NA,
                              align = "right", partial=T))%>%
      ggplot(aes(date, average))+
      geom_col_interactive(aes(tooltip = paste0(date, ": ", round(average, 2))), size=0.4,
                           color=NA, fill = "darkred", alpha=0.2, position="identity")+
      scale_y_continuous(labels = function(x) paste0(x, "%"))+
      geom_path(aes(date, average), col="darkred", size=1)+
      scale_x_date(date_labels = "%m/%y")+
      ylim(-100, 50)+
      theme_nn()
    
    output$ret_rec_ch <- renderGirafe(
      girafe(ggobj = ret_rec+
               xlab(i18n$t("თვეები"))+
               ylab(i18n$t("საყიდლები, გართობა, დასვენება")),
             options = list(opts_tooltip(css = tooltip_css),
                            opts_sizing(width = .7) ) 
      )
    )
    
    workplace <- google_mobility%>%
      mutate(dtilate=as.Date(date))%>%
      arrange(desc(date))%>%
      mutate(average=rollmean(workplaces_percent_change_from_baseline, mean, k=7, fill=NA,
                              align = "right", partial=T))%>%
      ggplot(aes(date, average))+
      geom_col_interactive(aes(tooltip = paste0(date, ": ", round(average, 2))), size=0.4,
                           color=NA, fill = "darkred", alpha=0.2, position="identity")+
      scale_y_continuous(labels = function(x) paste0(x, "%"))+
      geom_path(aes(date, average), col="darkred", size=1)+
      scale_x_date(date_labels = "%m/%y")+
      ylim(-100, 50)+
      theme_nn()
    
    output$workplace_ch <- renderGirafe(
      girafe(ggobj = workplace+
               xlab(i18n$t("თვეები"))+
               ylab(i18n$t("სამუშაო ადგილები")),
             options = list(opts_tooltip(css = tooltip_css),
                            opts_sizing(width = .7) ) 
      )
    )
    
    transit <- google_mobility%>%
      mutate(date=as.Date(date))%>%
      arrange(desc(date))%>%
      mutate(average=rollmean(transit_stations_percent_change_from_baseline, mean, k=7, fill=NA,
                              align = "right", partial=T))%>%
      ggplot(aes(date, average))+
      geom_col_interactive(aes(tooltip = paste0(date, ": ", round(average, 2))), size=0.4,
                           color=NA, fill = "darkred", alpha=0.2, position="identity")+
      scale_y_continuous(labels = function(x) paste0(x, "%"))+
      geom_path(aes(date, average), col="darkred", size=1)+
      scale_x_date(date_labels = "%m/%y")+
      ylim(-100, 50)+
      theme_nn()
    
    output$transit_ch <- renderGirafe(
      girafe(ggobj = transit+
               xlab(i18n$t("თვეები"))+
               ylab(i18n$t("ტრანსპორტით გადაადგილება")),
             options = list(opts_tooltip(css = tooltip_css),
                            opts_sizing(width = .7) ) 
      )
    )
    
    # residential <- google_mobility%>%
    #   filter(type == "residential_percent_change_from_baseline")%>%
    #   arrange(desc(date))%>%
    #   mutate(average=rollmean(data, 7, align = "right", fill = NA))%>%
    #   ggplot()+
    #   geom_line(aes(date, average), col="darkred", size=1, lineend = "round")+
    #   geom_col_interactive(aes(date, average, tooltip = paste0(date, ": ", round(average, 2)),
    #                            data_id = average), size=0.4,
    #                        color=NA, fill = "darkred", alpha=0.2)+
    #   scale_y_continuous(labels = function(x) paste0(x, "%"))+
    #   scale_x_date(date_labels = "%m/%y")+
    #   ylim(-100, 50)+
    #   theme_nn()
    # 
    # output$residential_ch <- renderGirafe(
    #   girafe(ggobj = transit+
    #            xlab(i18n$t("თვეები"))+
    #            ylab(i18n$t("საცხოვრებელი ადგილები")),
    #          options = list(opts_tooltip(css = tooltip_css),
    #                         opts_sizing(width = .7) ) 
    #   )
    # )
    
    ### Facebook mobility: three facets for Tbilisi, Batumi, Kutaisi
    
    fb_tbilisi <- fb_mov%>%
      filter(polygon_name %in% c("Tbilisi"))%>%
      select(ds,all_day_bing_tiles_visited_relative_change, 
             polygon_name)%>%
      group_by(polygon_name) %>% 
      mutate(average=rollmean(all_day_bing_tiles_visited_relative_change, 
                              mean, k=7, fill=NA,
                              align = "right", partial=T),
             date=as.Date(ds))%>%
      ggplot()+
      geom_line(aes(date,average), color="darkgreen", size=1)+
      geom_col_interactive(aes(date, average, tooltip = paste0(date, ": ", round(average, 2)),
                               data_id = average), size=0.4,
                           color=NA, fill = "darkgreen", alpha=0.2)+
      
      scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
      scale_x_date(date_labels = "%m/%Y")+
      ylim(-1, 0.5)+
      theme_nn()
    
    output$fb_tbilisi_ch <- renderGirafe(
      girafe(ggobj = fb_tbilisi+
               xlab(i18n$t("თვეები"))+
               ylab(i18n$t("მობილობა")),
             options = list(opts_tooltip(css = tooltip_css),
                            opts_sizing(width = .7) ) 
      )
    )
    
    fb_batumi <- fb_mov%>%
      filter(polygon_name %in% c("Batumi"))%>%
      select(ds,all_day_bing_tiles_visited_relative_change, 
             polygon_name)%>%
      group_by(polygon_name) %>% 
      mutate(average=rollmean(all_day_bing_tiles_visited_relative_change, 
                              mean, k=7, fill=NA,
                              align = "right", partial=T),
             date=as.Date(ds))%>%
      ggplot()+
      geom_line(aes(date,average), color="darkgreen", size=1)+
      geom_col_interactive(aes(date, average, tooltip = paste0(date, ": ", round(average, 2)),
                               data_id = average), size=0.4,
                           color=NA, fill = "darkgreen", alpha=0.2)+
      
      scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
      scale_x_date(date_labels = "%m/%Y")+
      ylim(-1, 0.5)+
      theme_nn()
    
    output$fb_batumi_ch <- renderGirafe(
      girafe(ggobj = fb_batumi+
               xlab(i18n$t("თვეები"))+
               ylab(i18n$t("მობილობა")),
             options = list(opts_tooltip(css = tooltip_css),
                            opts_sizing(width = .7) ) 
      )
    )
    
    fb_kutaisi <- fb_mov%>%
      filter(polygon_name %in% c("Kutaisi"))%>%
      select(ds,all_day_bing_tiles_visited_relative_change, 
             polygon_name)%>%
      group_by(polygon_name) %>% 
      mutate(average=rollmean(all_day_bing_tiles_visited_relative_change, 
                              mean, k=7, fill=NA,
                              align = "right", partial=T),
             date=as.Date(ds))%>%
      ggplot()+
      geom_line(aes(date,average), color="darkgreen", size=1)+
      geom_col_interactive(aes(date, average, tooltip = paste0(date, ": ", round(average, 2)),
                               data_id = average), size=0.4,
                           color=NA, fill = "darkgreen", alpha=0.2)+
      
      scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
      scale_x_date(date_labels = "%m/%Y")+
      ylim(-1, 0.5)+
      theme_nn()
    
    output$fb_kutaisi_ch <- renderGirafe(
      girafe(ggobj = fb_kutaisi+
               xlab(i18n$t("თვეები"))+
               ylab(i18n$t("მობილობა")),
             options = list(opts_tooltip(css = tooltip_css),
                            opts_sizing(width = .7) ) 
      )
    )
    
    output$title_fb <- renderUI({tipify(bsButton("h56", icon("question-circle"),
                                                 size = "default"),
                                        i18n$t("Facebook-ის მობილობის მონაცემები გვიჩვენებს, წინა კვირასთან შედარებით, გაიზარდა თუ შემცირდა ამა თუ იმ კონკრეტულ გეოგრაფიულ არეალში გადაადგილება"))})
    
    output$title_gg <- renderUI({tipify(bsButton("h57", icon("question-circle"),
                                                 size = "default"),
                                        i18n$t("Google-ის მობილობის მონაცემები გვიჩვენებს, წინა კვირასთან შედარებით, გაიზარდა თუ შემცირდა ამა თუ იმ კონკრეტული დანიშნულებით გადაადგილება"))})
    
  })
} ### This ends server part
