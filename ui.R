library(shiny)
library(shinydashboard)
library(tidyverse)
library(shiny.i18n)
library(ggiraph)
library(rgdal)
library(leaflet)
library(httr)
library(zoo)


# library(cronR)

i18n <- Translator$new(translation_json_path='translations/translation.json')

i18n$set_translation_language('ქართული')

## setwd("D:\\Dropbox\\My projects\\scratchpad\\cov_shiny\\server")

url <- "https://www.dropbox.com/scl/fi/5f734v40u5t8pogvvcbpa/covid_data_georgia.xlsx?dl=1&rlkey=iwbmp1y34u30e9bri8n7y5hku"

httr::GET(url, write_disk(path = "www/src.xlsx", overwrite = T))

# httr::GET(url, write_disk(path = "www/src.xlsx", overwrite = T))

src <- readxl::read_excel("www/src.xlsx", sheet=1)

todays_date <- src %>%
    mutate(lubridate::as_date(date))%>%
    arrange(desc(date))%>%
    slice(1)%>%
    pull(date)

ui <- fluidPage(
  shiny.i18n::usei18n(i18n),
  tags$div(style = "float: left;"
  ),
  dashboardPage(
  
#  shiny.i18n::usei18n(i18n),
#  tags$div(style = "float: right;",
#      selectInput('selected_language',
#                  i18n$t("Change language"),
#                  choices = i18n$get_languages(),
#                  selected = i18n$get_key_translation())
#  ),
  title="კორონავირუსი საქართველოში",
  shinydashboard::dashboardHeader(
    title = span(
      tags$img(src="img/header.png", width = '110%'), 
      column(12, class="title-box", 
             tags$h1(class="primary-title", style='margin-top:25px; margin-bottom: 25px; font-size:24px; text-transform: uppercase;',
                     i18n$t("კორონავირუსი საქართველოში"))))),
  
  shinydashboard::dashboardSidebar(
    sidebarMenu(
      selectInput('selected_language',
                  i18n$t("ენის გადართვა"),
                  choices = i18n$get_languages(),
                  selected = i18n$get_key_translation()),
      menuItem(h5(i18n$t("თავფურცელი"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "home"),
      menuItem(h5(i18n$t("ჯამური მონაცემები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "total_data"),
      menuItem(h5(i18n$t("ტესტები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "tests"),
      menuItem(h5(i18n$t("ჰოსპიტალიზაცია"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "hospitalization"),
      menuItem(h5(i18n$t("სხვა ინდიკატორები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "other_indicators"),
      menuItem(h5(i18n$t("შედარება"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "comparison"),
      menuItem(h5(i18n$t("მონაცემების წყარო"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "methodology")
    )
  ),
  shinydashboard::dashboardBody(
  tags$style(type="text/css", "
    .content-wrapper {
        margin-top: 20px;
    }
    .content {
        padding-top: 30px;
    }
    .title-box {
        position: absolute;
        text-align: center;
        top: 50%;
        left: 50%;
        transform:translate(-50%, -50%);
    }
    @media (max-width: 590px) {
        .title-box {
            position: absolute;
            text-align: center;
            top: 10%;
            left: 10%;
            transform:translate(-5%, -5%);
        }
    }
    @media (max-width: 767px) {
        .primary-title {
            font-size: 1.2em;
        }
        .primary-subtitle {
            font-size: 1.1em;
        }
    }
/*    Make the image taller */
    .main-header .logo {
        height: 100px;
		padding-left: 0px;
    }
/*    Override the default media-specific settings */
    @media (max-width: 5000px) {
        .main-header {
            padding: 0 0;
            position: relative;
        }
        .main-header .logo,
        .main-header .navbar {
            width: 100%;
            float: none;
        }
        .main-header .navbar {
            margin: 0;
        }
        .main-header .navbar-custom-menu {
            float: right;
        }
    }
/*    Move the sidebar down */
    .main-sidebar {
        position: absolute;
    }
    .left-side, .main-sidebar {
        padding-top: 170px;
    }
    .content {
        padding-top: 50px;
    }"
  ),

tags$head(
  # Custom CSS
  tags$link(rel="shortcut icon", href="favicon.png"),
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
  tags$style(HTML(
    '.tabbable > .nav > li > a                  {color:black; font-family: "BPG_upper"}'
  ))
),
shinydashboard::tabItems(
tabItem(
    tabName = "home",
    fluidRow(
      box(
        title=h5(todays_date, style="font-size: '4px'; font-family: 'BPG_upper';"),
        
        width = 12,
        infoBoxOutput("new_cases"),
        valueBoxOutput("new_deaths"),
        valueBoxOutput("new_recoveries"),
        valueBoxOutput("new_hospitalization"),
        valueBoxOutput("new_active"),
        valueBoxOutput("new_critical"),
        valueBoxOutput("new_tests"),
        valueBoxOutput("new_r"),
        valueBoxOutput("new_innoculated")
      )
    ),
    fluidRow(
      tabBox(
        height = "500",
        tabPanel(h5(i18n$t("ახალი შემთხვევები სულ"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), leafletOutput("today_map")),
        tabPanel(h5(i18n$t("ახალი შემთხვევები 1000 მოსახლეზე"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), leafletOutput("today_map_pc"))
      ),
      tabBox(
        height = "500",
        tabPanel(h5(i18n$t("ახალი შემთხვევა"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("daily_cases_chart", height = 400)),
        tabPanel(h5(i18n$t("გამოჯანმრთელდა"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("recov_ts_chart", height = 400)),
        tabPanel(h5(i18n$t("გარდაიცვალა"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("deaths_ts_chart", height = 400))
      )
    ),
  ),
tabItem(
    tabName = "total_data",
    fluidRow(
      box(
        title=h5(todays_date, style="font-size: '4px'; font-family: 'BPG_upper';"),
        width = 12,
        valueBoxOutput("cumulative_cases"),
        valueBoxOutput("cumulative_deaths"),
        valueBoxOutput("cumulative_recoveries"),
        valueBoxOutput("pop_share"),
        valueBoxOutput("cumulative_tests"),
        valueBoxOutput("cumulative_innoculated")
      ),
      tabBox(
        height = "500",
        width = 12,
        tabPanel(h5(i18n$t("შემთხვევები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("tot_cumulative_cases_ch", height = 400)),
        tabPanel(h5(i18n$t("გამოჯანმრთელდა"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("tot_cumulative_recovered_ch", height = 400)),
        tabPanel(h5(i18n$t("გარდაიცვალა"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("tot_cumulative_died_ch", height = 400))
      )
    )
),
tabItem(
  tabName = "tests",
  fluidRow(
    tabBox(
      height = "500",
      width = 12,
      tabPanel(h5(i18n$t("ყოველდღიურად"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("tests_ts_chart", height = 400)),
      tabPanel(h5(i18n$t("ჯამურად"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("tot_cumulative_tests_ch", height = 400)),
      tabPanel(h5(i18n$t("დადებითი წილი"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("tot_positive_ratio_ch", height = 400)),
      tabPanel(h5(i18n$t("PCR"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("tot_pcr_tests_ch", height = 400)),
      tabPanel(h5(i18n$t("სწრაფი"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("tot_rapid_tests_ch", height = 400))
    )
  )
),
tabItem(
  tabName = "hospitalization",
  fluidRow(
    tabBox(
      height = "500",
      width = 12,
      tabPanel(h5(i18n$t("ჰოსპიტალიზაცია"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("tot_hospitalized_ch", height = 400)),
      tabPanel(h5(i18n$t("ჰოსპიტალიზაცია, 1000 მოსახლეზე"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("thous_hospitalized_ch", height = 400)),
      tabPanel(h5(i18n$t("მძიმე პაციენტები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("critical_ch", height = 400)),
      tabPanel(h5(i18n$t("პაციენტები მართვით სუნთქვაზე"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("ventil_ch", height = 400))
    )
    
  )
),
tabItem(
  tabName = "other_indicators",
  fluidRow(
    box(
      width=6,
      title=h5(i18n$t("რეპროდუქციის ინდექსი"), style="font-size: '4px'; font-family: 'BPG_upper';  text-transform: uppercase;"),
      "რეპროდუქციის ეფექტური ინდექსი (R) წარმოადგენს ე.წ. მეორადი შემთხვევების რაოდენობას, რომელიც დაკავშირებულია დაავადების თითოეულ უკვე არსებულ შემთხვევას. დაავადებულთა რაოდენობის შესამცირებლად, R 1-ზე ნაკლები უნდა იყოს",
      girafeOutput("tracking_r_output", height = 400)
    ),
    box(
      width=6,
      title=h5(i18n$t("სიმკაცრის ინდექსი"), style="font-size: '4px'; font-family: 'BPG_upper';  text-transform: uppercase;"),
      "სიმკაცრის ინდექსი წარმოადგენს რიცხვს, რომელიც გვიჩვენებს, რამდენად მკაცრია თუ არა ვირუსთან დაკავშირებული შეზღუდვები. 100-თან ახლოს მყოფი მნიშვნელობები მკაცრ შეზღუდვებზე მიანიშნებს",
      girafeOutput("tracking_stringency_output", height = 400)
    )
  )
),
tabItem(
  tabName = "comparison",
  fluidRow()
),
tabItem(tabName = "methodology",
        includeMarkdown("www/methods.Rmd")
        
)
),
)
))