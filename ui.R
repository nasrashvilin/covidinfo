library(shiny)
library(shinydashboard)
library(tidyverse)
library(shiny.i18n)
library(ggiraph)
library(rgdal)
library(leaflet)
library(httr)
library(zoo)
library(scales)
library(httr)
library(rvest)
library(shinyBS)
library(readxl)

# library(cronR)
options(shiny.sanitize.errors = FALSE)

i18n <- Translator$new(translation_json_path='translations/translation.json')

i18n$set_translation_language('ქართული')

ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  shiny.i18n::usei18n(i18n),
  tags$div(style = "float: left;"
  ),
  dashboardPage(
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
      menuItem(h5(i18n$t("თავფურცელი"), icon="search", style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "home"),
      menuItem(h5(i18n$t("ჯამური მონაცემები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "total_data"),
      menuItem(h5(i18n$t("ტესტები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "tests"),
      menuItem(h5(i18n$t("ჰოსპიტალიზაცია"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "hospitalization"),
	    #sidebar for occupied territories
      menuItem(h5(i18n$t("ოკუპირებული რეგიონები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "occupied_territories"),
      menuItem(h5(i18n$t("სხვა ინდიკატორები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "other_indicators"),
      # menuItem(h5(i18n$t("შედარება"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
      #          tabName = "comparison"),
      menuItem(h5(i18n$t("მონაცემების წყარო"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
               tabName = "data_source")
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
  # tags$link(rel="shortcut icon", href="favicon.png"),
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
  tags$style(HTML(
    '.tabbable > .nav > li > a                  {color:black; font-family: "BPG_upper"}'
  ))
),
# tags$span(textOutput("updated_at"), style = "
#               position:absolute;
#               bottom:0;
#               width:100%;
#               height:10px;
#               color: black;
#               padding: 10px;
#               font-family: 'BPG_lower';
#               z-index: 1000;"),
shinydashboard::tabItems(
tabItem(
    tabName = "home",
    fluidRow(
      shinydashboard::box(
        title=h5(textOutput("todays_date_out"), style="font-size: '4px'; font-family: 'BPG_upper';"),
        width = 12,
        infoBoxOutput("new_cases"),
        valueBoxOutput("new_deaths"),
        valueBoxOutput("new_recoveries"),
        valueBoxOutput("new_hospitalization"),
        valueBoxOutput("new_active"),
        valueBoxOutput("new_critical"),
        valueBoxOutput("new_tests"),
        valueBoxOutput("new_r"),
        valueBoxOutput("new_innoculated"),
        h6(textOutput("updated_at"), style="font-size: '2px'; font-family: 'BPG_lower';")
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
      shinydashboard::box(
        title=h5(textOutput("todays_date"), style="font-size: '4px'; font-family: 'BPG_upper';"),
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
        tabPanel(h5(i18n$t("გამოჯანმრთელდა"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("tot_cumulative_recovered_ch", height = 400, width=1000)),
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
      tabPanel(h5(i18n$t("დადებითი ტესტების წილი, %"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("tot_positive_ratio_ch", height = 400)),
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
  tabName = "occupied_territories",
  fluidRow(
    tabBox(
      height = "700", #ზომები რომ გავზარდოთ და გვერდზე ჰორიზონტალური სივრცე სრულად აითვისოს?
      width = 25,
      tabPanel(h5(i18n$t("ოკუპირებული რეგიონები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), 
	       girafeOutput("occupied_territories", height = 400)),
      )
    
  )
),
tabItem(
  tabName = "other_indicators",
  fluidRow(
    shinydashboard::box(
      width=6,
      title=h5(i18n$t("რეპროდუქციის ინდექსი"), style="font-size: '4px'; font-family: 'BPG_upper';  text-transform: uppercase;"),
      i18n$t("რეპროდუქციის ეფექტური ინდექსი (R) წარმოადგენს ე.წ. მეორადი შემთხვევების რაოდენობას, რომელიც დაკავშირებულია დაავადების თითოეულ უკვე არსებულ შემთხვევას. დაავადებულთა რაოდენობის შესამცირებლად, R 1-ზე ნაკლები უნდა იყოს"),
      girafeOutput("tracking_r_output", height = 400)
    ),
    shinydashboard::box(
      width=6,
      title=h5(i18n$t("სიმკაცრის ინდექსი"), style="font-size: '4px'; font-family: 'BPG_upper';  text-transform: uppercase;"),
      i18n$t("სიმკაცრის ინდექსი წარმოადგენს რიცხვს, რომელიც გვიჩვენებს, თუ რამდენად მკაცრია ვირუსთან დაკავშირებული შეზღუდვები. 100-თან ახლოს მყოფი მნიშვნელობები მკაცრ შეზღუდვებზე მიანიშნებს"),
      girafeOutput("tracking_stringency_output", height = 400)
    ),
    tabBox(
      title = uiOutput("title_fb"),
      height = "500",
      width = 6,
      tabPanel(h5(i18n$t("თბილისი"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("fb_tbilisi_ch", height = 400)),
      tabPanel(h5(i18n$t("ბათუმი"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("fb_batumi_ch", height = 400)),
      tabPanel(h5(i18n$t("ქუთაისი"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("fb_kutaisi_ch", height = 400))
    ),
    tabBox(
      title = uiOutput("title_gg"),
      height = "500",
      width = 6,
      tabPanel(h5(i18n$t("საკვები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("grocs_ch", height = 400)),
      tabPanel(h5(i18n$t("პარკი"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("parks_ch", height = 400)),
      tabPanel(h5(i18n$t("საყიდლები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("ret_rec_ch", height = 400)),
      tabPanel(h5(i18n$t("სამუშაო"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("workplace_ch", height = 400)),
      tabPanel(h5(i18n$t("ტრანსპორტი"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("transit_ch", height = 400))
      # tabPanel(h5(i18n$t("შინ"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"), girafeOutput("residential_ch", height = 400))
    )
  )
),
# tabItem(
#   tabName = "comparison",
#   fluidRow()
# ),
tabItem(tabName = "data_source",
        fluidRow(
        # includeMarkdown("www/methods.Rmd")
          shinydashboard::box(
            title=h5(i18n$t("მონაცემები დაავადების გავრცელების შესახებ"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
            width = 12,
            i18n$t("საქართველოს მთავრობა"), br(), i18n$t("საქართველოს დაავადებათა კონტროლის ეროვნული ცენტრი")
            ),
          shinydashboard::box(
            title=h5(i18n$t("რეპროდუქციის ინდექსი"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
            width = 12,
            "Arroyo-Marioli F, Bullano F, Kucinskas S, Rondón-Moreno C (2021) Tracking of COVID-19: A new real-time estimation using the Kalman filter. PLoS ONE 16(1): e0244474. https://doi.org/10.1371/journal.pone.0244474"
          ),
          shinydashboard::box(
            title=h5(i18n$t("სიმკაცრის ინდექსი"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
            width = 12,
            "The Oxford COVID-19 Government Response Tracker (OxCGRT)"
          ),
          shinydashboard::box(
            title=h5(i18n$t("მობილობა (Facebook)"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
            width = 12,
            "https://data.humdata.org/dataset/movement-range-maps"
          ),
          shinydashboard::box(
            title=h5(i18n$t("მობილობა (Google)"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
            width = 12,
            "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
          ),
          shinydashboard::box(
            title=h5(i18n$t("რეპლიკაციის კოდი"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
            width = 12,
            "https://github.com/davidsichinava/covidinfo"
          ),
          shinydashboard::box(
            title=h5(i18n$t("პროექტის ავტორები"), style="font-size: '4px'; font-family: 'BPG_upper'; text-transform: uppercase;"),
            width = 12,
            i18n$t("დავით სიჭინავა"), ": @davidsichinava, ", i18n$t("ნიკა ნასრაშვილი"), ": @nasrashvilin"
          )
        )
)
),
),
),
)
