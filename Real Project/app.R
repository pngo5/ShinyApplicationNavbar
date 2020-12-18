library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DT)
library(scales)
library(tidyverse)
library(leaflet)
library(fs)
library(htmltools)
library(wbstats)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(rsconnect)





#Reused some of the code from these github
#https://github.com/chschoenenberger/covid19_dashboard
#https://github.com/zappingseb/coronashiny


#Load data
root = "https://raw.githubusercontent.com/"
repo = "CSSEGISandData/COVID-19"
folder = "/master/csse_covid_19_data/csse_covid_19_time_series/"

url_data_folder = str_c(root, repo, folder, sep="")
#
url_confirmed_US = str_c(
    url_data_folder, "time_series_covid19_confirmed_US.csv", sep="")
url_confirmed_global = str_c(
    url_data_folder, "time_series_covid19_confirmed_global.csv", sep="")
url_death_US = str_c(
    url_data_folder, "time_series_covid19_deaths_US.csv", sep="")
url_death_global = str_c(
    url_data_folder, "time_series_covid19_deaths_global.csv", sep="")
url_recovered_global = str_c(
    url_data_folder, "time_series_covid19_recovered_global.csv", sep="")

#-------------------------------------------------
#Sources for re-using codes 
#https://stackoverflow.com/questions/59778337/how-to-pivot-longer-a-set-of-multiple-columns-and-how-to-go-back-from-that-long
#https://cran.r-project.org/web/packages/tidyr/vignettes/pivot.html
#https://stackoverflow.com/questions/3402371/combine-two-data-frames-by-rows-rbind-when-they-have-different-sets-of-columns

dataFile<-read_csv(url_confirmed_global)
current_date<- as.Date(names(dataFile)[ncol(dataFile)], format="%m/%d/%y")






confirmed_sub <- read_csv(url_confirmed_global)
confirmed_sub <- confirmed_sub %>%
  gather("date","value",which(colnames(confirmed_sub)=="1/22/20"):ncol(confirmed_sub))%>%
  group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
  summarise("confirmed" = sum(value, na.rm = T))

recovered_sub <-read_csv(url_recovered_global)
recovered_sub <- recovered_sub %>%
  gather("date","value",which(colnames(recovered_sub)=="1/22/20"):ncol(recovered_sub))%>%
  group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
  summarise("recovered" = sum(value, na.rm = T))

deceased_sub <- read_csv(url_death_global)
deceased_sub <- deceased_sub %>%
  gather("date","value",which(colnames(deceased_sub)=="1/22/20"):ncol(deceased_sub))%>%
  group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
  summarise("death" = sum(value, na.rm = T))
#-------------------------------------------------
confirmed_sub_us <- read_csv(url_confirmed_US)
confirmed_sub_us <- confirmed_sub_us %>%
  select(Province_State, Country_Region, Lat, Long_, 12:ncol(confirmed_sub_us)) %>%
  rename(`Province/State` = Province_State, `Country/Region` = Country_Region, Long = Long_) %>%
  gather("date","value",which(colnames(confirmed_sub_us)=="1/22/20"):ncol(confirmed_sub_us)-7)%>%
  group_by(`Province/State`, `Country/Region`, date) %>%
  mutate(
    Lat  = na_if(Lat, 0),
    Long = na_if(Long, 0)
  ) %>%
  summarise(
    "Lat"       = mean(Lat, na.rm = T),
    "Long"      = mean(Long, na.rm = T),
    "confirmed" = sum(value, na.rm = T)
  )
deceased_sub_us <-read_csv(url_death_US)
deceased_sub_us <- deceased_sub_us %>%
  select(Province_State, Country_Region, 13:(ncol(deceased_sub_us))) %>%
  rename(`Province/State` = Province_State, `Country/Region` = Country_Region) %>%
  pivot_longer(names_to = "date", cols = 5:(ncol(deceased_sub_us) - 11)) %>%
  group_by(`Province/State`, `Country/Region`, date) %>%
  summarise("death" = sum(value, na.rm = T))

cases_us <- confirmed_sub_us %>%
  full_join(deceased_sub_us) %>%
  add_column(recovered = NA) %>%
  select(`Province/State`, `Country/Region`, date, Lat, Long, confirmed, recovered, death)
#Sources
#https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/join
#https://www.guru99.com/r-dplyr-tutorial.html
#https://medium.com/@HollyEmblem/joining-data-with-dplyr-in-r-874698eb8898
#https://community.rstudio.com/t/new-variable-with-mutate-and-ifelse/26727
#https://stackoverflow.com/questions/19253820/how-to-implement-coalesce-efficiently-in-r

#Used these website to get Update_data working
Update_data <- confirmed_sub %>%
  full_join(recovered_sub) %>%
  full_join(deceased_sub) %>%
  rbind(cases_us) %>%
  ungroup() %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  arrange(date) %>%
  group_by(`Province/State`, `Country/Region`, Lat, Long) %>%
  fill(confirmed, recovered, death) %>%
  replace_na(list(death = 0, confirmed = 0)) %>%
  mutate(
    recovered_est = lag(confirmed, 14, default = 0) - death,
    recovered_est = ifelse(recovered_est > 0, recovered_est, 0),
    recovered     = coalesce(recovered, recovered_est),
    active        = confirmed - recovered - death
  ) %>%
  select(-recovered_est) %>%
  pivot_longer(names_to = "var", cols = c(confirmed, recovered, death, active)) %>%
  filter(!(is.na(`Province/State`) && `Country/Region` == "US")) %>%
  filter(!(Lat == 0 & Long == 0)) %>%
  ungroup()
# Updated _data 
Update_data <- Update_data %>%
  group_by(`Province/State`, `Country/Region`) %>%
  mutate(value_new = value - lag(value, 4, default = 0)) %>%
  ungroup()

#-------------------------------------------------
confirmed_us_df  <-  read_csv(url_confirmed_US)
confirmed_us_df  <-  confirmed_us_df %>%
    gather(which(colnames(confirmed_us_df)=="1/22/20"):ncol(confirmed_us_df),
           key = "date", value = "confirmed") %>%
    mutate(date = mdy(date)) %>%
    rename(region = "Country_Region",
           sub_region = "Province_State")


confirmed_global_df = read_csv(url_confirmed_global)
confirmed_global_df <- confirmed_global_df %>% 
    gather(which(colnames(confirmed_global_df)=="1/22/20"):ncol(confirmed_global_df), 
           key = "date", value = "confirmed") %>% 
    mutate(date = mdy(date)) %>%
    rename(Province_State = "Province/State",
           Country_Region = "Country/Region")

death_global_df = read_csv(url_death_global)
death_global_df <-  death_global_df %>% 
    gather(which(colnames(death_global_df)=="1/22/20"):ncol(death_global_df), 
           key = "date", value = "death") %>% 
    mutate(date = mdy(date)) %>% 
    rename(Province_State = "Province/State",
           Country_Region = "Country/Region")

recovered_global_df = read_csv(url_recovered_global)
recovered_global_df <-  recovered_global_df %>% 
    gather(which(colnames(recovered_global_df)=="1/22/20"):ncol(recovered_global_df), 
           key = "date", value = "recovered") %>% 
    mutate(date = mdy(date)) %>% 
    rename(Province_State = "Province/State",
           Country_Region = "Country/Region")
#----------------------------------------------------------------------------------------------------
#Everything above is for the map!
#----------------------------------------------------------------------------------------------------
global_df0 <- merge(x=confirmed_global_df,y=recovered_global_df,by=c("Province_State","Country_Region", "date"),all.x=TRUE, all.y=TRUE)
global_df0 <- merge(x=global_df0,y=death_global_df,by=c("Province_State","Country_Region", "date"),all.x=TRUE, all.y=TRUE)

global_df0 <- global_df0 %>% 
    rename(sub_region = "Province_State",
           region = "Country_Region")

# ---- Download Total_population_dw data ----
Total_population_dw <- wb(country = "countries_only", indicator = "SP.POP.TOTL", startdate = 2019, enddate = 2020) %>%
    select(country, value) %>%
    rename(Total_population_dw = value)
pops_names <- c("Brunei Darussalam",
                        "Congo, Dem. Rep.", "Congo, Rep.", "Czech Republic",
                        "Egypt, Arab Rep.", "Iran, Islamic Rep.", "Korea, Rep.", 
                        "St. Lucia", "West Bank and Gaza", "Russian Federation",
                        "Slovak Republic", "United States", "St. Vincent and the Grenadines", 
                        "Venezuela, RB")
atls_names <- c("Brunei", "Congo (Kinshasa)", "Congo (Brazzaville)",
                        "Czechia", "Egypt", "Iran", "Korea, South",
                        "Saint Lucia", "occupied Palestinian territory", 
                        "Russia", "Slovakia", "US", "Saint Vincent and the Grenadines",
                        "Venezuela")
Total_population_dw[which(Total_population_dw$country %in% pops_names), "country"] <- atls_names



Update_data <- Update_data %>%
    left_join(Total_population_dw, by = c("Country/Region" = "country"))


date_data <- function(inputDate) {
    Update_data[which(Update_data$date == inputDate),] %>%
        distinct() %>%
        pivot_wider(id_cols = c("Province/State", "Country/Region", "date", "Lat", "Long", "Total_population_dw"), names_from = var, values_from = value) %>%
        filter(confirmed > 0 |
                   recovered > 0 |
                   death > 0 |
                   active > 0)
}

Update_latest <- date_data(max(Update_data$date))

# Define UI for application that draws a histogram
#https://rstudio.github.io/shinythemes/
ui={ page<-navbarPage(
  
        theme = shinytheme("cosmo"),
        title = "Covid-19",
        tabPanel("Interactive Data",icon =icon("digital-tachograph"),
             sidebarLayout(
                 sidebarPanel(width = 3,
                              selectizeInput("region", "Region: ", choices = unique(global_df0$region), multiple = T),
                              dateRangeInput("dates", "Date range: ", start = min(global_df0$date), end = max(global_df0$date))
                 ),
                 mainPanel(
                     fluidRow(width = 12,
                              column(width = 4, "Total confirmed case", 
                                     verbatimTextOutput("confirmed_case")),
                              column(width = 4, "Total recovered case",
                                     verbatimTextOutput("recovered_case")),
                              column(width = 4, "Total deaths",
                                     verbatimTextOutput("death_case"))
                     ), br(),
                     # verbatimTextOutput("text"),
                     h3("Number of confirmed, recovered and death cases"),
                     DT::dataTableOutput("tabledownload"),
                     #dataTableOutput("table"), br(),
                     h3("Top 5 region with confirmed case"),
                     plotOutput("plot"),
                     downloadButton('imagedownloadinteractive','Download')
                     
                     # plotOutput("distPlot")
                 )
             )
             
        ),
                    tabPanel("Graphs",icon =icon("chart-bar"),
                             tabsetPanel(
                               tabPanel("Most Deaths",
                                        h3("Top 5 countries with the most deaths"),
                                        plotOutput("Q1"),
                                        downloadButton('downloadPlot','Download')
                                       #h3("US first recovered case"),
                                        #verbatimTextOutput("Q5")
                                        ),
                               tabPanel("Confirmed Case",
                                        h3("Monthly trend of confirmed case in Georgia"),
                                        plotOutput("Q3"),
                                        downloadButton('downloadPlot2','Download'))
                          

                        )
                      ),
        
                      tabPanel("Table",icon =icon("table"),
                               tabsetPanel(
                                 tabPanel("US States",
                                        h3("US States with > 1000 cases in Jun 2020"),
                                        DT::dataTableOutput("table_out")),
                                        #dataTableOutput("Q2"), br(),
                                        #downloadButton('downloadData', 'cvs'),
                                        #actionButton('downloadDatapdf', 'pdf')),
                                      
                                       
                                
                                   tabPanel("Recovery Rate",   
                                        h3("Countries with the highest recovery rate"),
                                        DT::dataTableOutput("table_out2"))
                                        #dataTableOutput("Q4"), br(),
                                        #downloadButton('downloadData2', 'Download data'))
                                        
                                        
                                 )
                               ),
                      tabPanel("Map",icon = icon("map-marked-alt"),
                               fluidRow( 
                             #  fluidRow(
                                # uiOutput("box_keyFigures"),
                                # style = 'padding:50px'
                      
                             #  ),
                               fluidRow(  
                                 column(
                                   box(
                                     width = 12,
                                     leafletOutput("map_view")
                                   ),
                                   class = "map",
                                   width = 8,
                                   style = 'padding:0px;'
                                   
                                 ),
                                 column(
                                   uiOutput("summaryTables"),
                                   class = "summary",
                                   width = 4,
                                   style = 'padding:0px;'
                                 ),
                                 column(
                                   sliderInput(
                                     "timeSlider",
                                     label      = " ",
                                     min        = min(Update_data$date),
                                     max        = max(Update_data$date),
                                     value      = max(Update_data$date),
                                     width      = "100%",
                                     timeFormat = "%d.%m.%Y",
                                     animate    = animationOptions(loop = TRUE)
                                   ),
                                   class = "slider",
                                   color = "#ffc04d",
                                   width = 12,
                                   style = 'padding-left:15px; padding-right:15px;'
                                 )
                               )
                            )
                        )

                    )

                        page[[3]][[1]]$children[[1]]$children[[2]]$children[[1]]$children[[1]] <- 
                          tags$li(tags$a(target="_blank",
                            href = 'https://github.com/pngo5/ShinyApplicationNavbar', 
                            icon("github-square"),
                            "Github"
                          )
                    )
                                                
          page

}

# Service logic
server <- function(input, output, session) {
    # Server: Map function ------------------------------------------------------- 
    # Adding a function that will get the confirmed cases,deceased, recovered, and active.  
    addLabel <- function(data) {
        data$label <- paste0(
            '<b>', ifelse(is.na(data$`Province/State`), data$`Country/Region`, data$`Province/State`), '</b><br>
   <table style="width:120px;">
    <tr><td>Confirmed:</td><td align="right">', data$confirmed, '</td></tr>
    <tr><td>Deceased:</td><td align="right">', data$death, '</td></tr>
    <tr><td>Estimated Recoveries:</td><td align="right">', data$recovered, '</td></tr>
    <tr><td>Active:</td><td align="right">', data$active, '</td></tr>
    </table>'
        )
        data$label <- lapply(data$label, HTML)
        
        return(data)
    }
    # Server: Core  -------------------------------------------------------   
    global_df <- reactive({
        if(!is.null(input$region)){
            global_df <- global_df0 %>%
                filter(region %in% input$region)
        } else {
            global_df <-global_df0
        } 
        
        global_df <-  global_df %>% 
            filter( date >= input$dates[1] & date <= input$dates[2])
        return(global_df)
    })
    
    output$confirmed_case <- renderText({
        # input$dates[1]
        global_df <- global_df()
        format(sum(global_df$confirmed[global_df$date == max(global_df$date)], na.rm = T),big.mark = ",")
    })
    
    output$recovered_case <- renderText({
        # input$dates[2]
        global_df <- global_df()
        format(sum(global_df$recovered[global_df$date == max(global_df$date)], na.rm = T), big.mark = ",")
    })
    
    output$death_case <- renderText({
        global_df <- global_df()
        format(sum(global_df$death[global_df$date == max(global_df$date)], na.rm = T), big.mark = ",")
    })

    output$table <- renderDataTable({
        global_df <- global_df()
        global_df %>%
            group_by(sub_region, region) %>%
            summarize(confirmed = max(confirmed),
                      recovered = max(recovered),
                      death = max(death)) %>% 
            arrange(region)
    })
    
    
    
    
    
    
    output$plot <- renderPlot({
        global_df <- global_df()
        global_df %>% 
            group_by(region) %>%
            summarize(confirmed = max(confirmed)) %>% 
            arrange(desc(confirmed)) %>% 
            top_n(5) %>% 
            ggplot(aes(x = reorder(region, -confirmed, sum), y = confirmed)) + geom_col(fill = "#ffc04d") + xlab("region") + scale_y_continuous(labels = comma_format(big.mark = ","))
    })
    
    # Server: Menu -------------------------------------------------------
    
    #"Q1. Top 5 countries with the most deaths",
    output$Q1 <- renderPlot({
        global_df0 %>% 
            group_by(region) %>% 
            summarize(death = max(death)) %>% 
            arrange(desc(death)) %>% 
            top_n(5) %>% 
            ggplot(aes(x = reorder(region, -death, sum), y = death)) + geom_col(fill = "#ffc04d") + xlab("countries") + scale_y_continuous(labels = comma_format(big.mark = ","))
    })
    
    # "Q2. US States with > 1000 cases in Jun 2020",
    output$Q2 <- renderDataTable({
        confirmed_us_df %>% 
            filter(region == "US") %>% 
            filter(date == as.Date("2020-05-31") | date == as.Date("2020-06-30")) %>% 
            group_by(sub_region) %>% 
            summarize(new_confirmed = max(confirmed) - min(confirmed)) %>%
            arrange(desc(new_confirmed)) %>% 
            filter(new_confirmed > 1000)
    })
    
    # "Q3. Monthly trend of confirmed case in Georgia",
    output$Q3<-d1 <- renderPlot({
        confirmed_us_df %>% 
            mutate(month = month(date)) %>% 
            filter(region == "US" & sub_region == "Georgia") %>% 
            group_by(month) %>% 
            summarize(
                confirmed = max(confirmed)
            ) %>% 
            ggplot(aes(x = month, y = confirmed)) + geom_point(size = 2) + geom_line(size = 1) + scale_y_continuous(labels = comma_format(big.mark = ","))
    })
    
    # "Q4. Countries with the highest recovery rate",
    output$Q4 <- renderDataTable({
        global_df0 %>% 
            filter(date == max(global_df0$date)) %>% 
            filter(confirmed > 1000000) %>% 
            group_by(region) %>% 
            summarize(recovery_rate = round(recovered/confirmed, 2)) %>% 
            arrange(desc(recovery_rate)) 
    })
    
    # "Q5. US first recovered case",
    output$Q5 <- renderPrint({
        filter_df <- global_df0 %>% 
            filter(region == "US" & recovered > 0) %>% 
            arrange(date) %>% 
            # top_n(1) %>% 
            select(date)
        
        print(format(filter_df$date[1], "%d %B %Y"))
    })
    # Server: Map output -------------------------------------------------------
    #https://rstudio.github.io/leaflet/basemaps.html
    #https://rstudio.github.io/leaflet/popups.html
    #https://github.com/kcredit/R-Leaflet-Shiny-Map
    #https://github.com/Robinlovelace/Creating-maps-in-R/blob/master/vignettes/vspd-base-shiny.md
    #https://gist.github.com/ramnathv/6144287
    map <- leaflet(addLabel(Update_latest)) %>%
        setMaxBounds(-180, -90, 180, 90) %>%
        setView(0, 20, zoom = 2) %>%
        addTiles() %>%
        addProviderTiles(providers$Stamen.Toner, group = "Dark") %>%
        addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
        addLayersControl(
            baseGroups    = c("Dark", "Satellite"),
            overlayGroups = c("Confirmed","Confirmed (per capita)", "Estimated Recoveries", "Deceased", "Active", "Active (per capita)")
        ) %>%
      hideGroup("Confirmed (per capita)") %>%
      hideGroup("Estimated Recoveries") %>%
      hideGroup("Deceased") %>%
      hideGroup("Active") %>%
      hideGroup("Active (per capita)") %>%
      addEasyButton(easyButton(
        icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
        onClick = JS("function(btn, map){ map.setView([20, 0], 2); }"))) %>%
      addEasyButton(easyButton(
        icon    = "glyphicon glyphicon-map-marker", title = "Locate Me",
        onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 6}); }")))
    
    observe({
        req(input$timeSlider, input$map_view_zoom)
        zoomLevel               <- input$map_view_zoom
        data                    <- date_data(input$timeSlider) %>% addLabel()
        data$confirmedPerCapita <- data$confirmed / data$Total_population_dw * 100000
        data$activePerCapita    <- data$active / data$Total_population_dw * 100000
        
        leafletProxy("map_view", data = data) %>%
            clearMarkers() %>%
            addCircleMarkers(
                lng          = ~Long,
                lat          = ~Lat,
                radius       = ~log(confirmed^(zoomLevel / 2)),
                stroke       = FALSE,
                fillOpacity  = 0.5,
                color        = "#ffc04d",
                label        = ~label,
                labelOptions = labelOptions(textsize = 15),
                group        = "Confirmed"
            ) %>%
          addCircleMarkers(
            lng          = ~Long,
            lat          = ~Lat,
            radius       = ~log(confirmedPerCapita^(zoomLevel)),
            stroke       = FALSE,
            color        = "#00b3ff",
            fillOpacity  = 0.5,
            label        = ~label,
            labelOptions = labelOptions(textsize = 15),
            group        = "Confirmed (per capita)"
          ) %>%
          addCircleMarkers(
            lng          = ~Long,
            lat          = ~Lat,
            radius       = ~log(recovered^(zoomLevel)),
            stroke       = FALSE,
            color        = "#005900",
            fillOpacity  = 0.5,
            label        = ~label,
            labelOptions = labelOptions(textsize = 15),
            group = "Estimated Recoveries"
          ) %>%
          addCircleMarkers(
            lng          = ~Long,
            lat          = ~Lat,
            radius       = ~log(death^(zoomLevel)),
            stroke       = FALSE,
            color        = "#E7590B",
            fillOpacity  = 0.5,
            label        = ~label,
            labelOptions = labelOptions(textsize = 15),
            group        = "Deceased"
          ) %>%
          addCircleMarkers(
            lng          = ~Long,
            lat          = ~Lat,
            radius       = ~log(active^(zoomLevel / 2)),
            stroke       = FALSE,
            color        = "#f49e19",
            fillOpacity  = 0.5,
            label        = ~label,
            labelOptions = labelOptions(textsize = 15),
            group        = "Active"
          ) %>%
          addCircleMarkers(
            lng          = ~Long,
            lat          = ~Lat,
            radius       = ~log(activePerCapita^(zoomLevel)),
            stroke       = FALSE,
            color        = "#f4d519",
            fillOpacity  = 0.5,
            label        = ~label,
            labelOptions = labelOptions(textsize = 15),
            group        = "Active (per capita)"
          )
    })
    
    output$map_view <- renderLeaflet(map)
    #--------------------------------------------
    #Table Map
 
    
    
    sumData <- function(date) {
      if (date >= min(Update_data$date)) {
        data <- date_data(date) %>% summarise(
          confirmed = sum(confirmed, na.rm = T),
          recovered = sum(recovered, na.rm = T),
          death  = sum(death, na.rm = T),
          countries = n_distinct(`Country/Region`)
        )
        return(data)
      }
      return(NULL)
    }
    
    key_figures <- reactive({
      data           <- sumData(input$timeSlider)
      data_yesterday <- sumData(input$timeSlider - 1)
      
      data_new <- list(
        new_confirmed = (data$confirmed - data_yesterday$confirmed) / data_yesterday$confirmed * 100,
        new_recovered = (data$recovered - data_yesterday$recovered) / data_yesterday$recovered * 100,
        new_deceased  = (data$death - data_yesterday$death) / data_yesterday$death * 100,
        new_countries = data$countries - data_yesterday$countries
      )
      
      keyFigures <- list(
        "confirmed" = HTML(paste(format(data$confirmed, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_confirmed))),
        "recovered" = HTML(paste(format(data$recovered, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_recovered))),
        "death"  = HTML(paste(format(data$death, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_deceased))),
        "countries" = HTML(paste(format(data$countries, big.mark = " "), "/ 195", sprintf("<h4>(%+d)</h4>", data_new$new_countries)))
      )
      return(keyFigures)
    })
  #---------------------------------------------
  # Top label
    output$valueBox_confirmed <- renderValueBox({
      valueBox(
        key_figures()$confirmed,
        subtitle = "Confirmed",
        icon     = icon("file-medical"),
        color    = "light-blue",
        width    = NULL
  
      )
    })
    
    
    output$valueBox_recovered <- renderValueBox({
      valueBox(
        key_figures()$recovered,
        subtitle = "Estimated Recoveries",
        icon     = icon("heart"),
        color    = "light-blue",
     
        
      )
    })
    
    output$valueBox_deceased <- renderValueBox({
      valueBox(
        key_figures()$death,
        subtitle = "Deceased",
        icon     = icon("heartbeat"),
        color    = "light-blue",
   
      )
    })
    
    output$valueBox_countries <- renderValueBox({
      valueBox(
        key_figures()$countries,
        subtitle = "Affected Countries",
        icon     = icon("flag"),
        color    = "light-blue",
     
      )
    })
    
    output$box_keyFigures <- renderUI(box(
      title = paste0("Date: (", strftime(input$timeSlider, format = "%d.%m.%Y"), ")"),
      fluidRow(
        column(
          valueBoxOutput("valueBox_confirmed", width = 3),
          valueBoxOutput("valueBox_recovered", width = 3),
          valueBoxOutput("valueBox_deceased", width = 3),
          valueBoxOutput("valueBox_countries", width = 3),
          width = 12,
          style = "margin-left: -20px"
        )
      )
    ))
    
    

   
    
#------------------------------------------------------------------------------    
    output$summaryTables <- renderUI({
      tabBox(
        tabPanel("Country/Region",
                 div(
                   dataTableOutput("summaryDT_country"),
                   style = "margin-top: -10px")
        ),
        tabPanel("Province/State",
                 div(
                   dataTableOutput("summaryDT_state"),
                   style = "margin-top: -10px"
                 )
        ),
        width = 12
      )
    })
    
    
    
    output$summaryDT_country <- renderDataTable(getSummaryDT(date_data(current_date), "Country/Region", selectable = TRUE))
    proxy_summaryDT_country  <- dataTableProxy("summaryDT_country")
    output$summaryDT_state   <- renderDataTable(getSummaryDT(date_data(current_date), "Province/State", selectable = TRUE))
    proxy_summaryDT_state    <- dataTableProxy("summaryDT_state")
    
    observeEvent(input$timeSlider, {
      data <- date_data(input$timeSlider)
      replaceData(proxy_summaryDT_country, summariseData(data, "Country/Region"), rownames = FALSE)
      replaceData(proxy_summaryDT_state, summariseData(data, "Province/State"), rownames = FALSE)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    observeEvent(input$summaryDT_country_row_last_clicked, {
      selectedRow     <- input$summaryDT_country_row_last_clicked
      selectedCountry <- summariseData(date_data(input$timeSlider), "Country/Region")[selectedRow, "Country/Region"]
      location        <- active_data %>%
        distinct(`Country/Region`, Lat, Long) %>%
        filter(`Country/Region` == selectedCountry) %>%
        summarise(
          Lat  = mean(Lat),
          Long = mean(Long)
        )
      leafletProxy("overview_map") %>%
        setView(lng = location$Long, lat = location$Lat, zoom = 4)
    })
    
    observeEvent(input$summaryDT_state_row_last_clicked, {
      selectedRow     <- input$summaryDT_state_row_last_clicked
      selectedCountry <- summariseData(date_data(input$timeSlider), "Province/State")[selectedRow, "Province/State"]
      location <- active_data %>%
        distinct(`Province/State`, Lat, Long) %>%
        filter(`Province/State` == selectedCountry) %>%
        summarise(
          Lat  = mean(Lat),
          Long = mean(Long)
        )
      leafletProxy("overview_map") %>%
        setView(lng = location$Long, lat = location$Lat, zoom = 4)
    })
    
    summariseData <- function(df, groupBy) {
      df %>%
        group_by(!!sym(groupBy)) %>%
        summarise(
          "Confirmed"            = sum(confirmed, na.rm = T),
          "Estimated Recoveries" = sum(recovered, na.rm = T),
          "Deceased"             = sum(death, na.rm = T),
          "Active"               = sum(active, na.rm = T)
        ) %>%
        as.data.frame()
    }
    
    getSummaryDT <- function(data, groupBy, selectable = FALSE) {
      datatable(
        na.omit(summariseData(data, groupBy)),
        rownames  = FALSE,
        options   = list(
          order          = list(1, "desc"),
          scrollX        = TRUE,
          scrollY        = "37vh",
          scrollCollapse = T,
          dom            = 'ft',
          paging         = FALSE
        ),
        selection = ifelse(selectable, "single", "none")
      )
    }
    
    
    
    
    
    
# Map part stop here  
#---------------------------------------------
#Download part start here
#---------------------------------------------
    datasetInput <- reactive({
      confirmed_us_df %>% 
        filter(region == "US") %>% 
        filter(date == as.Date("2020-05-31") | date == as.Date("2020-06-30")) %>% 
        group_by(sub_region) %>% 
        summarize(new_confirmed = max(confirmed) - min(confirmed)) %>%
        arrange(desc(new_confirmed)) %>% 
        filter(new_confirmed > 1000)
    }) 
    
    output$downloadData <- downloadHandler(
      filename = function() { 
        paste("US_States_in_Jun_2020", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(datasetInput(), file)
      })
    #pdf
    output$downloadDatapdf <- downloadHandler(
      filename = function() { 
        paste("US_States_in_Jun_2020", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        write.pdf(datasetInput(), file)
      })
 #----------------------------------Downloading however using datatable instead of manual download
    output$table_out  <- DT::renderDataTable(
      datatable(
        datasetInput(),
        rownames = TRUE,
        options = list(
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = FALSE,
          dom = 'Btlfipr',
          buttons = c('copy', 'csv', 'excel', 'pdf','print')
        ),
        class = "display", #if you want to modify via .css
        extensions = "Buttons"
      ))
    
#--------------------------------------------------------------Pictures
    data<- reactive({
      global_df0 %>% 
        group_by(region) %>% 
        summarize(death = max(death)) %>% 
        arrange(desc(death)) %>% 
        top_n(5) %>% 
        ggplot(aes(x = reorder(region, -death, sum), y = death)) + geom_col(fill = "#ffc04d") + xlab("countries") + scale_y_continuous(labels = comma_format(big.mark = ","))
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function() { paste(data(), '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = data(), device = "png")
      }
    )
  
#--------------------------------------------------------second graph
    
    data2<- reactive({
      confirmed_us_df %>% 
        mutate(month = month(date)) %>% 
        filter(region == "US" & sub_region == "Georgia") %>% 
        group_by(month) %>% 
        summarize(
          confirmed = max(confirmed)
        ) %>% 
        ggplot(aes(x = month, y = confirmed)) + geom_point(size = 2) + geom_line(size = 1) + scale_y_continuous(labels = comma_format(big.mark = ","))

    })

    output$downloadPlot2 <- downloadHandler(
      filename = function() { paste(data2(), '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = data2(), device = "png")
      }
    )
#------------------------------------------------------------------------------------interactive table
    tabledownload<-  reactive({
      global_df <- global_df()
      global_df %>%
        group_by(sub_region, region) %>%
        summarize(confirmed = max(confirmed),
                  recovered = max(recovered),
                  death = max(death)) %>% 
        arrange(region)
    })
    
    
    
    output$tabledownload  <- DT::renderDataTable(
      datatable(
        tabledownload(),
        rownames = TRUE,
        options = list(
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = FALSE,
          dom = 'Btlfipr',
          buttons = c('copy', 'csv', 'excel', 'pdf','print')
        ),
        class = "display", #if you want to modify via .css
        extensions = "Buttons"
      ))
    
#----------------------------------------------------------------------------------Interactive images at the bottom
    
    imagedownload<-  reactive({
      global_df <- global_df()
      global_df %>% 
        group_by(region) %>%
        summarize(confirmed = max(confirmed)) %>% 
        arrange(desc(confirmed)) %>% 
        top_n(5) %>% 
        ggplot(aes(x = reorder(region, -confirmed, sum), y = confirmed)) + geom_col(fill = "#ffc04d") + xlab("region") + scale_y_continuous(labels = comma_format(big.mark = ","))
    })
    
    output$imagedownloadinteractive <- downloadHandler(
      filename = function() { paste(imagedownload(), '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = imagedownload(), device = "png")
      }
    )
    
    

    
    
    
    
    
  #------------------------------------------------
 #Download logic part 2   
    datasetInput2 <- reactive({
      global_df0 %>% 
        filter(date == max(global_df0$date)) %>% 
        filter(confirmed > 1000000) %>% 
        group_by(region) %>% 
        summarize(recovery_rate = round(recovered/confirmed, 2)) %>% 
        arrange(desc(recovery_rate)) 
    }) 
    
    output$downloadData2 <- downloadHandler(
      filename = function() { 
        paste("Countries_highest_recovery_rate", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(datasetInput2(), file)
      })
    
    
    output$table_out2  <- DT::renderDataTable(
      datatable(
        datasetInput2(),
        rownames = TRUE,
        options = list(
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = FALSE,
          dom = 'Btlfipr',
          buttons = c('copy', 'csv', 'excel', 'pdf','print')
        ),
        class = "display", #if you want to modify via .css
        extensions = "Buttons"
      ))
  #-------------------------------------------------
    
    
    
    
    
    
    
 
}

# Run the application 
shinyApp(ui = ui, server = server)
