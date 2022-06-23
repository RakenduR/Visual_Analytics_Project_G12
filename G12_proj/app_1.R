library(shiny)
library(shinyWidgets)
library(readxl)
library(plotly)
library(tidyverse)
library(dplyr)
library(bslib)
library(lubridate)
library(zoo)
library(tsibble)
library(forecast)
library(tseries)
library(fpp3)
library(DT)
library(ggHoriPlot) 
library(ggthemes)
library(timetk)
library(tidyquant)

### DATA CLEANING
##Reynard's data
overall_forecast <- read_csv("data/overall forecast.csv")
overall_forecast$Time <- yearmonth(overall_forecast$Time)
overall_forecast <- as_tsibble(overall_forecast, index = Time)

division_forecast <- read_csv("data/division forecast.csv")
division_forecast$Time <- yearmonth(division_forecast$Time)
division_forecast <- as_tsibble(division_forecast, index = Time)
## CPI

cpi <- head(read_excel("data/cpijan22.xlsx",
                       sheet = 'T4',
                       skip = 5),
            146)

cpi1 <- cpi %>%
    select(-c(1:615)) %>%
    mutate_if(is.character,as.numeric) 

cpi2 <- cpi %>%
    select(c(1:3))

cpi3 <- cbind(cpi2, cpi1)

cpi_cln <- cpi3 %>%
    select(c(1:123)) %>%
    pivot_longer(cols = 4:123,
                 names_to = 'Time',
                 values_to = 'CPI',
                 values_drop_na = TRUE)

cpi_cln$Time <-  gsub(" ", "-", cpi_cln$Time) 
cpi_cln$Time <-  ym(cpi_cln$Time)

cpi_cln$Year <- as.numeric(format(cpi_cln$Time, "%Y"))
cpi_cln$Month <- as.numeric(format(cpi_cln$Time, "%m"))

div_cpi <- unique(na.omit(cpi_cln$Division))

cat_cpi <- unique(na.omit(cpi_cln$Category))

var_cpi <- unique(na.omit(cpi_cln$Variables))

year_cpi <- unique(as.numeric(format(as.Date(cpi_cln$Time),"%Y")))

## CPI Change
cpi_change <- head(read_excel("data/cpijan22.xlsx",
                              sheet = 'T8',
                              skip = 5),
                   146)

cpi_change1 <- cpi_change %>%
    select(-c(1:614)) %>%
    mutate_if(is.character,as.numeric) 

cpi_change2 <- cpi_change %>%
    select(c(1:3))

cpi_change3 <- cbind(cpi_change2, cpi_change1)

cpi_change_cln <- cpi_change3 %>%
    select(c(1:123)) %>%
    pivot_longer(cols = 4:123,
                 names_to = 'Time',
                 values_to = 'Change',
                 values_drop_na = TRUE)

cpi_change_cln$Time <-  gsub(" ", "-", cpi_change_cln$Time) 
cpi_change_cln$Time <-  ym(cpi_change_cln$Time)
cpi_change_cln$Time <- as.yearmon(cpi_change_cln$Time, "%m/%Y")

cpi_change_cln$Year <- as.numeric(format(cpi_change_cln$Time, "%Y"))
cpi_change_cln$Month <- as.numeric(format(cpi_change_cln$Time, "%m"))

## Average Retail Price 

arp <- head(read_excel("data/cpijan22.xlsx", 
                       sheet = 'T2', 
                       skip = 5), 
            69)

arp_cln <- arp %>%
    select(-c(3:50, 147)) %>%
    pivot_longer(cols = 3:98,
                 names_to = 'Time',
                 values_to = 'Price',
                 values_drop_na = TRUE)

arp_cln$Time <-  gsub(" ", "-", arp_cln$Time) 
arp_cln$Time <-  ym(arp_cln$Time)

arp_cln$Year <- as.integer(format(arp_cln$Time, "%Y"))
arp_cln$Month <- as.character(format(arp_cln$Time, "%b"))

cpi_change_cln$Variables <- factor(cpi_change_cln$Variables,
                                   levels = as.vector(unique(cpi_change_cln$Variables)))

cat_arp <- unique(na.omit(arp_cln$Category))

### Timeseries and Correlation analysis
df_cpi <- read_xlsx("data/CPI_T4.xlsx")

df_cpi$date2 <- df_cpi$date

df_cpi <- separate(df_cpi, date2, c("year", "month", "day"))

df_cpi <- select(df_cpi, - (`day`)) 

df_cpi$year = as.numeric(df_cpi$year)

# Setup for the plotly charts (# FALSE returns ggplots)
interactive <- FALSE


### USER INTERFACE

ui <- fluidPage(
    theme = bs_theme(bootswatch = "minty"),

# Top Navigation Bar
    
    navbarPage("Money No €nough",
               
               tabPanel("User Guide", 
                        icon = icon("map-marked-alt")),

# EXPLORE TAB               
               navbarMenu("Explore",
                          icon = icon("globe"),
                          
                        
# EXPLORE - Monthly CPI by Division  


                          tabPanel("Monthly CPI by Division",
                                   h4("Monthly Consumer Price Index in Singapore"),
                                   fluidRow(
                                     column(3,
                                            sliderInput("lineyear", 
                                                        "Select Time Range:",
                                                        min = min(cpi_cln$Year), 
                                                        max = max(cpi_cln$Year),
                                                        step = 1,
                                                        value = c(2018,2021),
                                                        sep = ""),
                                            selectInput(input = "linedivision", 
                                                        label = "Select Divisions:",
                                                        choices = c(div_cpi),
                                                        multiple = TRUE,
                                                        selected = "Food"),
                                            h6("Click to add division; Backspace to remove division.", 
                                               style = "font-size:10px;"),
                                            radioButtons("linedate", 
                                                         label = "Select X-axis Ticks Interval:",
                                                         choices = list("Yearly" = "1 year", 
                                                                        "Monthly" = "1 month"), 
                                                         selected = "1 year")
                                     ),
                                     column(9,
                                            textOutput("lineerror"),
                                            h5("CPI over Time (Base Year = 2019)"),
                                            plotlyOutput("lineplot"),
                                            h5("Comparison of CPI by Division"),
                                            plotlyOutput("box"))
                                   )),


# EXPLORE - Change in CPI Horizon Plot (id = horizon, year = horyear, division = hordiv)

                          tabPanel("Change in CPI",
                                   fluidRow(
                                       headerPanel(h4("Monthly Change in CPI of Goods in Singapore")),
                                       column(3,
                                              sliderInput("horyear", 
                                                          "Select Time Range:",
                                                          min = min(cpi_change_cln$Year), 
                                                          max = max(cpi_change_cln$Year),
                                                          step = 1,
                                                          value = c(2018,2021),
                                                          sep = ""),
                                              selectInput(input = "hordiv", 
                                                          label = "Select Division:",
                                                          choices = c(div_cpi),
                                                          multiple = FALSE),
                                              radioButtons("hordate", 
                                                           label = "Select X-axis Ticks Interval:",
                                                           choices = list("Yearly" = "1 year", 
                                                                          "Monthly" = "1 month"), 
                                                           selected = "1 year"),
                                              radioButtons("horfont", 
                                                           label = "Select Y-axis Font Size:",
                                                           choices = list("Small" = 5, 
                                                                          "Large" = 10), 
                                                           selected = 5)),
                                       column(9,
                                              h6("Please allow 10 seconds for the chart to run and show up for the first time",
                                                 style = "font-size:10px;"),
                                              plotOutput("horizon")
                                              ))),

# EXPLORE - Change in Retail Price (Starburst) (Cat = starcat)

                          tabPanel("Change in Retail Price",
                                   fluidRow(
                                       headerPanel(h4("Retail Price (SGD) of Items in Singapore")),
                                       column(3,
                                              selectInput(input = "starcat", 
                                                          label = "Select Category:",
                                                          choices = c(cat_arp),
                                                          multiple = FALSE),
                                              uiOutput("starvar")),
                                       column(5, 
                                              plotOutput("starburst"),
                                              textOutput('"starerror"')),
                                       column(4, tableOutput("startab"))
                                       ))),

# ANALYSE TAB
               
               navbarMenu("Analyse",
                          icon = icon("chart-bar"),
                          
# ANALYSE - Correlation Analysis (ID = correlation, year = corryear, division = corrdiv)

                          tabPanel("Correlation Analysis",
                                   h4("Correlation Analysis of CPI in Singapore by Division"),
                                   fluidRow(
                                       column(3,
                                              sliderInput(inputId = "corryear",
                                                          label = "Time period",
                                                          min = min(df_cpi$year),
                                                          max = max(df_cpi$year)-1,
                                                          timeFormat = "%Y",
                                                          step = 1,
                                                          value = c(2018,2021),
                                                          sep = ""),
                                              selectInput(inputId = "corrdivision", 
                                                          label = "Select division",
                                                          choices = unique(df_cpi$div),
                                                          selected = "All Items") ),
                                       column(4, plotOutput("corrSeries")),
                                       column(5, plotOutput("acfSeries"))),
                                   fluidRow(
                                     #column(3),
                                     column(9, offset = 3,
                                            h5("Facet of CPI by Divisions"),
                                            plotOutput("overall"))
                                            )
                                   ),

# ANALYSE - Seasonality Analysis (ID = seasonality, Division = seadiv)

                          tabPanel("Seasonality Analysis",
                                   h4("Seasonal Analysis of CPI in Singapore by Division"),
                                   fluidRow(
                                       column(3,
                                              sliderInput(inputId = "syear",
                                                          label = "Time period",
                                                          min = min(df_cpi$year),
                                                          max = max(df_cpi$year)-1,
                                                          timeFormat = "%Y",
                                                          step = 1,
                                                          value = c(2018,2021),
                                                          sep = ""),
                                              selectInput(inputId = "sdivision", 
                                                          label = "Select division",
                                                          choices = unique(df_cpi$div),
                                                          selected = "All Items")),
                                       column(4, plotOutput("SSeries")),
                                       column(5, plotOutput("STLSeries")))
                                   )),

# PREDICT TAB
               
               navbarMenu("Predict",
                          icon = icon("chart-line"),
                          
# PREDICT - Overall 
                          
                          tabPanel("CPI of Divisions",
                                   h4("Prediction of CPI in Singapore"),
                                   fluidRow(
                                       column(3,
                                              selectInput(input = "inddiv",
                                                          label = "Select Division:",
                                                          choices = c("All", "Food", "Clothing & Footwear","Housing & Utilities",
                                                                      "Household Durables & Services", "Health Care", "Transport",
                                                                      "Communication", "Recreation & Culture", "Education",
                                                                      "Miscellaneous Goods & Services")),
                                              numericInput("months", "Months to Forecast Ahead:", value = 12),
                                              selectInput(input = "model",
                                                          label = "Select Model",
                                                          choices = c("ARIMA","ETS", "TSLM", "AR")),
                                       h6("ARIMA: Autoregressive integrated moving average model", style = "font-size:10px;"),
                                       h6("ETS: Error,Trend,Seasonal model", style = "font-size:10px;"),
                                       h6("TSLM: Time Series Linear Model", style = "font-size:10px;"),
                                       h6("AR: Autoregressive time series model", style = "font-size:10px;")),

                                              
                                       column(9, 
                                              h5("Forecast of CPI by Division"),
                                              h6("Please allow 10 seconds for the predictive models to run and show up on the screen",
                                                 style = "font-size:10px;"),
                                              plotOutput("DivisionForecast"),
                                              dataTableOutput("selection"),
                                              h5("Diagnostics"),
                                              plotOutput("residuals")
                                              
                                       )))



)
    )
)


### SERVER

server <- function(input, output) {
    
    # Dynamic output for Average Retail Pricing Variable
    
    output$starvar <- renderUI({
        
        selectInput(inputId = "starvar", "Select Variable:", choices = var_arp() )
        
    })
    
    
    var_arp <- reactive({
      
        var_arp <- arp_cln %>%
            filter(Category == input$starcat) 
      
        unique(var_arp$Variables)
        
    })
    
    # EXPLORE - LINE CHART START #
    
    cpi_line <- reactive({
      
      validate(
        need(input$linedivision != "", "No division selected. Please select a division."))
      
      cpi_line <- cpi_cln %>%
        filter(Variables == input$linedivision)
      
      cpi_line
      
    })

    output$lineplot <- renderPlotly({
      
      lineplot <- 
        ggplot(cpi_line(),
               aes(x = Time, y = CPI, group = Variables, color = Variables,
                   text = paste('Time:', as.yearmon(Time, "%m/%Y"),
                                '<br>CPI: ', CPI,
                                '<br>Variable: ', Variables))) +
        geom_line()+
        theme_minimal()+
        scale_x_date(expand=c(0,0), 
                     date_breaks = input$linedate, 
                     date_labels = "%b '%Y",
                     limit=c(as.Date(ISOdate(min(input$lineyear), 1, 1)),
                             as.Date(ISOdate(max(input$lineyear), 12, 31))))+
        theme(axis.text.x = element_text(angle = 60),
              axis.title.x = element_blank())
      
      ggplotly(lineplot, tooltip = c("text"))
    })
    
    # EXPLORE - LINE CHART END
    
    
    # EXPLORE - BOX PLOT START
    
    cpi_box <- reactive({
      
      validate(
        need(input$linedivision != "", " "))
      
      cpi_box <- cpi_cln %>%
        filter(Variables == input$linedivision)
      
      cpi_box
      
    })
    
    output$box <- renderPlotly({
      
      boxplot <- 
        ggplot(cpi_box(),
               aes(x=Variables, y=CPI, color = Variables)) + 
        geom_boxplot() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60),
              axis.title.x = element_blank())
      
      ggplotly(boxplot)
      
    })
    
    # EXPLORE - BOX PLOT END
    
    # EXPLORE - HORIZON PLOT START
    
    output$horizon <- renderPlot({
        
        p <- cpi_change_cln %>%
            filter(Division == input$hordiv) %>%
            mutate(factor(Variables,
                          levels = as.vector(unique(Variables)))) %>%
            arrange(Variables) %>%
            ggplot() +
            geom_horizon(aes(as.Date(Time), Change),
                         origin = 0,
                         horizonscale = c(-3.0, -2.0, -0.3,  0,  0.3, 2.0, 3.0))+
            scale_fill_hcl(palette = 'RdBu') +
            facet_grid(Variables~.)+
            theme_few() +
            theme(
                panel.spacing.y=unit(0, "lines"),
                plot.title = element_text(size=15),
                plot.subtitle = element_text(size=12),
                strip.text.y = element_text(size = input$horfont, angle = 0, hjust = 0),
                axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.x = element_text(size = 10, angle = 90, hjust = 0, vjust = 0.5),
                axis.title.x = element_blank(),
                panel.border = element_blank(),
                legend.position="none") +
            scale_x_date(expand=c(0,0), 
                         date_breaks = input$hordate, 
                         date_labels = "%b '%Y",
                         limit=c(as.Date(ISOdate(min(input$horyear), 1, 1)),
                                 as.Date(ISOdate(max(input$horyear), 12, 31)))) +
            xlab('Date') +
            ggtitle(paste('Division:',input$hordiv), 
                    paste('Year:', min(input$horyear),'to', max(input$horyear)))
        
        p
    })
    
    # EXPLORE - HORIZON PLOT END #
    
    # EXPLORE - STARBURST CHART START #
    
    arp_star <- reactive({
      
      validate(
        need(input$starvar != "", "No variable selected. Please select a variable."))
      
      arp_star <- arp_cln %>%
        filter(Variables == input$starvar)
      
      arp_star
      
    })
    
    output$starburst <- renderPlot({
    
        r <- #arp_cln %>%
            #filter(Variables == input$starvar)  %>%
            ggplot(arp_star(),
                   aes(x = Year, y = 1, fill = Price, label = Month)) +
            geom_bar(stat = "identity") +
            xlim(2011, 2024) +
            coord_polar(theta = "y" ) +
            scale_fill_gradient(low = "lightcyan", high = "midnightblue") +
            geom_text(aes(x = Year + 2,
                          label = ifelse(Year == 2021, Month, ""), 
                          size = 0.5),
                      position = position_stack(vjust = 0.5)) +
            ylab("") + 
            xlab("") +
            labs(caption = "Each concentric circle represents a year, beginning from year 2014 
for the innermost ring, to year 2021 for the outermost ring.") +
            theme(plot.title = element_text(size=15),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  strip.background = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  legend.position="none",
                  plot.caption.position = "plot",
                  plot.caption = element_text(hjust = 0.5)
                  ) +
            ggtitle(paste('Item:',input$starvar))
            
        r
    })
    
    # EXPLORE - STARBURST CHART END#
    
    # EXPLORE - STARBURST TABLE START#
    
    arp_tab <- reactive({
      
      validate(
        need(input$starvar != "", " "))
      
      arp_tab <- arp_cln %>%
        filter(Variables == input$starvar)
      
      arp_tab
      
    })
    
    output$startab <- renderTable({ 
        
        table <- arp_tab() %>%
            select(Year, Price) %>%
            group_by(Year) %>%
            summarize(Min = min(Price, na.rm = TRUE),
                      Mean = mean(Price, na.rm = TRUE),
                      Median = median(Price, na.rm = TRUE),
                      Max = max(Price, na.rm = TRUE))
                      
        table
        })
    
    # STARBURST TABLE END #
    
    ## ANALYSE - Time series ##
    
    output$tSeries <- renderPlot({
      cpi_div <- df_cpi %>%
        filter(div == input$division) %>% 
        filter(year >= input$year1[1] & year <= input$year1[2]) %>%
        plot_time_series(date, cpi,
                         .facet_ncol = 4, .facet_scales = "free",
                         .interactive = interactive)
      
      cpi_div
      
      
    })
    
    ##ANALYSE - Correlation ##
    output$corrSeries <- renderPlot({
      cpi_div <- df_cpi %>%
        filter(div == input$corrdivision) %>% 
        filter(year >= input$corryear[1] & year <= input$corryear[2]) %>%
        plot_time_series(date, cpi,
                         .facet_ncol = 4, .facet_scales = "free",
                         .interactive = interactive)
      
      cpi_div
      
    })
    
    output$acfSeries <- renderPlot({
      cpi_acf <- df_cpi %>%
        filter(div == input$corrdivision) %>%
        filter(year >= input$corryear[1] & year <= input$corryear[2]) %>%
        plot_acf_diagnostics(
          date, cpi,               # ACF & PACF
          .lags = "3 months",          # 1 quarter lags
          .interactive = interactive)
      
      cpi_acf
    })
    
    output$overall <- renderPlot({
      cpi_div <- df_cpi %>%
        group_by(div) %>%
        plot_time_series(date, cpi, 
                         .facet_ncol = 4, .facet_scales = "free",
                         .interactive = interactive)
      
      cpi_div
      
      
    })
    
    output$SSeries <- renderPlot({
      cpi_season <- df_cpi %>%
        filter(div == input$sdivision) %>% 
        filter(year >= input$syear[1] & year <= input$syear[2]) %>%
        plot_seasonal_diagnostics(
          date, 
          cpi, 
          .interactive = interactive)
      
      cpi_season
      
      
    })
    
    
    #trend decomposition
    output$STLSeries <- renderPlot({
      cpi_stl <- df_cpi %>%
        filter(div == input$sdivision) %>% 
        filter(year >= input$syear[1] & year <= input$syear[2]) %>%
        plot_stl_diagnostics(
          date, cpi,
          .frequency = "auto", .trend = "auto",
          .feature_set = c("observed", "season", "trend", "remainder"),
          .interactive = interactive)
      
      cpi_stl
      
    })
    

    
    ### PREDICTIVE 
    
    Food <- division_forecast$Food
    ClothingFootwear <- division_forecast$`Clothing & Footwear`
    HousingUtilities <- division_forecast$`Housing & Utilities`
    HouseholdServices <- division_forecast$`Household Durables & Services`    
    Healthcare <- division_forecast$`Health Care`
    Transport <- division_forecast$Transport    
    Communication <- division_forecast$Communication   
    RecreationCulture <- division_forecast$`Recreation & Culture`   
    Education <- division_forecast$Education   
    Miscellaneous <- division_forecast$`Miscellaneous Goods & Services`
    Overall <- overall_forecast$CPI
    
    division <- reactive({
      if(input$inddiv == "Food"){
        overall_forecast$CPI <- Food
      }
      
      if(input$inddiv == "Clothing & Footwear"){
        overall_forecast$CPI <- ClothingFootwear
      }
      
      if(input$inddiv == "Housing & Utilities"){
        overall_forecast$CPI <- HousingUtilities
      }
      
      if(input$inddiv == "Household Durables & Services"){
        overall_forecast$CPI <- HouseholdServices
      }
      
      if(input$inddiv == "Health Care"){
        overall_forecast$CPI <- Healthcare
      }
      
      if(input$inddiv == "Transport"){
        overall_forecast$CPI <- Transport
      }
      
      if(input$inddiv == "Communication"){
        overall_forecast$CPI <- Communication
      }
      
      if(input$inddiv == "Recreation & Culture"){
        overall_forecast$CPI <- RecreationCulture
      }
      
      if(input$inddiv == "Education"){
        overall_forecast$CPI <- Education
      }
      
      if(input$inddiv == "Miscellaneous Goods & Services"){
        overall_forecast$CPI <- Miscellaneous
      }
      
      if(input$inddiv == "All"){
        overall_forecast$CPI <- overall_forecast$CPI
      }
      
      return(overall_forecast)
      
    })
    
    
    
    output$DivisionForecast <- renderPlot({
      overall_forecast <- division()
      
      if (input$model == "ARIMA"){
        
        overall_forecast %>%
          model(ARIMA(CPI)) %>%
          forecast(h = paste0(input$months," months")) %>%
          autoplot(overall_forecast) +
          labs(title = paste0(input$months, "-month forecasts for ", input$inddiv, " items using the ", input$model, " model"),
               y="Consumer Price Index",
               x="Date") +
          theme_light()
      }
      
      else if (input$model == "ETS"){
        overall_forecast %>%
          model(ETS(CPI)) %>%
          forecast(h = paste0(input$months," months")) %>%
          autoplot(overall_forecast) +
          labs(title = paste0(input$months, "-month forecasts for ", input$inddiv, " items using the ", input$model, " model"),
               y="Consumer Price Index",
               x="Date") +
          theme_light()
        
        
      }
      
      else if (input$model == "TSLM"){
        overall_forecast %>%
          model(TSLM(CPI ~ trend())) %>%
          forecast(h = paste0(input$months," months")) %>%
          autoplot(overall_forecast) +
          labs(title = paste0(input$months, "-month forecasts for ", input$inddiv, " items using the ", input$model, " model"),
               y="Consumer Price Index",
               x="Date") +
          theme_light()
        
        
      }
      
      else if (input$model == "AR"){
        overall_forecast %>%
          model(AR(CPI)) %>%
          forecast(h = paste0(input$months," months")) %>%
          autoplot(overall_forecast) +
          labs(title = paste0(input$months, "-month forecasts for ", input$inddiv, " items using the ", input$model, " model"),
               y="Consumer Price Index",
               x="Date") +
          theme_light()
        
        
      }
    })
    
    
    
    
    output$selection <- DT::renderDataTable({
      overall_forecast <- division()
      
      if (input$model == "ARIMA"){
        
        fit <- overall_forecast %>%
          filter(Time < yearmonth("2021 Nov")) %>%
          model(ARIMA(CPI)
          ) %>%
          forecast(h= "12 months")
        
        selection <- fit %>% accuracy(overall_forecast)
        
        selection
      }
      
      else if (input$model == "ETS"){
        
        fit <- overall_forecast %>%
          filter(Time < yearmonth("2021 Nov")) %>%
          model(ETS(CPI)
          ) %>%
          forecast(h= "12 months")
        
        selection <- fit %>% accuracy(overall_forecast)
        
        selection
      }
      
      else if (input$model == "TSLM"){
        
        fit <- overall_forecast %>%
          filter(Time < yearmonth("2021 Nov")) %>%
          model(TSLM(CPI ~ trend())
          ) %>%
          forecast(h= "12 months")
        
        selection <- fit %>% accuracy(overall_forecast)
        
        selection
      }
      
      else if (input$model == "AR"){
        
        fit <- overall_forecast %>%
          filter(Time < yearmonth("2021 Nov")) %>%
          model(AR(CPI)
          ) %>%
          forecast(h= "12 months")
        
        selection <- fit %>% accuracy(overall_forecast)
        
        selection
      }
    })
    
    output$residuals <- renderPlot({
      overall_forecast <- division()
      
      if (input$model == "ARIMA"){
        
        overall_forecast %>%
          model(ARIMA(CPI)) %>%
          gg_tsresiduals() +
          labs(title = "Diagnostics")
      }
      
      else if (input$model == "ETS"){
        overall_forecast %>%
          model(ETS(CPI)) %>%
          gg_tsresiduals() +
          labs(title = "Diagnostics")
      }
      
      else if (input$model == "TSLM"){
        overall_forecast %>%
          model(TSLM(CPI ~trend())) %>%
          gg_tsresiduals() +
          labs(title = "Diagnostics")
      }
      
      else if (input$model == "AR"){
        overall_forecast %>%
          model(AR(CPI)) %>%
          gg_tsresiduals() +
          labs(title = "Diagnostics")
      }
    })
}
 
shinyApp(ui = ui, server = server)