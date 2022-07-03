##############################
##### Load Packages      #####
##############################


pacman::p_load(shiny,tidyverse,lubridate,zoo,ggthemes,hrbrthemes,ggdist,gghalves,
              ggridges,patchwork,zoo, ggrepel,ggiraph,gganimate,scales,shiny, shinydashboard, shinythemes,
              tsibble,tseries,plotly,ggstatsplot,forecast,tools,shinyWidgets,readxl,bslib,patchwork,tmap, sf, leaflet,
              rstantools)



##############################
##### Read & Clean Data  #####
##############################

##### Q1 #####

travel_filt <- read_rds('data/rds/travel_filt.rds')
restaurants <- read_csv("data/Restaurants.csv")
pubs <- read_csv("data/Pubs.csv")

data_travel= travel_filt %>%
  mutate(weekday = weekdays(checkInTime),
         day = day(checkInTime),
         month=as.character(checkInTime,"%b %y"),
         year = year(checkInTime),
         monthYear = as.yearmon(checkInTime),
         travelEndLocationId=as.character(travelEndLocationId),
         timeSpent = checkOutTime - checkInTime,
         participantId=as.character(participantId),
         purpose=as.character(purpose))
data_travel$timeSpent <- as.numeric(as.character(data_travel$timeSpent))

data_travel <- data_travel[,c("participantId","travelStartLocationId", "travelEndLocationId", "purpose",  "amountSpent","timeSpent","weekday","day","month","year","monthYear")]

group_pub <-merge(x=data_travel, y=pubs, by.x = "travelEndLocationId", by.y = "pubId")
group_pub$venue <- "Pub"
group_pub$foodCost <- 0.00

group_restaurant<-merge(x=data_travel, y=restaurants, by.x = 'travelEndLocationId', by.y =  'restaurantId')
group_restaurant$venue <- "Restaurant"
group_restaurant$hourlyCost <- 0.00

group <-rbind(group_pub, group_restaurant)

period <- unique(na.omit(group$monthYear))


##### Q2 #####
dataset_1 <- read_rds("data/rds/dataset_1.rds")
dataset_2 <- read_rds("data/rds/dataset_2.rds")
dataset_3 <- read_rds("data/rds/dataset_3.rds")
dataset_4 <- read_rds("data/rds/dataset_4.rds")

edutitle <- c("All",as.character(unique(dataset_1$educationLevel)))
educode <- c("All",as.character(unique(dataset_1$educationLevel)))
names(educode) <- edutitle

agetitle <- c("All",as.character(unique(dataset_1$agegroup)))
agecode <- c("All",as.character(unique(dataset_1$agegroup)))
names(agecode) <- agetitle


##### Q3 #####

jobs <- read_rds("data/rds/jobs.rds")
jbuildings <- read_sf("data/jBuildings.csv",
                      options = "GEOM_POSSIBLE_NAMES=location")

jjobs <- read_sf("data/jjobs.csv",
                 options = "GEOM_POSSIBLE_NAMES=location")

##############################
##### UI                 #####
##############################




ui <- navbarPage(
  title = "Vastly Challenging Economics",
  fluid = TRUE,
  collapsible = TRUE,
  windowTitle = "VAST Challenge 3 - Economic",
  theme=shinytheme("united"),
  id = "navbarID",
  tabPanel(title = "Overview",icon = icon("globe"),
           mainPanel(
             ##### Introduction #####
           )
  ),
  ##### Q1 UI #####
navbarMenu("Business",icon = icon("dollar-sign",lib = "font-awesome"),
           tabPanel("Revenue",
                    sidebarPanel(
                      width = 3,
                      radioButtons("venue", 
                                   label = "Select the Venue:",
                                   choices = list("Pub" = "Pub", 
                                                  "Restaurant" = "Restaurant"), 
                                   selected = "Pub"
                      ),
                      
                      selectInput(input = "period", 
                                  label = "Choose Period:",
                                  choices = c(period),
                                  multiple = TRUE,
                                  selected = period[1]),
                      
                      radioButtons("group", 
                                   label = "Select X-axis Interval:",
                                   choices = list("daily" = "day", 
                                                  "weekly" = "weekday",
                                                  "monthly" = "month"), 
                                   selected = "day")
                    ),
                    
                    mainPanel(
                      width = 9,
                      fluidRow(
                        column(width = 12,
                               
                               plotOutput("revenue",
                                          height = "500px")
                               
                        )
                      )
                    )
           ),
           tabPanel("Q1A",
             
           ),
           tabPanel("Q1B",
                    )
  
),
  
  ##### End of Q1 UI #####
  
  ##### Q2 UI #####
navbarMenu("Finance",icon = icon("search-dollar",lib = "font-awesome"),
           tabPanel("Income and Expense",
                    sidebarPanel(width =3,
                                 selectInput("eduselect",
                                             "Select Education Level",
                                             unique(dataset_1$educationLevel)
                                 ),
                                 selectInput("ageselect",
                                             "Select Age Group",
                                             unique(dataset_1$agegroup)
                                 )
                    ),
                    mainPanel(width = 9,
                              plotOutput("plot1",
                                         height = "500px")
                              
                              
                    )
           ),
           tabPanel("Finance Variation ",
                    sidebarPanel(width =3
                    ),
                    mainPanel(width = 9,
                              plotOutput("plot2",
                                         height = "500px")
                    )
                    
           ),
           tabPanel("Expense Calendar",
                    h4("Variations in Expenditure of various groups"),
                    sidebarPanel(width =3,
                                 radioButtons("edulevel", 
                                              label = "Select Education Level:",
                                              choices = educode, 
                                              selected = "All"
                                 ),
                                 radioButtons("agelevel", 
                                              label = "Select Age Group:",
                                              choices = agecode, 
                                              selected = "All"
                                 )
                    ),
                    mainPanel(width = 9,
                              plotOutput("plot3",
                                         height = "500px")
                    )
                    
           ),
           tabPanel("Prediction",
                    h4("Prediction of Income of Residents of Engagement, Ohio"),
                    sidebarPanel(width =3,
                                 radioButtons("prededulevel", 
                                              label = "Select Education Level:",
                                              choices = educode, 
                                              selected = "All"
                                 ),
                                 radioButtons("predagelevel", 
                                              label = "Select Age Group:",
                                              choices = agecode, 
                                              selected = "All"
                                 ),                      
                                 radioButtons("predperiod",
                                              label = "Select Forecast Period:",
                                              choices = list("6 months" = "6 months",
                                                             "12 months" = "12 months",
                                                             "18 months" = "18 months"),
                                              selected = "6 months"
                                 ),
                                 radioButtons("predmodel", 
                                              label = "Select Model:",
                                              choices = c("ARIMA","ETS", "TSLM")
                                 )
                    ),
                    mainPanel(width = 9,
                              h4("Prediction of Income by Education Level and Age group"),
                              plotOutput("plot4"),
                              dataTableOutput("selection"),
                              h5("Diagnostics"),
                              plotOutput("residuals")
                    )
                    
           )
           ),


##### End of Q2 UI #####


##### Q3 UI #####
navbarMenu("Employer",icon = icon("bar-chart-o"),
           tabPanel("Employer Health",
                    sidebarPanel(width = 3,
                                 selectInput(inputId = "educationrequired",
                                             label = "Select Education Level",
                                             choices = c("Low" = "Low",
                                                         "High School or College" = "HighSchoolOrCollege",
                                                         "Bachelors" = "Bachelors",
                                                         "Graduate" = "Graduate",
                                                         "All" = "All"),
                                             selected = "All"),
                                 sliderInput(inputId = "bins",
                                             label = "Number of Bins",
                                             min = 4,
                                             max = 20,
                                             value = 12),
                                 sliderInput(inputId = "jobshourlyrate",
                                             label = "Select Hourly Rate for map plot",
                                             value = c(10,100),
                                             min = 10,
                                             max = 100)
                    ),
                    mainPanel(width = 9, fluidRow(column(width = 6,plotOutput("Wagespread", height = "850px")), column(width = 6, tmapOutput("jmap", height = "850px"))))),
           tabPanel("Employment Patterns",
                    sidebarPanel(width = 3,
                                 selectInput(inputId = "categoryj",
                                             label = "Select Categorical X-Variable for Comparison against Hourly Wage Rate",
                                             choices = c("Age group for average age hired" = "agegroupj",
                                                         "Education Level" = "educationRequirement"),
                                             selected = "educationRequirement"),
                                 selectInput(inputId = "testj",
                                             label = "Type of statistical test:",
                                             choices = c("parametric" = "p",
                                                         "nonparametric" = "np",
                                                         "robust" = "r",
                                                         "Bayes Factor" = "bf"),
                                             selected = "p"),
                                 selectInput(inputId = "plotTypej",
                                             label = "Type of plot:",
                                             choices = c("boxviolin" = "boxviolin",
                                                         "box" = "box",
                                                         "violin" = "violin"),
                                             selected = "boxviolin"),
                                 selectInput(inputId = "yvariablej",
                                             label = "Select Y-Variable for correlation plot against average age hired:",
                                             choices = c("Hourly Wage Rate" = "hourlyRate",
                                                         "No. of jobs hired for" = "jobshired"),
                                             selected = "jobshired"),
                                 selectInput(inputId = "testj2",
                                             label = "Type of statistical test:",
                                             choices = c("parametric" = "p",
                                                         "nonparametric" = "np",
                                                         "robust" = "r",
                                                         "Bayes Factor" = "bf"),
                                             selected = "p"),
                                 checkboxInput(inputId = "marginalj", 
                                               label = "Display marginal graphs", 
                                               value = TRUE)
                    ),
                    mainPanel(width = 9, plotOutput("employmentpattern", height = "600px"))
           ),
           tabPanel("Turnover",
                    sidebarPanel(width = 3,
                                 selectInput(inputId = "educationrequired2",
                                             label = "Select Education Level for turnover map plot",
                                             choices = c("Low" = "Low",
                                                         "High School or College" = "HighSchoolOrCollege",
                                                         "All" = "All"),
                                             selected = "All"),
                                 sliderInput(inputId = "jobshourlyrate2",
                                             label = "Select Hourly Rate for turnover map plot",
                                             value = c(10,22),
                                             min = 10,
                                             max = 22),
                                 selectInput(inputId = "educationrequired3",
                                             label = "Select Education Level for jobs with 0 applicants map plot",
                                             choices = c("Low" = "Low",
                                                         "High School or College" = "HighSchoolOrCollege",
                                                         "Bachelors" = "Bachelors",
                                                         "Graduate" = "Graduate",
                                                         "All" = "All"),
                                             selected = "All"),
                                 sliderInput(inputId = "jobshourlyrate3",
                                             label = "Select Hourly Rate for jobs with 0 applicants map plot",
                                             value = c(10,20),
                                             min = 10,
                                             max = 20)
                    ),
                    mainPanel(width = 9, plotOutput("turnover", height = "180px"), fluidRow(column(width = 6, tmapOutput("turnovermap", height = "700px")), column(width = 6, tmapOutput("turnovermap2", height = "700px"))))
           )
           )




##### End of Q3 UI #####
)
##############################
##### Server Side        #####
##############################
server <- function(input, output) {


  ##### Q1 Server #####
  
  
  output$revenue <- renderPlot({
    print(input$venue)
    print(input$period)
    print(input$group)
    p <- group %>%
      filter(monthYear == input$period, venue == input$venue ) %>%
      group_by(!!!rlang::syms(input$group), travelEndLocationId) %>%
      summarise(amountSpent = (sum(amountSpent))) %>%
      ggplot(aes_string(x=input$group, y="amountSpent", group="travelEndLocationId")) +
      geom_line(aes(color=travelEndLocationId),show.legend = TRUE)+
      labs(
        y= 'Revenue (Thousands$)',
        title = "Revenue Pubs - 2022",
        caption = "Ohio USA"
      ) 
    p
  })
  
  ##### End of Q1 Server #####
  
  
  
  
  
  ##### Q2 Server #####
  
  
  plot1_data <- reactive({
    # filter data by selected options
    req(input$eduselect)
    req(input$ageselect)
    dataset_1%>%
      filter(educationLevel == input$eduselect)%>%
      filter(agegroup ==input$ageselect) %>%
      group_by(YearMon, category) %>%
      summarise(amount = sum(amount)) %>%
      mutate( amount = ifelse(amount < 0,-amount,amount))%>%
      mutate( percentage = amount/sum(amount))
    
  })
  
  
  output$plot1 <- renderPlot({
    
    data_for_plot1 <- plot1_data()
    
    
    data_for_plot1%>%
      ggplot(aes(x=YearMon, 
                 y = percentage, 
                 fill =factor(category,levels=c("Wage","Shelter","Recreation","Food","Education","RentAdjustment")))) +
      geom_area(alpha=0.6, size=1,colour="white") +
      xlab("Month and Year")+
      ylab("Percentage") +
      ggtitle("Income and Expense") +
      theme(axis.title.y=element_text(angle =0),
            axis.title.x=element_text(margin = margin(t=-10)),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_line(colour = "grey90"),
            panel.grid.major.x = element_line(colour = "grey90"),
            panel.grid.major.y = element_line(colour = "grey90"),
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(size =16, angle = 45, margin = margin(t = 30)),
            axis.text.y = element_text(size =16),
            axis.line = element_line(color="grey25", size = 0.02),
            axis.title = element_text(size=16),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            plot.title = element_text(size =20,hjust = 0.5)) +
      scale_fill_discrete(name = "Category")
    
  })
  
  
  plot2_data <- reactive({
    
    
    
  })
  
  
  output$plot2 <- renderPlot({
    
    dataset_3 %>%
      ggplot(aes(x=YearMon, y = amount, group = revenue))+
      geom_line(aes(color = revenue), size = 1)+
      geom_text(aes(label = Label),nudge_x = 0.15, size = 4) +
      facet_grid(educationLevel ~ agegroup)+
      ylab("Income")+
      xlab("Month, Year")+
      theme(axis.title.y=element_text(angle =0),
            axis.title.x=element_text(margin = margin(t=-10)),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_line(colour = "grey90"),
            panel.grid.major.x = element_line(colour = "grey90"),
            panel.grid.major.y = element_line(colour = "grey90"),
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(size =16, angle = 45, margin = margin(t = 30)),
            axis.text.y = element_text(size =16),
            axis.line = element_line(color="grey25", size = 0.02),
            axis.title = element_text(size=16),
            legend.position = "none",
            plot.title = element_text(size =20,hjust = 0.5))+
      ggtitle("Average Income by Education Level")
  })
  
  plot3_data <- reactive({
    
    req(input$edulevel)
    req(input$agelevel)
    print(input$edulevel)
    print(input$agelevel)
    
    if(input$edulevel == 'All' && input$agelevel == 'All')
    {
      dataset_4
    }
    else if(input$edulevel == 'All' && input$agelevel != 'All'){
      dataset_4%>%
        filter(agegroup == input$agelevel) 
    }
    else if(input$edulevel != 'All' && input$agelevel == 'All'){
      dataset_4%>%
        filter(educationLevel == input$edulevel)
    }
    else{
      dataset_4%>%
        filter(educationLevel == input$edulevel) %>%
        filter(agegroup == input$agelevel) 
    }
  })
  
  output$plot3 <- renderPlot({
    
    data_for_plot3 <- plot3_data()
    
    data_for_plot3%>%
      group_by(date,wkday,month,wkofmonth,day)%>%
      summarise(Avgexpense = mean(Avgexpense)) %>%
      ggplot(aes(wkday,wkofmonth,fill=Avgexpense))+
      geom_tile(color = "white", size = 0.1)+
      geom_text(aes(label = day), color = "white") +
      facet_wrap(~month,nrow = 3) +
      theme_tufte(base_family = "Helvetica")+
      coord_equal() +
      scale_fill_gradient(name = "Expense",
                          low = "sky blue", 
                          high = "dark blue",
                          labels = comma)+
      labs(x = NULL, 
           y = NULL) +
      theme(axis.text = element_text(size = 10,margin = margin(r = -60)),
            axis.ticks= element_blank(),
            axis.text.y = element_blank(),
            legend.title = element_text(size =16),
            legend.text = element_text(size = 16),
            legend.box.margin = margin(20,20,20,20),
            legend.margin = margin(20,20,20,20),
            strip.text.x = element_text(size = 14),
            plot.title = element_text(size = 20, hjust = 0.5))+
      ggtitle("Average Daily Expense of Residents")
  })
  
  plot4_data <- reactive({
    
    req(input$prededulevel)
    req(input$predagelevel)
    req(input$predmodel)
    req(input$predperiod)
    
    if(input$prededulevel == 'All' && input$predagelevel == 'All')
    {
      dataset_5 <- dataset_1
    }
    else if(input$prededulevel == 'All' && input$predagelevel != 'All'){
      dataset_5 <- dataset_1%>%
        filter(agegroup == input$predagelevel) 
    }
    else if(input$prededulevel != 'All' && input$predagelevel == 'All'){
      dataset_5 <- dataset_1%>%
        filter(educationLevel == input$prededulevel)
    }
    else{
      dataset_5 <- dataset_1%>%
        filter(educationLevel == input$prededulevel) %>%
        filter(agegroup == input$predagelevel) 
    }
    
    forecast_data <- dataset_5 %>%
      mutate(Time = yearmonth(timestamp))%>%
      filter(Time < yearmonth("2023 May")) %>%
      group_by(Time,participantId) %>%
      summarise(income = sum(ifelse(amount > 0,amount,0))) %>%
      ungroup() %>%
      group_by(Time) %>%
      summarise(income = mean(income))%>%
      ungroup()
    
    forecast_data_1 <- as_tsibble(forecast_data, index = Time)
    View(forecast_data_1)
    return(forecast_data_1)
    
  })
  
  output$plot4 <- renderPlot({
    
    data_for_plot4 <- plot4_data()
    print(input$predperiod)
    
    predictionModel <- req(input$predmodel)
    
    if(predictionModel == "ARIMA")
    {
      data_for_plot4%>%
        model(ARIMA(income)) %>%
        forecast(h = input$predperiod) %>%
        autoplot(data_for_plot4) +
        theme_light()
    }
    else if(predictionModel == "ETS")
    {
      data_for_plot4%>%
        model(ETS(income)) %>%
        forecast(h = input$predperiod) %>%
        autoplot(data_for_plot4) +
        theme_light()
    }
    else if(predictionModel =="TSLM")
    {
      data_for_plot4%>%
        model(TSLM(income~trend())) %>%
        forecast(h = input$predperiod) %>%
        autoplot(data_for_plot4) +
        theme_light()
    }
    else if(predictionModel =="AR")
    {
      data_for_plot4%>%
        model(AR(income)) %>%
        forecast(h = input$predperiod) %>%
        autoplot(data_for_plot4) +
        theme_light()
    }
  })
  
  output$selection <- DT::renderDataTable({
    
    data_for_plot4 <- plot4_data()
    print(input$predmodel)
    
    
    if (input$predmodel == "ARIMA"){
      
      fit1 <- data_for_plot4 %>%
        model(ARIMA(income))
      
      accuracy(fit1)
      
    }
    else if (input$predmodel == "ETS"){
      
      fit1 <- data_for_plot4 %>%
        model(ETS(income))
      
      accuracy(fit1)
      
      
    }
    else if (input$predmodel == "TSLM"){
      
      fit1 <- data_for_plot4 %>%
        model(TSLM(income~trend()))
      
      accuracy(fit1)
      
    }
    else if (input$predmodel == "AR"){
      
      fit1 <- data_for_plot4 %>%
        model(AR(income))
      
      accuracy(fit1)
      
    }
  })
  
  output$residuals <- renderPlot({
    data_for_plot4 <- plot4_data()
    print(input$predmodel)
    if (input$predmodel == "ARIMA"){
      
      data_for_plot4 %>%
        model(ARIMA(income)) %>%
        gg_tsresiduals() +
        labs(title = "Diagnostics")
    }
    
    else if (input$predmodel == "ETS"){
      data_for_plot4 %>%
        model(ETS(income)) %>%
        gg_tsresiduals() +
        labs(title = "Diagnostics")
    }
    
    else if (input$predmodel == "TSLM"){
      data_for_plot4 %>%
        model(TSLM(income ~trend())) %>%
        gg_tsresiduals() +
        labs(title = "Diagnostics")
    }
    
    else if (input$predmodel == "AR"){
      data_for_plot4 %>%
        model(AR(income)) %>%
        gg_tsresiduals() +
        labs(title = "Diagnostics")
    }
  })
  
  ##### End of Q2 Server #####
  
  ##### Q3 Server #####
  ##### Shiny Server: Employer: Employer Health ##### 
  
  
  ##### Education Filter#####
  jobs2 <- reactive({
    if(input$educationrequired == "All") {
      jobs2 <- jobs
    } else if(input$educationrequired =="Low") {
      jobs2 <- jobs %>%
        filter(educationRequirement == "Low")
    } else if(input$educationrequired =="HighSchoolOrCollege"){
      jobs2 <- jobs %>%
        filter(educationRequirement =="HighSchoolOrCollege")
    } else if(input$educationrequired =="Bachelors"){
      jobs2 <- jobs %>%
        filter(educationRequirement =="Bachelors")
    } else {
      jobs2 <- jobs %>%
        filter(educationRequirement =="Graduate")
    }
  })
  jobs3 <- jobs[,-7]
  
  jjobs1 <- jjobs%>%
    mutate(hourlyRate = as.numeric(hourlyRate)) %>%
    mutate(Turnover = as.numeric(Turnover)) %>%
    rename('AverageAgeHired'='mean.age.') %>%
    mutate(AverageAgeHired = as.numeric(AverageAgeHired))
  
  jjobs2 <- reactive({
    if(input$educationrequired == "All") {
      jjobs2 <- jjobs1 %>%
        filter(hourlyRate >= min(input$jobshourlyrate) & hourlyRate <= max(input$jobshourlyrate))
    } else if(input$educationrequired == "Low") {
      jjobs2 <- jjobs1 %>%
        filter(educationRequirement == "Low") %>%
        filter(hourlyRate >= min(input$jobshourlyrate) & hourlyRate <= max(input$jobshourlyrate))
    } else if (input$educationrequired == "HighSchoolOrCollege") {
      jjobs2 <- jjobs1 %>%
        filter(educationRequirement == "HighSchoolOrCollege") %>%
        filter(hourlyRate >= min(input$jobshourlyrate) & hourlyRate <= max(input$jobshourlyrate))
    } else if (input$educationrequired == "Bachelors") {
      jjobs2 <- jjobs1 %>%
        filter(educationRequirement == "Bachelors") %>%
        filter(hourlyRate >= min(input$jobshourlyrate) & hourlyRate <= max(input$jobshourlyrate))
    } else {
      jjobs2 <- jjobs1 %>%
        filter(educationRequirement == "Graduate") %>%
        filter(hourlyRate >= min(input$jobshourlyrate) & hourlyRate <= max(input$jobshourlyrate))
    }
  })
  
  output$Wagespread <- renderPlot({
    j <- ggplot(jobs, aes(x = hourlyRate,
                          fill = educationRequirement)) +
      geom_histogram(data = jobs3,
                     fill = "dark grey",
                     alpha = .5,
                     bins = input$bins) +
      geom_histogram(colour = "black", 
                     bins = input$bins) +
      facet_wrap(~ educationRequirement) + 
      xlab("Hourly Rate") +
      ylab("No. of Job Openings") +
      labs(title = "Spread of Hourly Wages by Education Level\n vs. Overall Job Market") +
      guides(fill = FALSE, scale = "none") +
      theme_bw()
    
    j2 <- ggplot(jobs2(), aes(x = hourlyRate,
                              fill = "dark blue")) +
      geom_histogram(colour = "black", 
                     bins = input$bins) +
      xlab("Hourly Rate") +
      ylab("No. of Job Openings") +
      labs(title = paste("Spread of Hourly Wages for","\n",input$educationrequired, "education level")) +
      guides(fill = FALSE, scale = "none") +
      theme_bw()
    
    j/j2
  })
  
  output$jmap <- renderTmap({
    j3<-tm_shape(jbuildings)+
      tm_polygons(col = "lightgrey",
                  size = 1,
                  border.col = "black",
                  border.lwd = 1)+
      tm_shape(jjobs2()) +
      tm_bubbles(col = "educationRequirement",
                 alpha = 0.7,
                 style = "jenks",
                 palette="RdYlBu",
                 size = "hourlyRate",
                 scale = 1,
                 border.col = "black",
                 border.lwd = 0.5,
                 title.col = "Education Level", id = "jobId",
                 popup.vars=c("Job ID" = "jobId","Employer ID" = "employerId", "Hourly Wage" = "hourlyRate", "Education Level" = "educationRequirement")
      ) 
  })
  ##### Shiny Server: Employer : Employment Patterns ##### 
  jobs$`mean(age)`[is.na(jobs$`mean(age)`)] <- 0
  jobs5 <- jobs %>%
    rename(averageage=`mean(age)`) %>%
    filter(averageage > 0 )
  
  jobs4 <- reactive({
    if(input$categoryj == "agegroupj") {
      jobs4 <- jobs5 %>%
        mutate(agegroupj = cut(jobs5$averageage, breaks = 4, labels = c("18-28.5","29-39", "40-49.5", "50-60" )))
    } else {
      jobs4 <- jobs 
    }
  })
  
  jobs6 <- reactive({
    if(input$yvariablej == "jobshired") {
      jobs6 <- jobs5 %>%
        group_by(averageage) %>%
        summarise(jobshired = n())
    } else {
      jobs6 <- jobs5
    }
  })
  
  output$employmentpattern <- renderPlot({
    
    j5<-ggbetweenstats(
      data = jobs4(),
      x = !!input$categoryj, 
      y = hourlyRate,
      type = input$testj,
      plot.type = input$plotTypej,
      mean.ci = TRUE, 
      pairwise.comparisons = TRUE, 
      pairwise.display = "s",
      p.adjust.method = "fdr",
      xlab = ifelse(input$categoryj == "agegroupj", "Age Group for Average Age hired ",
                    "Education Required"),
      ylab = "Hourly Rate",
      title = ifelse(input$categoryj == "agegroupj", "Are there any patterns between hourly rate and average age hired?",
                     "Are there any patterns between hourly rate and education level?"),
      messages = FALSE)
    
    j6<- ggscatterstats(
      data = jobs6(),
      x = averageage, 
      y = !!input$yvariablej,
      marginal = input$marginalj,
      xlab = "Average Age Hired",
      ylab = ifelse(input$yvariablej == "hourlyRate", "Hourly Rate", "No. of Jobs Hired for"),
      title = ifelse(input$yvariablej == "hourlyRate", "Is there a correlation between hourly rate and average age hired?",
                     "Is there a correlation between jobs hired for and age?"),
      type = input$testj2,
      conf.level = 0.95,
      bf.prior = 0.707)
    
    j5+j6 + plot_annotation(
      title = ifelse(input$categoryj =="agegroupj", "Investigating if there are any employment patterns when it comes to age",
                     "Investigating if there are any employment patterns when it comes to age and education level"),
      theme = theme(plot.title = element_text(size = 14, face = "bold"))
    )
  }) 
  ##### Shiny Server: Employer : turnover ##### 
  jobs$Turnover[is.na(jobs$Turnover)] <- 0
  output$turnover <- renderPlot({
    j7 <- jobs %>%
      filter(Turnover > 1) %>%
      ggplot(aes(x=educationRequirement)) +
      geom_bar(fill = "steelblue") +
      ylim(0, 60) + 
      geom_text(stat = "count",
                aes(label = stat(count)),
                hjust = -0.5,
                size = 4) +
      labs(title = "No.of jobs with high turnover rate by education level",
           x = "Education Level",
           y = "No. of jobs with high turnover rate") +
      coord_flip()
    
    j8 <- jobs %>%
      filter(Turnover < 1) %>%
      ggplot(aes(x=educationRequirement)) +
      geom_bar(fill = "steelblue") + 
      ylim(0, 160) +
      geom_text(stat = "count",
                aes(label = stat(count)),
                hjust = -0.5,
                size = 4) +
      labs(title = "No.of jobs with no job applicants by education level",
           x = "Education Level",
           y = "No. of jobs with no job applicants") +
      coord_flip()
    
    
    j7+j8
  })
  
  jjobs3 <- reactive({
    if(input$educationrequired2 == "All") {
      jjobs3 <- jjobs1 %>%
        filter(Turnover > 1) %>%
        filter(hourlyRate >= min(input$jobshourlyrate2) & hourlyRate <= max(input$jobshourlyrate2))
    } else if(input$educationrequired2 == "Low") {
      jjobs3 <- jjobs1 %>%
        filter(Turnover > 1) %>%
        filter(educationRequirement == "Low") %>%
        filter(hourlyRate >= min(input$jobshourlyrate2) & hourlyRate <= max(input$jobshourlyrate2))
    } else if (input$educationrequired2 == "HighSchoolOrCollege") {
      jjobs3 <- jjobs1 %>%
        filter(Turnover > 1) %>%
        filter(educationRequirement == "HighSchoolOrCollege") %>%
        filter(hourlyRate >= min(input$jobshourlyrate2) & hourlyRate <= max(input$jobshourlyrate2))
    } else if (input$educationrequired2 == "Bachelors") {
      jjobs3 <- jjobs1 %>%
        filter %>% (Turnover > 1)
      filter(educationRequirement == "Bachelors") %>%
        filter(hourlyRate >= min(input$jobshourlyrate2) & hourlyRate <= max(input$jobshourlyrate2))
    } else {
      jjobs3 <- jjobs1 %>%
        filter(Turnover > 1)
      filter(educationRequirement == "Graduate") %>%
        filter(hourlyRate >= min(input$jobshourlyrate2) & hourlyRate <= max(input$jobshourlyrate2))
    }
  })
  
  jjobs4 <- reactive({
    if(input$educationrequired3 == "All") {
      jjobs4 <- jjobs1 %>%
        filter(Turnover < 1) %>%
        filter(hourlyRate >= min(input$jobshourlyrate3) & hourlyRate <= max(input$jobshourlyrate3))
    } else if(input$educationrequired3 == "Low") {
      jjobs4 <- jjobs1 %>%
        filter(Turnover < 1) %>%
        filter(educationRequirement == "Low") %>%
        filter(hourlyRate >= min(input$jobshourlyrate3) & hourlyRate <= max(input$jobshourlyrate3))
    } else if (input$educationrequired3 == "HighSchoolOrCollege") {
      jjobs4 <- jjobs1 %>%
        filter(Turnover < 1) %>%
        filter(educationRequirement == "HighSchoolOrCollege") %>%
        filter(hourlyRate >= min(input$jobshourlyrate3) & hourlyRate <= max(input$jobshourlyrate3))
    } else if (input$educationrequired3 == "Bachelors") {
      jjobs4 <- jjobs1 %>%
        filter %>% (Turnover < 1)
      filter(educationRequirement == "Bachelors") %>%
        filter(hourlyRate >= min(input$jobshourlyrate3) & hourlyRate <= max(input$jobshourlyrate3))
    } else {
      jjobs4 <- jjobs1 %>%
        filter(Turnover < 1)
      filter(educationRequirement == "Graduate") %>%
        filter(hourlyRate >= min(input$jobshourlyrate3) & hourlyRate <= max(input$jobshourlyrate3))
    }
  })
  
  output$turnovermap <- renderTmap({
    j9<-tm_shape(jbuildings)+
      tm_polygons(col = "lightgrey",
                  size = 1,
                  border.col = "black",
                  border.lwd = 1)+
      tm_shape(jjobs3()) +
      tm_bubbles(col = "Turnover",
                 alpha = 0.7,
                 style = "jenks",
                 palette="Paired",
                 size = "hourlyRate",
                 scale = 1,
                 border.col = "black",
                 border.lwd = 0.5,
                 title.col = "Turnover Rate", id = "jobId",
                 popup.vars=c("Job ID" = "jobId","Employer ID" = "employerId", "Hourly Wage" = "hourlyRate", "Turnover Rate" = "Turnover")
      ) 
    
  })
  output$turnovermap2 <- renderTmap({
    j10<-tm_shape(jbuildings)+
      tm_polygons(col = "lightgrey",
                  size = 1,
                  border.col = "black",
                  border.lwd = 1)+
      tm_shape(jjobs4()) +
      tm_bubbles(col = "hourlyRate",
                 alpha = 0.7,
                 style = "jenks",
                 palette="RdYlGn",
                 scale = 1,
                 border.col = "black",
                 border.lwd = 0.5,
                 title.col = "Hourly Wage", id = "jobId",
                 popup.vars=c("Job ID" = "jobId","Employer ID" = "employerId", "Hourly Wage" = "hourlyRate", "Education\n Level" = "educationRequirement")
      ) 
    
  })
  
  ##### End of Q3 Server #####
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
