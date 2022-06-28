packages <- c('shiny', 'shinydashboard', 'shinythemes','tsibble','tseries',
              'plotly', 'tidyverse', 'ggstatsplot','zoo','forecast',
              'tools','shinyWidgets','readxl','bslib','dplyr','lubridate')

for (p in packages){
  library(p, character.only=T)
}

exam <- read_csv("data/Exam_data.csv")
travel_filt <- readRDS('data/rds/travel_filt.rds')
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
         travelTime = travelEndTime- travelStartTime,
         participantId=as.character(participantId),
         purpose=as.character(purpose))
data_travel$timeSpent <- as.numeric(as.character(data_travel$timeSpent))
data_travel$travelTime <- as.numeric(as.character(data_travel$travelTime))

data_travel <- data_travel[,c("participantId","travelStartLocationId", "travelEndLocationId", "purpose",  "amountSpent","timeSpent","travelTime","weekday","day","month","year","monthYear")]

group_pub <-merge(x=data_travel, y=pubs, by.x = "travelEndLocationId", by.y = "pubId")
group_pub$venue <- "Pub"
group_pub$foodCost <- 0.00

group_restaurant<-merge(x=data_travel, y=restaurants, by.x = 'travelEndLocationId', by.y =  'restaurantId')
group_restaurant$venue <- "Restaurant"
group_restaurant$hourlyCost <- 0.00

group <-rbind(group_pub, group_restaurant)

period <- unique(na.omit(group$monthYear))

participant <- unique(na.omit(group$participantId))

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  navbarPage(
  title = "VAST Challenge 3 - €conomic",
  fluid = TRUE,
  collapsible = TRUE,
  theme=shinytheme("united"),
  id = "navbarID",
  tabPanel(title = "Overview",icon = icon("globe"),
           mainPanel(
             ### the rest of your code
           )),
  navbarMenu("Exploratory", icon =icon("list-alt"),
             tabPanel("Revenue",
                      sidebarPanel(
                        width = 3,
                        radioButtons("venue", 
                                     label = "Select the Venue:",
                                     choices = list("Pub" = "Pub", 
                                                    "Restaurant" = "Restaurant"), 
                                     selected = "Pub"
                                     ),
                        # add selection for participant ID for graph 2
                        # add axis scale selection for plot 3
                        
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
                                 
                                           plotlyOutput("revenue"),
                                           plotlyOutput("box"),
                                           plotlyOutput("box_p")
                                 
                          )
                        )
                      )
                      

             )
             ),
  navbarMenu("Analysis",icon =icon("cog", lib = "glyphicon"),
             tabPanel("Time Series "),
             tabPanel("ANOVA",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(input = "period", 
                                                 label = "Choose Period:",
                                                 choices = c(period),
                                                 multiple = TRUE,
                                                 selected = period[1]),
                                     
                                     selectInput(input = "participant", 
                                                 label = "Choose ParticipantID:",
                                                 choices = c(participant),
                                                 multiple = TRUE,
                                                 selected = participant[1]),
                                     
                                     selectInput(inputId = "xvariable", 
                                                 label = "Select x-variable:",
                                                 choices = c("Participant" = "participantId",
                                                             "venue" = "venue"),
                                                 selected = "participantId"),
                                     
                                     selectInput(inputId = "yvariable",
                                                 label = "Select y-variable:",
                                                 choices = c("amount" = "amountSpent",
                                                              "Time Spent" = "timeSpent",
                                                              "Travel Time" = "travelTime"),
                                                 selected = "amountSpent"),
                                     
                                     selectInput(inputId = "test",
                                                 label = "Type of statistical test:",
                                                 choices = c("parametric" = "p",
                                                             "nonparametric" = "np",
                                                             "robust" = "r",
                                                             "Bayes Factor" = "bf"),
                                                 selected = "p"),
                                     
                                     selectInput(inputId = "plotType",
                                                 label = "Type of plot:",
                                                 choices = c("boxviolin" = "boxviolin",
                                                             "box" = "box",
                                                             "violin" = "violin"),
                                                 selected = "boxviolin"),
                                     textInput(inputId = "plotTitle",
                                               label = "Plot title",
                                               placeholder = "Enter text to be used as plot title"),
                                     actionButton(inputId = "goButton", 
                                                  "Go!")
                        ),
                        mainPanel(width = 9,
                                  box(
                                    plotOutput("boxplot",
                                               height = "500px"))
                        )
                      )),
             tabPanel("Correlation",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(inputId = "xvariable", 
                                                 label = "Select x-variable:",
                                                 choices = c("amount" = "amountSpent",
                                                             "Time Spent" = "timeSpent",
                                                             "Travel Time" = "travelTime"),
                                                 selected = "timeSpent"),
                                     
                                     selectInput(inputId = "yvariable",
                                                 label = "Select y-variable:",
                                                 choices = c("amount" = "amountSpent",
                                                             "Time Spent" = "timeSpent",
                                                             "Travel Time" = "travelTime"),
                                                 selected = "amountSpent"),
                                     selectInput(inputId = "test1",
                                                 label = "Type of statistical test:",
                                                 choices = c("parametric" = "p",
                                                             "nonparametric" = "np",
                                                             "robust" = "r",
                                                             "Bayes Factor" = "bf"),
                                                 selected = "p"),
                                     checkboxInput(inputId = "marginal", 
                                                   label = "Display marginal graphs", 
                                                   value = TRUE),
                                     textInput(inputId = "plotTitle1",
                                               label = "Plot title",
                                               placeholder = "Enter text to be used as plot title"),
                                     actionButton(inputId = "goButton1", 
                                                  "Go!")
                        ),
                        mainPanel(width = 9,
                                  box(
                                    plotOutput("corrPlot",
                                               height = "500px"))
                        )
                      ))
  ),
  navbarMenu("Predictive",icon = icon("bar-chart-o"),
             tabPanel("Principal Component Analysis"),
             tabPanel("Hierarchical Custering"),
             tabPanel("kmeans Clustering"),
             tabPanel("Multiple Linear Regression"))
)
)

#========================#
###### Shiny Server ######
#========================#

server <- function(input, output){
  
##### Shiny Server: Between Group Analysis ##### 
  output$boxplot <- renderPlot({
    input$goButton
    set.seed(1234)
    print(input$xvariable)
    print(input$yvariable)
    p <- group %>%
      filter(monthYear == input$period) %>%
    ggbetweenstats(
      x = !!input$xvariable, 
      y = !!input$yvariable,
      type = input$test,
      title = isolate({
        toTitleCase(input$plotTitle)
      }),
      plot.type = input$plotType,
      mean.ci = TRUE, 
      pairwise.comparisons = TRUE, 
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE)
    p
  })
  
 
  output$revenue <- renderPlotly({
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
        x= print(input$group),
        title = print(input$venue),
        caption = "Ohio USA"
      ) +
      theme_minimal()+
      theme(axis.ticks.x= element_blank(),
            panel.background= element_blank(), 
            legend.background = element_blank(),
            plot.title = element_text(size=12, face="bold",hjust = 0.5),
            plot.subtitle = element_text(hjust = 1),
            plot.caption = element_text(hjust = 0),
            axis.title.y= element_text(angle=0))
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  output$box <- renderPlotly({
    
    boxplot <- group %>%
      filter(monthYear == input$period, venue == input$venue ) %>%
      group_by(!!!rlang::syms(input$group), travelEndLocationId) %>%
      summarise(amountSpent = (sum(amountSpent))) %>%
      ggplot(aes_string(x=input$group, y="amountSpent", group="travelEndLocationId", color =input$group)) + 
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60),
            axis.title.x = element_blank())
    
    ggplotly(boxplot)
    
  })
  
  
  output$box_p <- renderPlotly({
    print(input$period)
    boxplot <- group %>%
      filter(monthYear == input$period, venue == input$venue ) %>%
      ggplot(aes(x = travelEndLocationId, y= amountSpent, color = travelEndLocationId)) + 
      scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100))+
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60),
            axis.title.x = element_blank())
    
    ggplotly(boxplot)
    
  })
  
##### Shiny Server: Correlation Analysis #####   
  output$corrPlot <- renderPlot({
    input$goButton1
    set.seed(1234)
   corr<-  group %>%
      filter(monthYear == input$period, venue == input$venue)
   
  ggscatterstats(
    data = corr,
    x = !!input$xvariable1, 
    y = !!input$yvariable1,
    marginal = input$marginal,
    title = isolate({
      toTitleCase(input$plotTitle1)
    }),
    conf.level = 0.95,
    bf.prior = 0.707)
    })
  
}

shinyApp(ui = ui, server = server)
