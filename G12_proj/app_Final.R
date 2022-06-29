##############################
##### Load Packages      #####
##############################


packages <- c('shiny','tidyverse','lubridate','zoo','ggthemes','hrbrthemes','ggdist','gghalves',
              'ggridges','patchwork','zoo', 'ggrepel','ggiraph','gganimate','scales','shiny', 'shinydashboard', 'shinythemes','tsibble','tseries',
              'plotly','ggstatsplot','forecast','tools','shinyWidgets','readxl','bslib')

for (p in packages){
  library(p, character.only=T)
}

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
participant_fin <- read_rds("data/rds/participant_fin.rds")


##### Q3 #####

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

  tabPanel("Revenue",icon = icon("dollar-sign",lib = "font-awesome"),
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
  
  ##### End of Q1 UI #####
  
  ##### Q2 UI #####
  tabPanel("Finance",icon = icon("search-dollar",lib = "font-awesome"),
           sidebarPanel(width =3,
                        selectInput("partgroup",
                                    "Select Grouping",
                                    c("Education Level" = "educationLevel","Age Group" = "agegroup")
                        ),
                        textOutput("selectedGroup1")
           ),
           mainPanel(id = "Q2A",
             width = 9,
                     plotOutput("areaplot1",
                                height = "500px")
           )
      
  ),

##### End of Q2 UI #####
  tabPanel("Jobs",icon = icon("money",lib = "font-awesome"),
    sidebarPanel(
      
    ),
    mainPanel(
      
    )
  )

##### Q3 UI #####


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
  
  
  summary_data <- reactive({
    # group data by selected options
    req(input$partgroup)
    participant_fin%>%
      group_by(!!!rlang::syms(input$partgroup),date) %>%
      summarise(income = mean(income), expense= mean(expense))
    
  })
  
  output$areaplot1 <- renderPlot({
    req(input$partgroup)
    data_for_plot <- summary_data()
    
    
    area <- data_for_plot %>%
      ggplot(aes_string(x="date", y = "income", fill = input$partgroup))+
      geom_area()+
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
            legend.title = element_text(size =16),
            legend.text = element_text(size = 16),
            plot.title = element_text(size =20,hjust = 0.5))+
      ggtitle(paste("Average Income by", input$partgroup))+
      scale_fill_viridis_d(direction = -1)+
      scale_y_continuous(breaks = seq(0,30000,5000))
    
    
    return(area)
  })
  
  ##### End of Q2 Server #####
  
  ##### Q3 Server #####
  
  
  ##### End of Q3 Server #####
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
