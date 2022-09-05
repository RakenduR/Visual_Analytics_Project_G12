##############################
##### Load Packages      #####
##############################


pacman::p_load(shiny,tidyverse,lubridate,ggthemes,hrbrthemes,ggdist,gghalves,
              ggridges,zoo,ggrepel,ggiraph,gganimate,scales,shiny, shinydashboard, shinythemes,
              tsibble,tseries,plotly,ggstatsplot,forecast,tools,shinyWidgets,readxl,bslib,patchwork,tmap, sf,
              rstantools, reactable, reactablefmtr,gt, gtExtras, fpp3,DT)



##############################
##### Read & Clean Data  #####
##############################

##### Q1 #####

pubs_loc <- read_sf("data/Pubs.csv", options = "GEOM_POSSIBLE_NAMES=location")
restaurants_loc <- read_sf("data/Restaurants.csv", options = "GEOM_POSSIBLE_NAMES=location")
buildings <- read_sf("data/Buildings.csv", options = "GEOM_POSSIBLE_NAMES=location")

group <- read_rds("data/rds/group.rds")
group_loc <- read_rds("data/rds/group_loc.rds")

venueid_pub <- read_rds("data/rds/venueid_pub.rds")
venueid_rest <- read_rds("data/rds/venueid_rest.rds")
period <- read_rds("data/rds/period.rds")

participant <- unique(na.omit(group$participantId))

venueid <- unique(na.omit(group$travelEndLocationId))

group_spars <- group
group_spars$Venue <- group_spars$travelEndLocationId

##### Q2 #####

# data for stacked area plot
dataset_1 <- read_rds("data/rds/dataset_1.rds")

# data for facet line plot for income and expense
dataset_3 <- read_rds("data/rds/dataset_3.rds")

# data for calendar heatmap
dataset_4 <- read_rds("data/rds/dataset_4.rds")

##### Q3 #####

jobs <- read_rds("data/rds/jobs.rds")

jjobs <- read_sf("data/jjobs.csv",
                 options = "GEOM_POSSIBLE_NAMES=location")

##############################
##### UI                 #####
##############################




ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  navbarPage(
  title = "Vastly Challenging Economics",
  fluid = TRUE,
  collapsible = TRUE,
  windowTitle = "VAST Challenge 3 - Economic",
  theme=shinytheme("united"),
  id = "navbarID",
  tabPanel(title = "Overview",icon = icon("globe"),
           mainPanel(width = 12,
                     fluidRow(
                       column(width = 10,
                              ##### Introduction #####
                              h1("Once upon a time.."),
                              p("Engagement was a small town in Ohio. Not anymore. It is now buzzling with activity and experiencing an economic growth spurt.
                To keep up with the sudden emergence of businesses and increase in number of occupants, it is important that infrastructure
                undergoes rapid but planned development. To enable such a well planned development, let us explore, analyse and predict the 
                economic criteria of Engagement."),
                              h3("Lets begin our Journey through the streets of Engagement.."),
                              h4("Directions:"),
                              p("Under the Business tab, you will find a detailed study of the businesses in Engagement.We will analyse how these businesses are performing"),
                              p("Under the Finance tab, we will study the income and expenditure of the residents.We will compare the Financial situation of various groups
               of residents having different Educational Qualifications and from different age groups.We will also analyse the spending patterns of the 
               residents to understand what are the infrastructure and amenities that needs to be developed.We will also predict the growth in income of the 
               residents which will in turn help to understand their spending capacity."),
                              p("Under the Employers tab, we will study the employement patterns by the various establishments in Ohio and their location. We will also 
               study the employment patterns over different groups of residents with different Education Levels and belonging to different age groups.We 
               will also study the job opportunities in Engagement to understand the jobs with high turnover."),
                              h4("For detailed steps, please download our Userguide by clicking on the button in the top right corner")
                       ),
                       column(width = 2,
                                         downloadButton("downloadData", "Userguide")
                           )
                         )
                       )
  ),
  ##### Q1 UI #####
navbarMenu("Business",icon = icon("dollar-sign"),
           tabPanel("Change in revenue",
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
                               h4("Change in Revenue for restaurants and Pubs for city of Engagement, Ohio \n\n\n"),
                               plotlyOutput("revenue", 
                                            height = "500px",
                                            width = "850px")
                               
                        )
                      )
                    )
                    
                    
           ),
           tabPanel("Grouped change in Revenue",
                    sidebarPanel(
                      width = 3,
                      radioButtons("venue_2", 
                                   label = "Select the Venue:",
                                   choices = list("Pub" = "Pub", 
                                                  "Restaurant" = "Restaurant"), 
                                   selected = "Pub"
                      ),
                      # add selection for participant ID for graph 2
                      # add axis scale selection for plot 3
                      
                      selectInput(input = "period_2", 
                                  label = "Choose Period:",
                                  choices = c(period),
                                  multiple = TRUE,
                                  selected = period[1]),
                      
                      radioButtons("group_2", 
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
                               h4("Change in Revenue for restaurants and Pubs grouped over chosen time, Ohio \n\n\n"),
                               plotlyOutput("box",
                                            height = "500px",
                                            width = "850px")
                               
                        )
                      )
                    )
                    
                    
           ),
           tabPanel("Map of Venue visited by Participant",
                    sidebarPanel(
                      width = 3,
                      
                      selectInput(input = "period_map", 
                                  label = "Choose Period:",
                                  choices = c(period),
                                  multiple = TRUE,
                                  selected = period[1]),
                      
                      selectInput(input = "participant_map", 
                                  label = "Choose participant:",
                                  choices = c(participant),
                                  multiple = TRUE,
                                  selected = participant[1])
                      
                    ),
                    
                    mainPanel(
                      width = 9,
                      fluidRow(
                        column(width = 12,
                               h4("Plot of the Restaurants and Pubs visited by each selected Participant for city of Engagement, Ohio \n\n\n"),
                               h4("Red dots represent restaurants visited by the Participant"),
                               h4("Yellow dots represent pubs visited by the Participant"),
                               tmapOutput("map",
                                          height = "550px",
                                          width = "700px")
                        )
                      )
                    )
                    
           ),
           tabPanel("Sparksline for Revenue",
                    sidebarPanel(
                      width = 3,
                      
                      radioButtons("venue_sparks", 
                                   label = "Select the Venue:",
                                   choices = list("Pub" = "Pub", 
                                                  "Restaurant" = "Restaurant"), 
                                   selected = "Pub"
                      ),
                      radioButtons("year_sparks", 
                                   label = "Select the Year:",
                                   choices = list("2022" = "2022", 
                                                  "2023" = "2023"), 
                                   selected = "2022")
                      
                    ),
                    
                    mainPanel(
                      width = 9,
                      fluidRow(
                        column(width = 12,
                               h4("Statistics for restaurants and Pubs for city of Engagement, Ohio \n\n\n"),
                               gt_output("sparks")
                               
                        )
                      )
                    )
                    
                    
           ),
           tabPanel("ANOVA",

                      sidebarPanel(width = 3,
                                   selectInput(input = "period_anova", 
                                               label = "Choose Period:",
                                               choices = c(period),
                                               multiple = TRUE,
                                               selected = period[1]),
                                   
                                   radioButtons("venue_anova", 
                                                label = "Select the Venue:",
                                                choices = list("Pub" = "Pub", 
                                                               "Restaurant" = "Restaurant"), 
                                                selected = "Pub"
                                   ),
                                   
                                   selectInput(input = "venueid_anova", 
                                               label = "Select Venue (Restaurant/Pub ID):",
                                               choices = c(venueid),
                                               multiple = TRUE,
                                               selected = venueid[1]),
                                   
                                   selectInput(inputId = "yvariable_anova",
                                               label = "Select y-variable:",
                                               choices = c("Amount" = "amountSpent",
                                                           "Time Spent" = "timeSpent",
                                                           "Travel Time" = "travelTime"),
                                               selected = "amountSpent"),
                                   
                                   selectInput(inputId = "test_anova",
                                               label = "Type of statistical test:",
                                               choices = c("parametric" = "p",
                                                           "nonparametric" = "np",
                                                           "robust" = "r",
                                                           "Bayes Factor" = "bf"),
                                               selected = "p"),
                                   
                                   selectInput(inputId = "plotType_anova",
                                               label = "Type of plot:",
                                               choices = c("boxviolin" = "boxviolin",
                                                           "box" = "box",
                                                           "violin" = "violin"),
                                               selected = "boxviolin"),
                                   
                                   sliderInput(inputId = "outliers_anova",
                                               label = "Select max value for outliers",
                                               min = 10,
                                               max = 1000,
                                               step = 10,
                                               value = 100),
                                   
                                   textInput(inputId = "plotTitle_anova",
                                             label = "Plot title",
                                             placeholder = "Enter text to be used as plot title"),
                                   
                                   actionButton(inputId = "goButton", 
                                                "Go!")
                      ),
                      
                      mainPanel(
                        width = 9,
                        fluidRow(
                          column(width = 12,
                                 h4("ANOVA analysis for restaurants and Pubs for city of Engagement, Ohio \n\n\n"),
                                 box(
                                   plotOutput("boxplot",
                                              height = "550px",
                                              width = "700px"))
                                 
                          )
                        )
                      )
            ),
           tabPanel("Correlation",

                      sidebarPanel(width = 3,
                                   selectInput(input = "period_corr", 
                                               label = "Choose Period:",
                                               choices = c(period),
                                               multiple = TRUE,
                                               selected = period[1]),
                                   
                                   selectInput(input = "venueid_corr", 
                                               label = "Select Venue (Restaurant/Pub ID):",
                                               choices = c(venueid),
                                               multiple = TRUE,
                                               selected = venueid[1]),
                                   
                                   selectInput(inputId = "xvariable_corr", 
                                               label = "Select x-variable:",
                                               choices = c("amount" = "amountSpent",
                                                           "Time Spent" = "timeSpent",
                                                           "Travel Time" = "travelTime"),
                                               selected = "timeSpent"),
                                   
                                   selectInput(inputId = "yvariable_corr",
                                               label = "Select y-variable:",
                                               choices = c("amount" = "amountSpent",
                                                           "Time Spent" = "timeSpent",
                                                           "Travel Time" = "travelTime"),
                                               selected = "amountSpent"),
                                   
                                   selectInput(inputId = "test_corr",
                                               label = "Type of statistical test:",
                                               choices = c("parametric" = "p",
                                                           "nonparametric" = "np",
                                                           "robust" = "r",
                                                           "Bayes Factor" = "bf"),
                                               selected = "p"),
                                   
                                   checkboxInput(inputId = "marginal_corr", 
                                                 label = "Display marginal graphs", 
                                                 value = TRUE),
                                   
                                   textInput(inputId = "plotTitle_corr",
                                             label = "Plot title",
                                             placeholder = "Enter text to be used as plot title"),
                                   actionButton(inputId = "goButton_corr", 
                                                "Go!")
                      ),
                      mainPanel(
                        width = 9,
                        fluidRow(
                          column(width = 12,
                                 box(
                                   plotOutput("corrPlot",
                                              height = "550px",
                                              width = "700px"))
                                 
                          )
                        )
                      )
                      
                    )
  
),
  
  ##### End of Q1 UI #####
  
  ##### Q2 UI #####
navbarMenu("Finance",icon = icon("chart-line"),
           tabPanel("Income and Expense",
                    sidebarPanel(width =3,
                                 selectInput("eduselect",
                                             "Select Education Level",
                                             choices = c("Bachelors" = "Bachelors",
                                                         "Graduate" = "Graduate",
                                                         "High School or College" = "HighSchoolOrCollege",
                                                         "Low" = "Low"),
                                             selected = "Bachelors"
                                 ),
                                 selectInput("ageselect",
                                             "Select Age Group",
                                             choices = c(
                                                          "18-30" = "18-30",
                                                           "30-40" = "30-40",
                                                           "40-50" = "40-50",
                                                           "50-60" = "50-60"),
                                             selected = "18-30")
                                 ),
                    
                    mainPanel(width = 9,
                              plotOutput("plot1",
                                         height = "500px")
                              
                              
                    )
           ),
           tabPanel("Finance Variation ",
                    mainPanel(width = 12,
                              fluidRow(
                                column(width = 2),
                                column(width = 8,
                                            plotlyOutput("plot2",
                                                         height = "500px",
                                                         width = "1300px"),
                                       h4("Click on a plot to enlarge")

                                ),
                                column(width = 2)
                                ),
                              fluidRow(
                                column(width = 3),

                                column(width = 6,
                                         plotOutput("plot2a",
                                                      height = "500px",
                                                      width = "600px")

                                ),
                                column(width = 3)
                              )
                    )
                    
           ),
           tabPanel("Expense Calendar",
                    h4("Variations in Expenditure of various groups"),
                    sidebarPanel(width =3,
                                 radioButtons("edulevel", 
                                              label = "Select Education Level:",
                                              choices = c("All" = "All",
                                                          "Bachelors" = "Bachelors",
                                                          "Graduate" = "Graduate",
                                                          "High School or College" = "HighSchoolOrCollege",
                                                          "Low" = "Low"), 
                                              selected = "All"
                                 ),
                                 radioButtons("agelevel", 
                                              label = "Select Age Group:",
                                              choices = c("All" = "All",
                                                          "18-30" = "18-30",
                                                          "30-40" = "30-40",
                                                          "40-50" = "40-50",
                                                          "50-60" = "50-60"), 
                                              selected = "All"
                                 )
                    ),
                    mainPanel(width = 9,
                              plotOutput("plot3",
                                         height = "500px")
                    )
                    
           
           )
),


##### End of Q2 UI #####


##### Q3 UI #####
navbarMenu("Employer",icon = icon("chart-bar"),
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
                    mainPanel(width = 9, 
                              fluidRow(
                                column(width = 6,
                                       plotOutput("Wagespread", height = "850px")
                                       ), 
                                column(width = 6, 
                                       tmapOutput("jmap", height = "850px")
                                       )
                                )
                              )
                    ),
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
                    mainPanel(width = 9, 
                              plotOutput("employmentpattern", height = "600px")
                              )
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
                    mainPanel(width = 9, 
                              plotOutput("turnover", height = "180px"), 
                              fluidRow(
                                column(width = 6, 
                                       tmapOutput("turnovermap", height = "700px")
                                       ), 
                                column(width = 6, 
                                       tmapOutput("turnovermap2", height = "700px")
                                       )
                                )
                              )
           )
          )



)
##### End of Q3 UI #####
)
##############################
##### Server Side        #####
##############################
server <- function(input, output) {

  output$downloadData <- downloadHandler(
    filename = "Userguide.pdf",
    content = function(file) {
      file.copy("www/Userguide.pdf", file)
    }
  )

  ##### Q1 Server #####
  
  ## Revenue
  output$revenue <- renderPlotly({
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
  
  ## boxplots
  output$box <- renderPlotly({
    
    boxplot <- group %>%
      filter(monthYear == input$period_2, venue == input$venue_2 ) %>%
      group_by(!!!rlang::syms(input$group_2), travelEndLocationId ) %>%
      summarise(amountSpent = (sum(amountSpent))) %>%
      ggplot(aes_string(x=input$group_2, y="amountSpent", group="travelEndLocationId", color =input$group_2)) + 
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60),
            axis.title.x = element_blank())+
      labs(
        y= 'Revenue (Thousands$)',
        x= print(input$group_2),
        title = print(input$venue),
        caption = "Ohio USA"
      ) 
    
    ggplotly(boxplot)
    
  })
  
  ##Sparksline
  output$sparks <- render_gt({
    report <- group_spars %>%
      filter(year == input$year_sparks, venue == input$venue_sparks) %>%
      group_by(Venue, monthYear) %>%
      summarise(amountSpent = sum(amountSpent)) %>%
      ungroup()
    
    spark <- report %>%
      group_by(Venue) %>%
      summarize('Monthly Revenue' = list(amountSpent), 
                .groups = "drop")
    
    revenue <- report %>% 
      group_by(Venue) %>% 
      summarise("Min" = min(amountSpent, na.rm = T),
                "Max" = max(amountSpent, na.rm = T),
                "Average" = mean(amountSpent, na.rm = T)
      )
    revenue_data = left_join(revenue, spark)
    revenue_data %>%
      gt() %>%
      gt_plt_sparkline('Monthly Revenue')
    
    
  })
  
  ## Map
  output$map <- renderTmap({
    
    group_filt_loc <- group_loc %>%
      filter(participantId == input$participant_map, monthYear == input$period_map)
    
    restaurants <- unique(na.omit(group_filt_loc$travelEndLocationId))
    
    restaurants_modified<- restaurants_loc %>%
      filter(restaurantId %in% c(restaurants))
    
    pubs <- unique(na.omit(group_filt_loc$travelEndLocationId))
    
    pubs_modified<- pubs_loc %>%
      filter(pubId %in% c(pubs))
    
    tm_shape(buildings)+
      tm_polygons(col = "grey60",
                  size = 1,
                  border.col = "black",
                  border.lwd = 1) +
      tm_shape(restaurants_modified)+
      tm_dots(col = "red")+
      tm_shape(pubs_modified)+
      tm_dots(col = "yellow")
    
  })
  
  ##  ANOVA
  
  output$boxplot <- renderPlot({
    input$goButton
    set.seed(1234)
    
    p <- group %>%
      filter(monthYear == input$period_anova, venue == input$venue_anova)%>%
      filter(travelEndLocationId == input$venueid_anova, amountSpent <= input$outliers_anova)
    
    ggbetweenstats(
      data = p,
      x = travelEndLocationId, 
      y = !!input$yvariable_anova,
      type = input$test_anova,
      title = isolate({
        toTitleCase(input$plotTitle_anova)
      }),
      plot.type = input$plotType_anova,
      mean.ci = TRUE, 
      pairwise.comparisons = TRUE, 
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE)
    #scale_y_continuous(breaks = seq(0, !!input$scaley, by = !!input$scalediff), limits = c(0, !!input$scaley))
    #scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100))
  })
  
  ##Correlation Analysis
  output$corrPlot <- renderPlot({
    input$goButton1
    set.seed(1234)
    corr<-group %>%
      filter(monthYear == input$period_corr) %>%
      filter(travelEndLocationId == input$venueid_corr)
    
    ggscatterstats(
      data = corr,
      x = !!input$xvariable_corr, 
      y = !!input$yvariable_corr,
      marginal = input$marginal_corr,
      title = isolate({
        toTitleCase(input$plotTitle_corr)
      }),
      conf.level = 0.95,
      bf.prior = 0.707)+
      scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100))
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
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_line(colour = "grey90"),
            panel.grid.major.x = element_line(colour = "grey90"),
            panel.grid.major.y = element_line(colour = "grey90"),
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(size =16, angle = 0),
            axis.text.y = element_text(size =16),
            axis.line = element_line(color="grey25", size = 0.02),
            axis.title = element_text(size=16),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            plot.title = element_text(size =20,hjust = 0.5)) +
      scale_fill_discrete(name = "Category")
    
  })

  
  output$plot2 <- renderPlotly({

    p <- dataset_3 %>%
      mutate(col_001 = paste(educationLevel, agegroup, sep=",")) %>%
      ggplot(aes(x=YearMon, y = amount, group = revenue, key =col_001))+
      geom_line(aes(color = revenue), size = 1)+
      geom_text(aes(label = Label),nudge_x = 0.1, size = 3) +
      facet_grid(educationLevel ~ agegroup)+
      ylab("Income")+
      xlab("Month, Year")+
      theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_line(colour = "grey90"),
            panel.grid.major.x = element_line(colour = "grey90"),
            panel.grid.major.y = element_line(colour = "grey90"),
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(size =8, angle = 60, margin = c(-10,0,0,0)),
            axis.text.y = element_text(size =8),
            axis.line = element_line(color="grey25", size = 0.02),
            axis.title.x = element_text(size=10, margin = c(20,0,0,0)),
            axis.title.y = element_text(size=10, angle = 90),
            legend.position = "none",
            plot.title = element_text(size =20,hjust = 0.5))+
      ggtitle("Average Income by Education Level")
    
    ggplotly(p)
    
  })
  
  
  output$plot2a <- renderPlot({
    
    selected_edu_1 = NULL
    if(!is.null(event_data("plotly_click"))){
      selected_key_1<- event_data("plotly_click")$key
      selected_edu_1 <- unlist(strsplit(toString(selected_key_1[1]),split=","))[1]
      
      selected_age_1 <- unlist(strsplit(toString(selected_key_1[1]),split=","))[2]
    }
    
    
    
    if(!is.null(selected_edu_1)){
dataset_3 %>%
        filter(educationLevel == selected_edu_1) %>%
        filter(agegroup == selected_age_1) %>%
        ggplot(aes(x=YearMon, y = amount, group = revenue))+
        geom_line(aes(color = revenue), size = 1)+
        geom_text(aes(label = Label),nudge_x = 0.1, size = 4) +
        ylab("Income\nand\nExpense")+
        xlab("Month, Year")+
        theme( panel.grid.minor.y = element_blank(),
               panel.grid.minor.x = element_line(colour = "grey90"),
               panel.grid.major.x = element_line(colour = "grey90"),
               panel.grid.major.y = element_line(colour = "grey90"),
               panel.background = element_rect(fill = "white"),
               axis.text = element_text(size =12),
               axis.line = element_line(color="grey25", size = 0.02),
               axis.title.x = element_text(size=14),
               axis.title.y = element_text(size=14, angle = 0),
               legend.position = "none",
               plot.title = element_text(size =16,hjust = 0.5))+
        ggtitle(paste0("Income and Expense, Education = ", selected_edu_1, ", Age Group = ", selected_age_1))+
        scale_y_continuous(breaks = seq(0, 10000, by = 2000), limits = c(0, 10000))


    }
  })
  
  
  plot3_data <- reactive({
    
    req(input$edulevel)
    req(input$agelevel)
    
    if(input$edulevel == 'All' && input$agelevel == 'All'){
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
      theme(axis.text = element_text(size = 10),
            axis.ticks= element_blank(),
            axis.text.y = element_blank(),
            legend.title = element_text(size =16),
            legend.text = element_text(size = 16),
            strip.text.x = element_text(size = 14),
            plot.title = element_text(size = 14, hjust = 0.5))+
      ggtitle("Average Daily Expense of Residents")
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
    rename('AverageAgeHired'='mean(age)') %>%
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
    j3<-tm_shape(buildings)+
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
    j9<-tm_shape(buildings)+
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
    j10<-tm_shape(buildings)+
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
