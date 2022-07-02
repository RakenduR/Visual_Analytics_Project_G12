packages <- c('shiny', 'shinydashboard', 'shinythemes','tsibble','tseries',
              'plotly', 'tidyverse', 'ggstatsplot','zoo','forecast',
              'tools','shinyWidgets','readxl','bslib','dplyr','lubridate','patchwork','tmap', 'sf', 'leaflet')

for (p in packages){
  library(p, character.only=T)
}

###Jeremiah's Data###
jobs <- read_rds("data/rds/jobs.rds")
jbuildings <- read_sf("data/jBuildings.csv",
                     options = "GEOM_POSSIBLE_NAMES=location")

jjobs <- read_sf("data/jjobs.csv",
                 options = "GEOM_POSSIBLE_NAMES=location")

###End###

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
                                     selected = "weekly")
                                    ),
                        
                      mainPanel(
                        width = 9,
                        fluidRow(
                          column(width = 9,
                                 wellPanel("Test Graph",
                                           plotOutput("revenue",
                                                      height = "500px")
                                 )
                          )
                        )
                      )
                      

             )
             ),
  navbarMenu("Analysis",icon =icon("cog", lib = "glyphicon"),
             tabPanel("Between group"),
             tabPanel("Within group"),
             tabPanel("ANOVA",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(inputId = "xvariable",
                                                 label = "Select x-variable:",
                                                 choices = c("Class" = "CLASS",
                                                             "Gender" = "GENDER",
                                                             "Race" = "RACE"),
                                                 selected = "GENDER"),
                                     selectInput(inputId = "yvariable",
                                                 label = "Select y-variable:",
                                                 choices = c("English" = "ENGLISH",
                                                              "Maths" = "MATHS",
                                                              "Science" = "SCIENCE"),
                                                 selected = "ENGLISH"),
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
                                     selectInput(inputId = "xvariable1",
                                                 label = "Select x-variable:",
                                                 choices = c("English" = "ENGLISH",
                                                             "Maths" = "MATHS",
                                                             "Science" = "SCIENCE"),
                                                 selected = "ENGLISH"),
                                     selectInput(inputId = "yvariable1",
                                                 label = "Select y-variable:",
                                                 choices = c("English" = "ENGLISH",
                                                             "Maths" = "MATHS",
                                                             "Science" = "SCIENCE"),
                                                 selected = "MATHS"),
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
            ))
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
    
    ggbetweenstats(
      data = exam,
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
  })
  
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
##### Shiny Server: Correlation Analysis #####   
  output$corrPlot <- renderPlot({
    input$goButton1
    set.seed(1234)
    
  ggscatterstats(
    data = exam,
    x = !!input$xvariable1, 
    y = !!input$yvariable1,
    marginal = input$marginal,
    title = isolate({
      toTitleCase(input$plotTitle1)
    }),
    conf.level = 0.95,
    bf.prior = 0.707)
    })

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
}

shinyApp(ui = ui, server = server)
