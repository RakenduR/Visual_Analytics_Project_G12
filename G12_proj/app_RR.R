packages <- c('shiny', 'shinydashboard', 'shinythemes', 
              'plotly', 'tidyverse', 'ggstatsplot', 
              'tools','gganimate','ggiraph','zoo')

for (p in packages){
  library(p, character.only=T)
}

participant_fin <- read_rds("data/rds/participant_fin.rds")


##############

ui <- navbarPage(
  title = "VAST Challenge 3 - Economic",
  fluid = TRUE,
  collapsible = TRUE,
  windowTitle = "VAST Challenge 3 - Economic",
  theme=shinytheme("united"),
  id = "navbarID",
  tabPanel(title = "Introduction",
           mainPanel()
             ### the rest of your code
           ),
  navbarMenu("Exploratory",
             tabPanel("Graph 1",
                      sidebarPanel("Sidebar",
                                   width = 3),
                      mainPanel(
                                width = 9,
                                fluidRow(
                                  column(width = 9,
                                         wellPanel("Test Graph",
                                           plotOutput("lineplot",
                                                      height = "500px")
                                         )
                                  )
                                )
                        )
                      ),
             tabPanel("Graph 2",
                      sidebarPanel("Sidebar",
                                   width = 3),
                      mainPanel(
                                width = 9,
                                fluidRow(
                                  column(width = 9,
                                         wellPanel("Test Graph",
                                           plotOutput("scatterplot",
                                                      height = "500px")
                                         )
                                  )
                                )
                      )
             ),
             tabPanel("Graph 3",
                      sidebarPanel("Sidebar",
                                   width = 3),
                      mainPanel(
                        width = 9,
                        fluidRow(
                          column(width = 9,
                                 wellPanel("Test Graph",
                                           plotOutput("tooltipplot",
                                                      height = "500px")
                                 )
                          )
                        )
                      )
             )
  )
  
)

#========================#
###### Shiny Server ######
#========================#

server <- function(input, output){
  
  output$lineplot <- renderPlot({
    
    
   line <- participant_fin %>%
     ggplot(aes(x=date, y = income, group =participantId, color =educationLevel))+
     geom_line()+
     ylab("Income")+
     xlab("Month, Year")+
     theme(panel.grid.minor.y = element_blank(),
           panel.grid.minor.x = element_line(colour = "grey90"),
           panel.grid.major.x = element_line(colour = "grey90"),
           panel.grid.major.y = element_line(colour = "grey90"),
           panel.background = element_rect(fill = "white"),
           axis.text.x = element_text(size =16, angle = 45, margin = margin(t = 30,r=30)),
           axis.text.y = element_text(size =16),
           axis.line = element_line(color="grey25", size = 0.2),
           axis.title = element_text(size=16),
           legend.title = element_text(size =16),
           legend.text = element_text(size = 16),
           plot.title = element_text(size =20,hjust = 0.5))+
     ggtitle("Income of Participants by Education Level")
   
   
   return(line)
    
  })
  output$scatterplot <- renderPlot({
    
    
    scatter <- participant_fin %>%
      filter(date >= 'Apr 2022') %>%
      transform(date = as.Date(date, frac = 1)) %>%
      ggplot(aes(x=income, y = abs(expense), size = savings, color = educationLevel))+
      geom_point(alpha=0.7) +
      ggtitle("Income vs Expense by different Education Levels") +
      ylab("Expense") +
      xlab("Income")+
      theme_minimal() +
      theme(axis.line = element_line(size = 0.5),
            axis.text = element_text(size = 16),
            axis.title = element_text(size=16),
            axis.title.y = element_text(angle = 0),
            legend.title = element_text(size =16),
            legend.text = element_text(size = 16),
            plot.title = element_text(size =20,hjust = 0.5))+
      labs(title ='Period : {frame_time}')+
      transition_time(date)+
      ease_aes('linear')
    
    
    return(scatter)
    
  })
  output$tooltipplot <- renderPlot({
    
    
    participant_fin$tooltip <- c(paste0(
      "Id = ", participant_fin$participantId,
      "\n Income = $", round(participant_fin$income,digits = 0),
      "\n Education :",participant_fin$educationLevel))
    
    p1 <- participant_fin %>%
      ggplot(aes(x=date, y = income, group =participantId, color =educationLevel))+
      geom_line_interactive(aes(tooltip = tooltip),size =0.4)+
      ylab("Income")+
      xlab("Month, Year")+
      theme(panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_line(colour = "grey90"),
            panel.grid.major.x = element_line(colour = "grey90"),
            panel.grid.major.y = element_line(colour = "grey90"),
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(size =16, angle = 45, margin = margin(t = 30,r=30)),
            axis.text.y = element_text(size =16),
            axis.line = element_line(color="grey25", size = 0.2),
            axis.title = element_text(size=16),
            legend.title = element_text(size =16),
            legend.text = element_text(size = 16),
            plot.title = element_text(size =20,hjust = 0.5))+
      ggtitle("Income of Participants by Education Level")
    
    interactiveplot <- girafe(
      ggobj = p1,
      width_svg = 12,
      height_svg = 12*0.618
    )
    
    
    return(interactiveplot)
    
  })
}

shinyApp(ui = ui, server = server)
