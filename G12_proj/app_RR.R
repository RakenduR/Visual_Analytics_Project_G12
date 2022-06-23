packages <- c('shiny', 'shinydashboard', 'shinythemes', 
              'plotly', 'tidyverse', 'ggstatsplot', 
              'tools')

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
           mainPanel(
             img(src='Image/img.jpg', align = "right"),
             ### the rest of your code
           )),
  navbarMenu("Exploratory",
             tabPanel("Something",
                      sidebarPanel("Sidebar",
                                   width = 3),
                      mainPanel("Hello",
                                width = 9,
                                fluidRow(
                                  column(width = 9,
                                         wellPanel(
                                           plotOutput("lineplot",
                                                      height = "500px")
                                         )
                                  )
                                )
                        )
                      )
             ),
  
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


}

shinyApp(ui = ui, server = server)
