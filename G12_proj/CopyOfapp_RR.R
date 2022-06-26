packages <- c('shiny','tidyverse','lubridate','zoo','ggthemes','hrbrthemes','ggdist','gghalves',
              'ggridges','patchwork','zoo', 'ggrepel','ggiraph','gganimate','scales')

for (p in packages){
  library(p, character.only=T)
}

participant_fin <- read_rds("data/rds/participant_fin.rds")


selectInput("partgroup",
                         "Select Grouping",
                         c("Education Level" = "educationLevel","Age Group" = "agegroup")
                         ),
               textOutput("selectedGroup1")
ui <- fluidPage(
  title = "Introduction",
  sidebarPanel(width =3,
               
               ),
  mainPanel(width = 9,
            plotOutput("areaplot1",
                        height = "500px")
            )
  )
  

server <- function(input, output){
  
  
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
    
    #View(data_for_plot)
    
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
  
  output$selectedGroup1 <- renderText({
    paste("You chose",input$partgroup)
  })

}

shinyApp(ui = ui, server = server)
