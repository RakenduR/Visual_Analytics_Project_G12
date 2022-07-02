packages <- c('shiny','tidyverse','lubridate','zoo','ggthemes','hrbrthemes','ggdist','gghalves',
              'ggridges','patchwork','zoo', 'ggrepel','ggiraph','gganimate','scales')

for (p in packages){
  library(p, character.only=T)
}

dataset_1 <- read_rds("data/rds/dataset_1.rds")
dataset_2 <- read_rds("data/rds/dataset_2.rds")
dataset_4 <- read_rds("data/rds/dataset_4.rds")

edutitle <- c("All",as.character(unique(dataset_1$educationLevel)))
educode <- c("All",as.character(unique(dataset_1$educationLevel)))
#eduChoices <- setNames((as.character(educode)),edutitle)
names(educode) <- edutitle

agetitle <- c("All",as.character(unique(dataset_1$agegroup)))

agecode <- c("All",as.character(unique(dataset_1$agegroup)))

#ageChoices <- setNames((as.character(agecode)),agetitle)
names(agecode) <- agetitle




ui <- navbarPage(
  title = "Introduction",
  navbarMenu("Finance",
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
           sidebarPanel(width =3,
             radioButtons("period", 
                          label = "Choose Time Period",
                          choices = list("Hourly" = "hour_of_day",
                                         "Daily" = "day_of_month",
                                         "Weekly" = "week_day",
                                         "Monthly" = "MonthYear"), 
                          selected = "day_of_month"),
           ),
           mainPanel(width = 9,
                     plotOutput("plot2",
                                height = "500px")
           )

),
tabPanel("Expense Calendar",
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
         
)
)
)

server <- function(input, output){
  
  
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

}

shinyApp(ui = ui, server = server)
