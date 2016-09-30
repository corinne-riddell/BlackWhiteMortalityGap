library(shiny)
library(plotly)
library(viridis)
library(scales)
load(file = "/Users/corinneriddell/Dropbox/BlackWhiteGap/Data/main_datasets.Rdata")

ui1 <- fluidPage("Explore cause of death by age and year in the United States",
                 
                 # *Input() functions,
                 #selectInput(inputId = "sex", label = "Select a sex", choices = unique(levels(dat.clean.alabama$Sex2))),
                 #selectInput(inputId = "race", label = "Select a race", choices = unique(levels(dat.clean.alabama$Race2))),
                 #selectInput(inputId = "cod", label = "Select a cause of death", choices = unique(levels(dat.clean.alabama$COD2))),
                 selectInput(inputId = "state", label = "Select a state", choices = unique(levels(dat.clean$State2))),
                 #*Output() functions,
                 #plotlyOutput("heatmapCOD"),
                 #plotlyOutput("heatmapsCOD6")
                 plotlyOutput("population_trend"),
#add the new output object for our LE graphs -- see my notes.
                 plotlyOutput("life_expectancy")
#                 plotlyOutput("life_expectancy_gap")
                 )

server <- function(input, output) {
  
  output$population_trend <- renderPlotly({
      ggplotly(ggplot(subset(dat2, State2 == input$state & Age3 == 0), aes(y = Pop_across_age, x = Year3)) + 
      geom_line(aes(col = Sex2, lty = Race2)) +
      ylab("Population Size") + 
      scale_y_continuous(label = scales::comma) + 
      scale_x_continuous(name = "Year") + 
      theme_minimal() + theme(legend.title=element_blank()))
      
   
  })
  
  output$life_expectancy <- renderPlotly({
#    p1 <- ggplotly(ggplot(subset(dat2, State2 == input$state & Age3 == 0), aes(x = Year3, y = life.expectancy.birth)) + 
#      geom_line(aes(col = Sex2, lty = Race2)) + facet_wrap(~Sex2) +
#      scale_x_continuous(name = "Year") + scale_y_continuous(name = "Life expectancy at birth (years)") + 
#      theme_minimal() + theme(legend.title=element_blank()))

    p1 <- ggplotly(ggplot(subset(BlackWhite, State2 == input$state), aes(x = Year3, y = le.birth.black)) + 
                     geom_ribbon(aes(ymin = le.birth.black, ymax = le.birth.white), alpha = 0.3) +
                     geom_line(aes(col = Sex2, lty = "Black")) + 
                     geom_line(aes(y = le.birth.white, col = Sex2, lty = "White")) +
                     facet_wrap(~Sex2) +
                     scale_x_continuous(name = "Year") + scale_y_continuous(name = "Life expectancy at birth (years)") + 
                     theme_minimal() + theme(legend.title=element_blank()))
    
    p2 <- ggplotly(ggplot(subset(BlackWhite, State2 == input$state), aes(x = Year3, y = WBgap)) + geom_line(col = "grey") +
               facet_wrap(~Sex2) + scale_x_continuous(name = "Year") + scale_y_continuous(name = "Life expectancy gap (years)") + 
               theme_minimal() + theme(legend.title=element_blank()))
    subplot(p1, p2, shareX = T, nrows = 2, titleY = T)
  })
  
#  output$life_expectancy_gap <- renderPlotly({
#  ggplotly(ggplot(subset(BlackWhite, State2 == input$state), aes(x = Year3, y = WBgap)) + geom_line(aes(col = Sex2)) +
#      facet_wrap(~Sex2) + scale_x_continuous(name = "Year") + scale_y_continuous(name = "Life expectancy gap (years)") + 
#      theme_minimal() + theme(legend.title=element_blank()))
#})
    }

shinyApp(ui = ui1, server = server)
