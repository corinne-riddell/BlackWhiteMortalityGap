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
                 selectInput(inputId = "year1", label = "Select first comparison year", choices = unique(dat.clean$Year3)),
                 selectInput(inputId = "year2", label = "Select second comparison year", choices = unique(dat.clean$Year3)),
                 #*Output() functions,
                 #plotlyOutput("heatmapCOD"),
                 #plotlyOutput("heatmapsCOD6")
                 plotlyOutput("population_trend"),
#add the new output object for our LE graphs -- see my notes.
                 plotlyOutput("life_expectancy"),
                 plotOutput("age_comp1"),
                 plotOutput("age_comp2")
                 )

server <- function(input, output) {
  
  output$population_trend <- renderPlotly({
      ggplotly(ggplot(subset(dat2, State2 == input$state & Age3 == 0), aes(y = Pop_across_age, x = Year3)) + 
      geom_line(aes(col = Sex2, lty = Race2)) +
      scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
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
                  
                     geom_line(aes(y = le.birth.white, col = Sex2), lty = 1) +
                     geom_line(aes(col = Sex2), lty = 2) + 
                     scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
                     #c("#67a9cf", "#ef8a62")
                     facet_wrap( ~ Sex2) +
                     scale_x_continuous(name = "Year") + scale_y_continuous(name = "Life expectancy at birth (years)") + 
                     theme_minimal() + theme(legend.title=element_blank()))
    
    p2 <- ggplotly(ggplot(subset(BlackWhite, State2 == input$state), aes(x = Year3, y = WBgap)) + geom_line(col = "grey") +
               facet_wrap(~Sex2) + scale_x_continuous(name = "Year") + scale_y_continuous(name = "Life expectancy gap (years)") + 
               theme_minimal() + theme(legend.title=element_blank()))
    subplot(p1, p2, shareX = T, nrows = 2, titleY = T)
  })
  
  output$age_comp1 <- renderPlot({
    ggplot(as.data.frame(list.age.decomp.tables[[which(paired.ids$State2 ==  input$state & 
                                                         paired.ids$Year3 == input$year1 & 
                                                         paired.ids$Sex2 == "Male")]]), 
           aes(y = Ages, x = C_x)) + 
      geom_segment(aes(xend = 0, yend = Ages, col = adds_to_gap), lwd=8) +
      scale_color_manual(values = c("#d1e5f0", "#2166ac")) +  
      theme_minimal() +
      scale_y_reverse() + 
      geom_segment(x = 0, xend = 0, y = 10, yend = -90) +
      xlab("Loss or gain in life expectancy in years") +
      ggtitle(paste0("Contribution of age grouping to the black-white life expectancy gap for males in ", input$state, " in ", input$year1))
  })

  output$age_comp2 <- renderPlot({
  ggplot(as.data.frame(list.age.decomp.tables[[which(paired.ids$State2 ==  input$state & 
                                                       paired.ids$Year3 == input$year2 & 
                                                       paired.ids$Sex2 == "Male")]]), 
         aes(y = Ages, x = C_x)) + 
    geom_segment(aes(xend = 0, yend = Ages, col = adds_to_gap), lwd=8) +
    scale_color_manual(values = c("#d1e5f0", "#2166ac")) +  
    theme_minimal() +
    scale_y_reverse() + 
    geom_segment(x = 0, xend = 0, y = 10, yend = -90) +   
    xlab("Loss or gain in life expectancy in years") +
    ggtitle(paste0("Contribution of age grouping to the black-white life expectancy gap for males in ", input$state, " in ", input$year2))
      
})     
    }

shinyApp(ui = ui1, server = server)
