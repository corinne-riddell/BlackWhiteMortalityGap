library(shiny)
library(plotly)
library(viridis)
library(scales)
library(shinythemes)
load(file = "/Users/corinneriddell/Dropbox/BlackWhiteGap/Data/main_datasets.Rdata")

ui1 <- fluidPage(#theme = shinytheme("cosmo"),
                 #shinythemes::themeSelector(), 
                 titlePanel("Explore the black-white life expectancy gap in the United States"),
                 
                 sidebarLayout(
                   sidebarPanel(width = 4,
                     selectInput(inputId = "state", label = "Select a state", width = 200, choices = unique(levels(dat.clean$State2))),
                     strong("Select years to compare"),
                     tags$style(type="text/css", ".form-group.shiny-input-container{ display: inline-block } strong{ display: block !important  } img{ margin-bottom: 30px }"),
                  
                     selectInput(inputId = "year1", label = NA, choices = unique(dat.clean$Year3), width = 100),
                     selectInput(inputId = "year2", label = NA, choices = unique(dat.clean$Year3), selected = 2013, width = 100),
                     strong("Legend\n"),
                     img(src="legend.png"),
                     strong("Population size over time"),
                     plotlyOutput("population_trend")
                      ),
                   mainPanel(
                     
                     #add the new output object for our LE graphs -- see my notes.
                     plotlyOutput("life_expectancy"),
                     plotlyOutput("male_age_comp1"),
                     plotlyOutput("male_age_comp2"),
                     plotOutput("female_age_comp1"),
                     plotOutput("female_age_comp2")                   )
                 )

                 )

server <- function(input, output) {

  output$population_trend <- renderPlotly({
    p <- ggplotly(
      ggplot(subset(dat2, State2 == input$state & Age3 == 0), aes(y = Pop_across_age, x = Year3)) + 
      geom_line(aes(col = Sex2, lty = Race2)) +
      scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
      ylab("Population Size") + 
      scale_y_continuous(label = comma) + 
      scale_x_continuous(name = "Year") + 
      theme_minimal() + theme(legend.title=element_blank())) 
      # p %>% layout(legend = list(x = 0.5, y = -2))
     #p %>% layout(showlegend = F, yaxis = list(exponentformat("SI")))
    p %>% layout(showlegend = F)
   
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
    
    p1
    
    p2 <- ggplotly(ggplot(subset(BlackWhite, State2 == input$state), aes(x = Year3, y = WBgap)) + geom_line(col = "grey") +
               facet_wrap(~Sex2) + scale_x_continuous(name = "Year") + scale_y_continuous(name = "Life expectancy gap (years)") + 
               theme_minimal() + theme(legend.title=element_blank()))
    subplot(p1, p2, shareX = T, nrows = 2, titleY = T)
  })
  
  output$male_age_comp1 <- renderPlotly({
    ggplotly(ggplot(list.cod.decomp.tables[[which(paired.ids$State2 ==  input$state & 
                            paired.ids$Year3 == input$year1 & 
                            paired.ids$Sex2 == "Male")]],
                    aes(y = Ages, x = start)) + 
               geom_segment(aes(xend = finish, col = Cause.of.death, yend = Ages), lwd = 4) +
               xlab("Contribution to life expectancy gap (in years)") +
               ylab("Age group\n") + theme_minimal() +
               geom_vline(xintercept = 0))
    
#    p3 <- ggplotly(ggplot(as.data.frame(list.age.decomp.tables[[which(paired.ids$State2 ==  input$state & 
#                                                         paired.ids$Year3 == input$year1 & 
#                                                         paired.ids$Sex2 == "Male")]]), 
#           aes(y = Ages, x = C_x)) + 
 #     geom_segment(aes(xend = 0, yend = Ages, col = adds_to_gap), lwd=4) +
#      scale_color_manual(values = c("#d1e5f0", "#2166ac")) +  
#      theme_minimal() +
#      scale_y_reverse() + 
#      geom_segment(x = 0, xend = 0, y = 10, yend = -90) +
#      xlab("Loss or gain in life expectancy in years") +
#      ggtitle(paste0("Contribution of age grouping to the black-white life expectancy gap for males in ", input$state, " in ", input$year1))
#    )
#p3
       })

  output$male_age_comp2 <- renderPlotly({
    ggplotly(ggplot(list.cod.decomp.tables[[which(paired.ids$State2 ==  input$state & 
                            paired.ids$Year3 == input$year2 & 
                            paired.ids$Sex2 == "Male")]],
                    aes(y = Ages, x = start)) + 
               geom_segment(aes(xend = finish, col = Cause.of.death, yend = Ages), lwd = 4) +
               xlab("Contribution to life expectancy gap (in years)") +
               ylab("Age group\n") + theme_minimal() +
               geom_vline(xintercept = 0))
    
 # ggplot(as.data.frame(list.age.decomp.tables[[which(paired.ids$State2 ==  input$state & 
#                                                       paired.ids$Year3 == input$year2 & 
#                                                       paired.ids$Sex2 == "Male")]]), 
 #        aes(y = Ages, x = C_x)) + 
#    geom_segment(aes(xend = 0, yend = Ages, col = adds_to_gap), lwd=8) +
#    scale_color_manual(values = c("#d1e5f0", "#2166ac")) +  
#    theme_minimal() +
#    scale_y_reverse() + 
#    geom_segment(x = 0, xend = 0, y = 10, yend = -90) +   
#    xlab("Loss or gain in life expectancy in years") +
#    ggtitle(paste0("Contribution of age grouping to the black-white life expectancy gap for males in ", input$state, " in ", input$year2))
      
})  
  
  output$female_age_comp1 <- renderPlot({
    ggplot(as.data.frame(list.age.decomp.tables[[which(paired.ids$State2 ==  input$state & 
                                                         paired.ids$Year3 == input$year1 & 
                                                         paired.ids$Sex2 == "Female")]]), 
           aes(y = Ages, x = C_x)) + 
      geom_segment(aes(xend = 0, yend = Ages, col = adds_to_gap), lwd=8) +
      scale_color_manual(values = c("#fddbc7", "#b2182b")) +  
      theme_minimal() +
      scale_y_reverse() + 
      geom_segment(x = 0, xend = 0, y = 10, yend = -90) +
      xlab("Loss or gain in life expectancy in years") +
      ggtitle(paste0("Contribution of age grouping to the black-white life expectancy gap for males in ", input$state, " in ", input$year1))
  })
  
  output$female_age_comp2 <- renderPlot({
    ggplot(as.data.frame(list.age.decomp.tables[[which(paired.ids$State2 ==  input$state & 
                                                         paired.ids$Year3 == input$year2 & 
                                                         paired.ids$Sex2 == "Female")]]), 
           aes(y = Ages, x = C_x)) + 
      geom_segment(aes(xend = 0, yend = Ages, col = adds_to_gap), lwd=8) +
      scale_color_manual(values = c("#fddbc7", "#b2182b")) +  
      theme_minimal() +
      scale_y_reverse() + 
      geom_segment(x = 0, xend = 0, y = 10, yend = -90) +   
      xlab("Loss or gain in life expectancy in years") +
      ggtitle(paste0("Contribution of age grouping to the black-white life expectancy gap for males in ", input$state, " in ", input$year2))
    
  })  
    }

shinyApp(ui = ui1, server = server)
