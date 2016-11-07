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
      
      plotlyOutput("life_expectancy"),
      plotlyOutput("life_expectancy_smoothed"),
      plotlyOutput("sum_missing"))
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

  output$life_expectancy_smoothed <- renderPlotly({
    p1 <- ggplotly(ggplot(subset(BlackWhiteSmooth, State2 == input$state), aes(x = Year3, y = le.smoothed.black)) + 
                     geom_ribbon(aes(ymin = le.smoothed.black, ymax = le.smoothed.white), alpha = 0.3) +
                     geom_line(aes(y = le.smoothed.white, col = Sex2), lty = 1) +
                     geom_line(aes(col = Sex2), lty = 2) + 
                     scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
                     #c("#67a9cf", "#ef8a62")
                     facet_wrap( ~ Sex2) +
                     scale_x_continuous(name = "Year") + scale_y_continuous(name = "SMOOTHED Life expectancy at birth (years)") + 
                     theme_minimal() + theme(legend.title=element_blank()))
    
    p1
    
    p2 <- ggplotly(ggplot(subset(BlackWhiteSmooth, State2 == input$state), aes(x = Year3, y = WBgap)) + geom_line(col = "grey") +
                     facet_wrap(~Sex2) + scale_x_continuous(name = "Year") + scale_y_continuous(name = "Life expectancy gap (years)") + 
                     theme_minimal() + theme(legend.title=element_blank()))
    subplot(p1, p2, shareX = T, nrows = 2, titleY = T)
  })  
  
  output$sum_missing <- renderPlotly({
    temp.dat <- (dat.clean[dat.clean$State2 == input$state & dat.clean$Race2 == "Black" & dat.clean$Sex2 == "Male", c("Age2", "Population", "Year", "Count", "COD2")])
    
    sums <- temp.dat %>% 
      group_by(Year, Age2) %>% 
      summarize(sum.na = sum(is.na(Count)), sum.zero = sum(Count == 0, na.rm=T)) %>%
      arrange(Year, Age2)
    
    ggplotly(ggplot(dat = sums, aes(x = Year, y = sum.na)) + 
             geom_line(aes(col = "Sum NA")) + geom_line(aes(y = sum.zero, col = "Sum zeroes")) +
             facet_wrap(~Age2) +
      ylab("Number of missing death counts") +
      ggtitle("Black Males"))
  })
}

shinyApp(ui = ui1, server = server)
