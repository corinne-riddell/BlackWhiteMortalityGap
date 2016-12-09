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
      radioButtons(inputId = "LE_type", label = "Use data from", inline = T, 
                   choices = c("le.birth.black", "le.birth.black5", "le.birth.black9"), 
                   selected = "le.birth.black5"),
      radioButtons(inputId = "LE_type2", label = "Use data from", inline = T, 
                   choices = c("le.birth.white", "le.birth.white5", "le.birth.white9"), 
                   selected = "le.birth.white5"),
      textOutput("text1"),
      plotlyOutput("life_expectancy")
    )
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
    p %>% layout(showlegend = F)
    
  })
  
  gap.react <- reactive({
    switch(input$LE_type,
           "le.birth.black" = BlackWhite[["WBgap"]],
           "le.birth.black5" = BlackWhite[["WBgap5"]],
           "le.birth.black9" = BlackWhite[["WBgap9"]])
  })
  
  dat.react <- reactive({
    data.frame(State2 = BlackWhite[["State2"]], Year3 = BlackWhite[["Year3"]],Sex2 = BlackWhite[["Sex2"]], 
               y1 = BlackWhite[[input$LE_type]], y2 = BlackWhite[[input$LE_type2]], WBgap = gap.react())
  })
  
  output$life_expectancy <- renderPlotly({
    
    p1 <- ggplotly( ggplot(subset(dat.react(), State2 == input$state), aes(x = Year3, y = y1)) + 
                    geom_ribbon(aes(ymin = y1, ymax = y2), alpha = 0.3) +
                    geom_line(aes(y = y2, col = Sex2), lty = 1) + 
                    geom_line(aes(col = Sex2), lty = 2) + 
                    scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
                    facet_wrap( ~ Sex2) +
                    scale_x_continuous(name = "Year") + scale_y_continuous(name = "Life expectancy at birth (years)") + 
                    theme_minimal() + theme(legend.title=element_blank())
    )
    
    p1
    
    p2 <- ggplotly(ggplot(subset(dat.react(), State2 == input$state), aes(x = Year3, y = WBgap)) + geom_line(col = "grey") +
                     facet_wrap(~Sex2) + scale_x_continuous(name = "Year") + scale_y_continuous(name = "Life expectancy gap (years)") + 
                     theme_minimal() + theme(legend.title=element_blank()))
    subplot(p1, p2, shareX = T, nrows = 2, titleY = T)
  })
  
}

shinyApp(ui = ui1, server = server)