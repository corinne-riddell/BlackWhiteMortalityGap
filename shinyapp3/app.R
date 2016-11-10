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
                     radioButtons(inputId = "LE_type", label = "Model choice", inline = T, 
                                  choices = c("Bayesian Smoothing", "Impute 1", "Impute 5", "Impute 9"), 
                                  selected = "Impute 5"),
                     plotlyOutput("life_expectancy"),
                     radioButtons(inputId = "contribution_type", label = "Display contribution in", inline = T, choices = c("Years", "Proportion (%)"), selected = "Years"),
                     plotlyOutput("male_age_comp1"),
                     plotlyOutput("male_age_comp2"),
                     plotlyOutput("female_age_comp1"),
                     plotlyOutput("female_age_comp2")                   
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
      # p %>% layout(legend = list(x = 0.5, y = -2))
     #p %>% layout(showlegend = F, yaxis = list(exponentformat("SI")))
    p %>% layout(showlegend = F)
   
  })
  

  
  gap.react <- reactive({
    switch(input$LE_type,
           "Bayesian Smoothing" = BlackWhite[["WBgap.s"]],
           "Impute 1" = BlackWhite[["WBgap"]],
           "Impute 5" = BlackWhite[["WBgap5"]],
           "Impute 9" = BlackWhite[["WBgap9"]])
  })
  
  white.react <- reactive({
    switch(input$LE_type,
           "Bayesian Smoothing" = BlackWhite[["le.smoothed.white"]],
           "Impute 1" = BlackWhite[["le.birth.white"]],
           "Impute 5" = BlackWhite[["le.birth.white5"]],
           "Impute 9" = BlackWhite[["le.birth.white9"]])
  })
  
  black.react <- reactive({
    switch(input$LE_type,
           "Bayesian Smoothing" = BlackWhite[["le.smoothed.black"]],
           "Impute 1" = BlackWhite[["le.birth.black"]],
           "Impute 5" = BlackWhite[["le.birth.black5"]],
           "Impute 9" = BlackWhite[["le.birth.black9"]])
  })

  
  dat.react <- reactive({
    data.frame(State2 = BlackWhite[["State2"]], 
               Year3 = BlackWhite[["Year3"]], 
               Sex2 = BlackWhite[["Sex2"]], 
               y1 = black.react(), 
               y2 = white.react(), 
               WBgap = gap.react(),
               le.smoothed.white = BlackWhite[["le.smoothed.white"]],
               le.smoothed.black = BlackWhite[["le.smoothed.black"]],
               WBgap.s = BlackWhite[["WBgap.s"]])
  })
  
  output$life_expectancy <- renderPlotly({

    p1 <- ggplotly(ggplot(subset(dat.react(), State2 == input$state), aes(x = Year3, y = y1)) +
                    geom_ribbon(aes(ymin = y1, ymax = y2), alpha = 0.3) +
                    geom_line(aes(y = y2, col = Sex2), lty = 1) + 
                    geom_line(aes(col = Sex2), lty = 2) + 
                    geom_line(aes(y = le.smoothed.white), lty = 1, lwd = 0.5, alpha = 0.5) + 
                    geom_line(aes(y = le.smoothed.black), lty = 2, lwd = 0.5, alpha = 0.5) + 
                    scale_color_manual(values = c("#67a9cf", "#ef8a62", "black")) +
                    facet_wrap( ~ Sex2) +
                    scale_x_continuous(name = "Year") + scale_y_continuous(name = "Life expectancy at birth (years)") + 
                    theme_minimal() + theme(legend.title=element_blank())
                    )
    
    p1
    
    p2 <- ggplotly(ggplot(subset(dat.react(), State2 == input$state), aes(x = Year3, y = WBgap)) + geom_line(col = "grey") +
                     geom_line(aes(y = WBgap.s), col = "black", alpha = 0.5) + 
                     facet_wrap(~ Sex2) + scale_x_continuous(name = "Year") + 
                     scale_y_continuous(name = "Life expectancy gap (years)") + 
                     theme_minimal() + theme(legend.title=element_blank()))
    
    subplot(p1, p2, shareX = T, nrows = 2, titleY = T)
  })
  
  
  decomp.react <- reactive({
    switch(input$LE_type,
           "Bayesian Smoothing" = list.cod.decomp.tables.smoothed,
           "Impute 1" = list.cod.decomp.tables,
           "Impute 5" = list.cod.decomp.tables5,
           "Impute 9" = list.cod.decomp.tables9)
  })
  
  output$male_age_comp1 <- renderPlotly({

            ggplotly(ggplot(decomp.react()[[which(paired.ids$State2 ==  input$state & 
                                paired.ids$Year3 == input$year1 & 
                                paired.ids$Sex2 == "Male")]],
                        aes(y = Ages, x = start)) + 
                   geom_segment(aes(xend = finish, col = Cause.of.death, yend = Ages), lwd = 4) +
                   xlab("Contribution to life expectancy gap (in years)") +
                   ylab("Age group\n") + theme_minimal() +
                   geom_vline(xintercept = 0))
  
       })

  output$male_age_comp2 <- renderPlotly({
    ggplotly(ggplot(decomp.react()[[which(paired.ids$State2 ==  input$state & 
                            paired.ids$Year3 == input$year2 & 
                            paired.ids$Sex2 == "Male")]],
                    aes(y = Ages, x = start)) + 
               geom_segment(aes(xend = finish, col = Cause.of.death, yend = Ages), lwd = 4) +
               xlab("Contribution to life expectancy gap (in years)") +
               ylab("Age group\n") + theme_minimal() +
               geom_vline(xintercept = 0))
})  
  
  output$female_age_comp1 <- renderPlotly({
    
    ggplotly(ggplot(decomp.react()[[which(paired.ids$State2 ==  input$state & 
                                            paired.ids$Year3 == input$year1 & 
                                            paired.ids$Sex2 == "Female")]],
                    aes(y = Ages, x = start)) + 
               geom_segment(aes(xend = finish, col = Cause.of.death, yend = Ages), lwd = 4) +
               xlab("Contribution to life expectancy gap (in years)") +
               ylab("Age group\n") + theme_minimal() +
               geom_vline(xintercept = 0))
    
  })
  
  output$female_age_comp2 <- renderPlotly({
    ggplotly(ggplot(decomp.react()[[which(paired.ids$State2 ==  input$state & 
                                                    paired.ids$Year3 == input$year2 & 
                                                    paired.ids$Sex2 == "Female")]],
                    aes(y = Ages, x = start)) + 
               geom_segment(aes(xend = finish, col = Cause.of.death, yend = Ages), lwd = 4) +
               xlab("Contribution to life expectancy gap (in years)") +
               ylab("Age group\n") + theme_minimal() +
               geom_vline(xintercept = 0))
  })
    
}
shinyApp(ui = ui1, server = server)
