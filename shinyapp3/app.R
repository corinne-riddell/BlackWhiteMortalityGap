library(shiny)
library(plotly)
library(viridis)
library(scales)
library(shinythemes)
load(file = "/Users/corinneriddell/Dropbox/BlackWhiteGap/Data/main_datasets.Rdata")
source("/Users/corinneriddell/Documents/repos/BlackWhiteMortalityGap/Code/life_expectancy_functions.R")

ui1 <- fluidPage(theme = shinytheme("cosmo"),
                 #shinythemes::themeSelector(), 
                 titlePanel("Explore the black-white life expectancy gap in the United States"),
                 
                 sidebarLayout(
                   
                   sidebarPanel(width = 4,
                     selectInput(inputId = "state", label = "Select a state", width = 200, choices = unique(levels(dat.clean$State2))),
                     strong("Legend\n"),
                     img(src="legend.png"),
                     strong("Population size over time"),
                     plotlyOutput("population_trend")
                      ),
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Life Expectancy Gap",
                                radioButtons(inputId = "LE_type", label = "Model choice", inline = T, 
                                             choices = c("Impute 1", "Impute 5", "Impute 9"), 
                                             selected = "Impute 5"),
                                plotlyOutput("life_expectancy")),
                       
                       tabPanel("Decomposition by Age",
                                strong("Select sex"),
                                radioButtons(inputId = "selected_sex", label = NA, 
                                             inline = T, choices = c("Male", "Female"), selected = "Male"),
                                strong("Select years"),
                                tags$style(type="text/css", ".form-group.shiny-input-container{ display: inline-block } strong{ display: block !important  } img{ margin-bottom: 0px }"),
                                
                                selectInput(inputId = "year1", label = NA, choices = unique(dat.clean$Year3), width = 100),
                                selectInput(inputId = "year2", label = NA, choices = unique(dat.clean$Year3), selected = 2013, width = 100),
                                strong("Display contribution in"),
                                radioButtons(inputId = "contribution_type", label = NA, 
                                             inline = T, choices = c("Years", "Proportion (%)"), selected = "Years"),
                                plotlyOutput("age_bayes"),
                                plotlyOutput("age_bayes2"),
                                plotlyOutput("age_bayes_change")),
                       
                       tabPanel("Decomposition by Cause",
                                plotlyOutput("cod_bayes"),
                                plotlyOutput("cod_bayes2")),
                                #textOutput("temp2"),
                                #dataTableOutput("temp"),
                                #dataTableOutput("temp3")),
                       
                       tabPanel("Decomposition by Age & Cause",
                                plotlyOutput("age_cod_bayes"),
                                plotlyOutput("age_cod_bayes2"))
                       )
                     )
                   )
                 )

server <- function(input, output) {

  ##########################################
  ##              Sidebar                 ##
  ##########################################
  
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

  ##########################################
  ##          Life Expectancy tab         ##
  ##########################################
  
  gap.react <- reactive({
    switch(input$LE_type,
        #   "Bayesian Smoothing" = BlackWhite[["WBgap.s"]],
           "Impute 1" = BlackWhite[["WBgap"]],
           "Impute 5" = BlackWhite[["WBgap5"]],
           "Impute 9" = BlackWhite[["WBgap9"]])
  })
  
  white.react <- reactive({
    switch(input$LE_type,
       #    "Bayesian Smoothing" = BlackWhite[["le.smoothed.white"]],
           "Impute 1" = BlackWhite[["le.birth.white"]],
           "Impute 5" = BlackWhite[["le.birth.white5"]],
           "Impute 9" = BlackWhite[["le.birth.white9"]])
  })
  
  black.react <- reactive({
    switch(input$LE_type,
       #  "Bayesian Smoothing" = BlackWhite[["le.smoothed.black"]],
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
  
  ##########################################
  ##              Age tab                 ##
  ##########################################
  
  xaxis.react <- reactive({ 
    switch(input$contribution_type,
           "Years" = list.age.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                              paired.ids$Year3 == input$year1 & 
                                                              paired.ids$Sex2 == input$selected_sex)]][["C_x"]],
           "Proportion (%)" = list.age.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                       paired.ids$Year3 == input$year1 & 
                                                                       paired.ids$Sex2 == input$selected_sex)]][["C_x_proportion"]])
  })

  xaxis.react2 <- reactive({ 
    switch(input$contribution_type,
           "Years" = list.age.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                              paired.ids$Year3 == input$year2 & 
                                                              paired.ids$Sex2 == input$selected_sex)]][["C_x"]],
           "Proportion (%)" = list.age.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                       paired.ids$Year3 == input$year2 & 
                                                                       paired.ids$Sex2 == input$selected_sex)]][["C_x_proportion"]])
  })
  
  decomp.data.react <- reactive({
    data.frame(Ages = list.age.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                               paired.ids$Year3 == input$year1 & 
                                                               paired.ids$Sex2 == input$selected_sex)]][["Ages"]],
               adds_to_gap = list.age.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                      paired.ids$Year3 == input$year1 & 
                                                                      paired.ids$Sex2 == input$selected_sex)]][["adds_to_gap"]],
               x1 = xaxis.react())
  })
  
  decomp.data.react2 <- reactive({
    data.frame(Ages = list.age.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                               paired.ids$Year3 == input$year2 & 
                                                               paired.ids$Sex2 == input$selected_sex)]][["Ages"]],
               adds_to_gap = list.age.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                      paired.ids$Year3 == input$year2 & 
                                                                      paired.ids$Sex2 == input$selected_sex)]][["adds_to_gap"]],
               x1 = xaxis.react2())
  })

  contribution.data.react <- reactive({
                             temp <- contribution.to.gap.change(type.of.decomp = "Age", 
                              list.age.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                      paired.ids$Year3 == input$year1 & 
                                                                      paired.ids$Sex2 == input$selected_sex)]],
                              list.age.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                      paired.ids$Year3 == input$year2 & 
                                                                      paired.ids$Sex2 == input$selected_sex)]])
                             
                             temp[["change.x"]] <- switch(input$contribution_type,
                                                          "Years" = temp[["Contribution.to.change"]],
                                                          "Proportion (%)" = temp[["Contrib.to.change.prop"]]
                             )
                             
                             temp
  })
  

  xlim.upper <- reactive({ max(max(decomp.data.react()$x1), max(decomp.data.react2()$x1)) })
  xlim.lower <- reactive({ min(min(decomp.data.react()$x1), min(decomp.data.react2()$x1)) })  
  
  xaxis.title <- reactive({ 
    switch(input$contribution_type,
           "Years" = "(years)",
           "Proportion (%)" = "(%)")
    })
  
  output$age_bayes <- renderPlotly({
    
    p3 <- ggplotly(ggplot(decomp.data.react(), aes(y = Ages, x = x1)) + 
               geom_segment(aes(xend = 0, yend = Ages), lwd = 4, col = "#2166ac") + #, col = adds_to_gap
               #scale_color_manual(values = c("#d1e5f0", "#2166ac")) +  
               theme_minimal() +
               geom_vline(xintercept = 0) +
               #scale_y_reverse() + 
               xlab(paste0("Contribution to life expectancy gap ", xaxis.title())) +
               ggtitle(paste0(input$selected_sex, "s in ", input$state, " in ", input$year1)) +
               xlim(xlim.lower(), xlim.upper())
    )
    
    p3
    })

  output$age_bayes2 <- renderPlotly({
    p4 <- ggplotly(ggplot(decomp.data.react2(), aes(y = Ages, x = x1)) + 
                     geom_segment(aes(xend = 0, yend = Ages), lwd = 4, col = "#2166ac") + #, col = adds_to_gap
                     #scale_color_manual(values = c("#d1e5f0", "#2166ac")) +  
                     theme_minimal() +
                     geom_vline(xintercept = 0) +
                     #scale_y_reverse() + 
                     xlab(paste0("Contribution to life expectancy gap ", xaxis.title())) +
                     ggtitle(paste0(input$selected_sex, "s in ", input$state, " in ", input$year2)) +
                     xlim(xlim.lower(), xlim.upper())
    )
    p4
 })
  
  output$age_bayes_change <- renderPlotly({
    ggplotly(ggplot(contribution.data.react(), aes(y = Ages, x = change.x)) +
               geom_segment(aes(xend = 0, yend = Ages), lwd = 4, col = "#2166ac") + #, col = adds_to_gap
               #scale_color_manual(values = c("#d1e5f0", "#2166ac")) +  
               theme_minimal() +
               geom_vline(xintercept = 0) +
               #scale_y_reverse() + 
               xlab(paste0("Contribution to change in life expectancy gap", xaxis.title())) +
               ggtitle(paste0(input$selected_sex, "s in ", input$state, " in ", input$year2)) +
               xlim(xlim.lower(), xlim.upper())
    )
    
  })
  
  ##########################################
  ##         Cause of death tab           ##
  ##########################################
  
  xaxis.react3 <- reactive({ 
    switch(input$contribution_type,
           "Years" = list.cod.marginal.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                              paired.ids$Year3 == input$year1 & 
                                                              paired.ids$Sex2 == input$selected_sex)]][["C_x_COD"]],
           "Proportion (%)" = list.cod.marginal.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                       paired.ids$Year3 == input$year1 & 
                                                                       paired.ids$Sex2 == input$selected_sex)]][["C_x_COD_proportion"]])
  })
  
  xaxis.react4 <- reactive({ 
    switch(input$contribution_type,
           "Years" = list.cod.marginal.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                              paired.ids$Year3 == input$year2 & 
                                                              paired.ids$Sex2 == input$selected_sex)]][["C_x_COD"]],
           "Proportion (%)" = list.cod.marginal.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                       paired.ids$Year3 == input$year2 & 
                                                                       paired.ids$Sex2 == input$selected_sex)]][["C_x_COD_proportion"]])
  })
  
  cod.decomp.data.react <- reactive({
    data.frame(COD = list.cod.marginal.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                               paired.ids$Year3 == input$year1 & 
                                                               paired.ids$Sex2 == input$selected_sex)]][["Cause.of.death"]],
               adds_to_gap = list.cod.marginal.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                      paired.ids$Year3 == input$year1 & 
                                                                      paired.ids$Sex2 == input$selected_sex)]][["adds_to_gap"]],
               x1 = xaxis.react3())
  })
  
  cod.decomp.data.react2 <- reactive({
    data.frame(COD = list.cod.marginal.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                               paired.ids$Year3 == input$year2 & 
                                                               paired.ids$Sex2 == input$selected_sex)]][["Cause.of.death"]],
               adds_to_gap = list.cod.marginal.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                      paired.ids$Year3 == input$year2 & 
                                                                      paired.ids$Sex2 == input$selected_sex)]][["adds_to_gap"]],
               x1 = xaxis.react4())
  })
  
  xlim.upper2 <- reactive({ max(max(cod.decomp.data.react()$x1), max(cod.decomp.data.react2()$x1)) })
  xlim.lower2 <- reactive({ ifelse(min(min(cod.decomp.data.react()$x1), min(cod.decomp.data.react2()$x1)) >= 0, 0,
                                   min(min(cod.decomp.data.react()$x1), min(cod.decomp.data.react2()$x1))) 
    })  
  
   
  output$cod_bayes <- renderPlotly({  
    ggplotly(ggplot(cod.decomp.data.react(), aes(y = COD, x = x1)) + 
               geom_segment(aes(xend = 0, yend = COD, col = COD), lwd = 4) + 
               theme_minimal() +
               geom_vline(xintercept = 0) +
               xlab(paste0("Contribution to life expectancy gap ", xaxis.title())) +
               ylab("Cause of death") +
               ggtitle(paste0(input$selected_sex, "s in ", input$state, " in ", input$year1)) #+
               #xlim(xlim.lower2(), xlim.upper2())
             ) %>% layout(showlegend = F, xaxis = list(range = c(xlim.lower2(), xlim.upper2())), yaxis = list(autorange = "reversed"))
    
  })   
  
  output$cod_bayes2 <- renderPlotly({  
    ggplotly(ggplot(cod.decomp.data.react2(), aes(y = COD, x = x1)) + 
               geom_segment(aes(xend = 0, yend = COD, col = COD), lwd = 4) + 
               theme_minimal() +
               geom_vline(xintercept = 0) +
               xlab(paste0("Contribution to life expectancy gap ", xaxis.title())) +
               ylab("Cause of death") +
               ggtitle(paste0(input$selected_sex, "s in ", input$state, " in ", input$year2)) #+
               #xlim(xlim.lower2(), xlim.upper2())
             ) %>% layout(showlegend = F, xaxis = list(range = c(xlim.lower2(), xlim.upper2())), yaxis = list(autorange = "reversed"))
    
  })   
  
  #output$temp2 <- renderText(paste0("The lower limit is ", xlim.lower2(), " and the upper is ", xlim.upper2()))
  #output$temp <- renderDataTable(cod.decomp.data.react())
  #output$temp3 <- renderDataTable(cod.decomp.data.react2())

  ##########################################
  ##    Age and cause of death tab        ##
  ##########################################
  
  age.cod.data.react <- reactive({ 
    switch(input$contribution_type,
           "Years" = data.frame(start = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                 paired.ids$Year3 == input$year1 & 
                                                                                 paired.ids$Sex2 == input$selected_sex)]][["start"]],
                                finish = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                  paired.ids$Year3 == input$year1 & 
                                                                                  paired.ids$Sex2 == input$selected_sex)]][["finish"]],
                                Ages = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                paired.ids$Year3 == input$year1 & 
                                                                                paired.ids$Sex2 == input$selected_sex)]][["Ages"]],
                                COD = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                               paired.ids$Year3 == input$year1 & 
                                                                               paired.ids$Sex2 == input$selected_sex)]][["Cause.of.death"]]),
           
           "Proportion (%)" = data.frame(start = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                          paired.ids$Year3 == input$year1 & 
                                                                                          paired.ids$Sex2 == input$selected_sex)]][["start2"]],
                                         finish = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                           paired.ids$Year3 == input$year1 & 
                                                                                           paired.ids$Sex2 == input$selected_sex)]][["finish2"]],
                                         Ages = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                         paired.ids$Year3 == input$year2 & 
                                                                                         paired.ids$Sex2 == input$selected_sex)]][["Ages"]],
                                         COD = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                        paired.ids$Year3 == input$year2 & 
                                                                                        paired.ids$Sex2 == input$selected_sex)]][["Cause.of.death"]])
    )
  })

  age.cod.data.react2 <- reactive({ 
    switch(input$contribution_type,
           "Years" = data.frame(start = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                              paired.ids$Year3 == input$year2 & 
                                                              paired.ids$Sex2 == input$selected_sex)]][["start"]],
                                finish = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                  paired.ids$Year3 == input$year2 & 
                                                                                  paired.ids$Sex2 == input$selected_sex)]][["finish"]],
                                Ages = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                  paired.ids$Year3 == input$year2 & 
                                                                                  paired.ids$Sex2 == input$selected_sex)]][["Ages"]],
                                COD = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                  paired.ids$Year3 == input$year2 & 
                                                                                  paired.ids$Sex2 == input$selected_sex)]][["Cause.of.death"]]),
           
           "Proportion (%)" = data.frame(start = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                          paired.ids$Year3 == input$year2 & 
                                                                                          paired.ids$Sex2 == input$selected_sex)]][["start2"]],
                                         finish = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                           paired.ids$Year3 == input$year2 & 
                                                                                           paired.ids$Sex2 == input$selected_sex)]][["finish2"]],
                                         Ages = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                         paired.ids$Year3 == input$year2 & 
                                                                                         paired.ids$Sex2 == input$selected_sex)]][["Ages"]],
                                         COD = list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                        paired.ids$Year3 == input$year2 & 
                                                                                        paired.ids$Sex2 == input$selected_sex)]][["Cause.of.death"]])
    )
    
  })
  
  output$age_cod_bayes <- renderPlotly({
    
    ggplotly(ggplot(age.cod.data.react(),
                    aes(y = Ages, x = start)) + 
               geom_segment(aes(xend = finish, col = COD, yend = Ages), lwd = 4) +
               xlab(paste0("Contribution to life expectancy gap ", xaxis.title())) +
               ylab("Age group\n") + theme_minimal() +
               geom_vline(xintercept = 0) + 
               xlim(xlim.lower(), xlim.upper()) +
               ggtitle(paste0(input$selected_sex, "s in ", input$state, " in ", input$year1)) 
             ) 
  })  
  
  output$age_cod_bayes2 <- renderPlotly({
    
    ggplotly(ggplot(age.cod.data.react2(),
                    aes(y = Ages, x = start)) + 
               geom_segment(aes(xend = finish, col = COD, yend = Ages), lwd = 4) +
               xlab(paste0("Contribution to life expectancy gap ", xaxis.title())) +
               ylab("Age group\n") + theme_minimal() +
               geom_vline(xintercept = 0)  +
               xlim(xlim.lower(), xlim.upper()) +
               ggtitle(paste0(input$selected_sex, "s in ", input$state, " in ", input$year2)) 
             )
  })  
}

shinyApp(ui = ui1, server = server)
