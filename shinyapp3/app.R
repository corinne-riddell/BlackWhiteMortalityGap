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
                                plotlyOutput("life_expectancy"),
                                textOutput("Explain_LE_males"),
                                textOutput("Explain_LE_females")),
                       
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
                                plotlyOutput("cod_bayes2"),
                                plotlyOutput("cod_bayes_change")),
                                #textOutput("temp2"),
                                #dataTableOutput("temp"),
                                #dataTableOutput("temp3")),
                       
                       tabPanel("Decomposition by Age & Cause",
                                plotlyOutput("age_cod_bayes"),
                                plotlyOutput("age_cod_bayes2")),
                       
                       tabPanel("Summary across states",
                                radioButtons(inputId = "order_states", label = NA, 
                                             inline = T, choices = c("Gap in first year", "Gap in last year", "Change in gap"), 
                                             selected = "Gap in first year"),
                                plotlyOutput("state_cod_summary", height = 800),
                                dataTableOutput("data.temp"))
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
  ##            COD summary tab           ##
  ##########################################

  summary.react <- reactive({
    temp <- BlackWhite %>% 
      filter(Year3 %in% c(input$year1, input$year2), Sex2 == input$selected_sex) %>%
      select(State2, Year3, WBgap.s) %>%
      arrange(Year3)
    
    temp <- tidyr::spread(temp, Year3, WBgap.s) 
    names(temp)[2:3] <- c("first.gap","second.gap")  
    
    temp <- temp %>% mutate(gap.diff = first.gap - second.gap,
                              State.gdiff.order = reorder(State2, gap.diff),
                              State.g1.order = reorder(State2, first.gap),
                              State.g2.order = reorder(State2, second.gap),
                              State.gdiff.num = as.numeric(State.gdiff.order),
                              State.g1.num = as.numeric(State.g1.order),
                              State.g2.num = as.numeric(State.g2.order)
                              )
    
    temp
  })
  
  summary.cod.contrib.data.react <- reactive({
    
    temp.df <- data.frame(State2 = factor(), COD = factor(), Contribution.to.change = numeric(), Contrib.to.change.prop = numeric(), narrowed_gap = logical())
    for (state.i in levels(BlackWhite$State2)) {
      first.index = which(paired.ids$State2 ==  state.i & paired.ids$Year3 == input$year1 & paired.ids$Sex2 == input$selected_sex)
      second.index = which(paired.ids$State2 ==  state.i & paired.ids$Year3 == input$year2 & paired.ids$Sex2 == input$selected_sex)
      
      if(is.null(list.cod.marginal.tables.smoothed[[first.index]][1]) == FALSE & 
         is.null(list.cod.marginal.tables.smoothed[[second.index]][1]) == FALSE) {
        
        temp2 <- data.frame(State2 = state.i,
                            contribution.to.gap.change(type.of.decomp = "COD",
                                                       list.cod.marginal.tables.smoothed[[first.index]],
                                                       list.cod.marginal.tables.smoothed[[second.index]])
        )
        
        temp.df <- rbind(temp.df, temp2)
      }
      
    }
    
    temp.df <- make_dataset_cod_plot(cod.decomp.table = temp.df, age.groups = "State2", 
                                     cause.of.death = "COD", sign.var = "narrowed_gap", 
                                     decomp.var = "Contribution.to.change", decomp.var.prop = "Contrib.to.change.prop")
    
    temp.df <- merge(temp.df, summary.react(), by = "State2")
    
    temp.df <- temp.df %>% mutate(new.start = -start + first.gap, new.finish = -finish + first.gap)
    
     temp.df$order.states2 <- switch(input$order_states, 
                                    "Gap in first year" = temp.df$State.g1.order,
                                    "Gap in last year" = temp.df$State.g2.order,
                                    "Change in gap" = temp.df$State.gdiff.order)
    temp.df$order.states <- switch(input$order_states, 
                                   "Gap in first year" = temp.df$State.g1.num,
                                   "Gap in last year" = temp.df$State.g2.num,
                                   "Change in gap" = temp.df$State.gdiff.num)
    return(temp.df)
  })
  
  output$state_cod_summary <- renderPlotly({
    ggplotly(
      # ggplot(summary.react(), aes(y = State.g1.order, x = first.gap)) + 
      #          geom_segment(aes(yend = State.g1.order, xend = second.gap)) + #, arrow = arrow(angle = 30, ends = "last", length = unit(0.10, "inches")) 
      #          geom_point(aes(x = second.gap), shape = 108) + 
      #          theme_minimal()) %>% layout(xaxis = list(title = "Life expectancy gap (years)"), yaxis = list(title = NA, autorange = "reversed")
      #                                      )
      ggplot(summary.cod.contrib.data.react(), aes(y = order.states, x = new.start)) +
        geom_rect(aes(xmin = new.start, ymin = order.states - 0.45, ymax = order.states + 0.45, xmax = new.finish, fill = COD), color = "white") + #, arrow = arrow(angle = 30, ends = "last", length = unit(0.10, "inches"))
        geom_segment(aes(x = second.gap, xend= second.gap, y = order.states - 0.5, yend = order.states + 0.5), lty = 3) + #will need to change this - adds lots of points on top of each other
        #geom_rect(aes(xmin = second.gap, xmax = second.gap + 0.03, ymin = order.states - 0.2, ymax = order.states + 0.75), fill = "grey40", color = "grey40") + #will need to change this - adds lots of points on top of each other
        #geom_rect(aes(xmin = first.gap, xmax = first.gap + 0.03, ymin = order.states - 0.2, ymax = order.states + 0.75), fill = "black", color = "black") + #will need to change this - adds lots of points on top of each other
        geom_segment(aes(x = first.gap, xend= first.gap, y = order.states - 0.5, yend = order.states + 0.5)) + #will need to change this - adds lots of points on top of each other
        
               theme_minimal() + scale_y_continuous(breaks = 1:51, labels = levels(summary.cod.contrib.data.react()$order.states2)) )%>% 
      layout(xaxis = list(title = "Life expectancy gap (years)"), yaxis = list(title = NA, autorange = "reversed")
        )
    #NEED TO CHANGE THE BREAKS 1:51 TO REFLECT THE NUMBER OF STATES IN THE DISPLAY
    
      # ggplot(summary.cod.contrib.data.react(), aes(y = order.states, x = new.start)) + 
      #   geom_segment(aes(yend = order.states, xend = new.finish, col = COD), size = 4) + #, arrow = arrow(angle = 30, ends = "last", length = unit(0.10, "inches")) 
      #   geom_segment(aes(x = second.gap, xend = second.gap, yend = order.states + 1), col = "green") + #will need to change this - adds lots of points on top of each other
      #   geom_segment(aes(x = first.gap, xend = first.gap, yend = order.states + 1), col = "red") + #will need to change this - adds lots of points on top of each other
      #   theme_minimal()) %>% layout(xaxis = list(title = "Life expectancy gap (years)"), yaxis = list(title = NA, autorange = "reversed")
      #   )
    })
  
   output$data.temp <- renderDataTable({ summary.cod.contrib.data.react() })
  
  
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
  
  facet_names <- list(
    'Male'="Male Life Expectancy (years)",
    'Female'="Female Life Expectancy (years)"
  )
  
  output$life_expectancy <- renderPlotly({

    p1 <- ggplotly(ggplot(subset(dat.react(), State2 == input$state), aes(x = Year3, y = y1)) +
                    geom_ribbon(aes(ymin = y1, ymax = y2), alpha = 0.3) +
                    geom_line(aes(y = y2, col = Sex2), lty = 1) + 
                    geom_line(aes(col = Sex2), lty = 2) + 
                    geom_line(aes(y = le.smoothed.white), lty = 1, lwd = 0.5, alpha = 0.5) + 
                    geom_line(aes(y = le.smoothed.black), lty = 2, lwd = 0.5, alpha = 0.5) + 
                    scale_color_manual(values = c("#67a9cf", "#ef8a62", "black")) +
                    facet_grid(. ~ Sex2, labeller = as_labeller(facet_names)) +
                    #scale_x_continuous(name = "Year") + 
                     #scale_y_continuous(name = "Life expectancy at birth (years)") + 
                    theme_minimal() + theme(legend.title=element_blank()) 
                    ) %>% 
      layout(yaxis = list(title = "Life expectancy"))
    
    p1
    
    p2 <- ggplotly(ggplot(subset(dat.react(), State2 == input$state), aes(x = Year3, y = WBgap)) + geom_line(col = "grey") +
                     geom_line(aes(y = WBgap.s), col = "black", alpha = 0.5) + 
                     facet_grid(. ~ Sex2) + #scale_x_continuous(name = "Year") + 
                     #scale_y_continuous(name = "Life expectancy gap (years)") + 
                     theme_minimal() + theme(legend.title=element_blank(), strip.text.x = element_blank())) %>% 
      layout(yaxis = list(title = "Difference"), xaxis = list(title = "Year"))
    
    subplot(p1, p2, shareX = T, nrows = 2, titleY = T)
  })
  
  white.y1 <- reactive({
    round(BlackWhite %>% 
            filter(State2 == input$state,Year3 == input$year1, Sex2 == "Male") %>% 
            select(le.smoothed.white), 1)
  })
  
  white.y2 <- reactive({
    round(BlackWhite %>% 
            filter(State2 == input$state, Year3 == input$year2, Sex2 == "Male") %>% 
            select(le.smoothed.white), 1)
  })
  
  black.y1 <- reactive({
    round(BlackWhite %>% 
            filter(State2 == input$state,Year3 == input$year1, Sex2 == "Male") %>% 
            select(le.smoothed.black), 1)
  })
  
  black.y2 <- reactive({
    round(BlackWhite %>% 
            filter(State2 == input$state, Year3 == input$year2, Sex2 == "Male") %>% 
            select(le.smoothed.black), 1)
  }) 
  
  output$Explain_LE_males <- renderText({
    paste0("In ", input$state, " between ", input$year1,
           " and ", input$year2,", life expectancy at birth changed from ",  white.y1(), 
           " years to ", white.y2(), " years (for a change of ", round(white.y2()-white.y1(),1), 
           " years) for white males. For black males, the change was from ", black.y1()," years to ", black.y2(), 
           " years (", round(black.y2()-black.y1(),1),
           " years change). The black-white difference in male life expectancy changed by ",
           round((white.y2() - black.y2()) - (white.y1() - black.y1()), 1), " years: from ",
           round(white.y1() - black.y1(), 1), " years to ", round(white.y2() - black.y2(), 1) , " years.") 

    })

  fwhite.y1 <- reactive({
    round(BlackWhite %>% 
            filter(State2 == input$state,Year3 == input$year1, Sex2 == "Female") %>% 
            select(le.smoothed.white), 1)
  })
  
  fwhite.y2 <- reactive({
    round(BlackWhite %>% 
            filter(State2 == input$state, Year3 == input$year2, Sex2 == "Female") %>% 
            select(le.smoothed.white), 1)
  })
  
  fblack.y1 <- reactive({
    round(BlackWhite %>% 
            filter(State2 == input$state,Year3 == input$year1, Sex2 == "Female") %>% 
            select(le.smoothed.black), 1)
  })
  
  fblack.y2 <- reactive({
    round(BlackWhite %>% 
            filter(State2 == input$state, Year3 == input$year2, Sex2 == "Female") %>% 
            select(le.smoothed.black), 1)
  }) 
  
  output$Explain_LE_females <- renderText({
    paste0("For white females, life expectancy at birth changed from ",  fwhite.y1(), 
           " years to ", fwhite.y2(), " years (for a change of ", round(fwhite.y2()-fwhite.y1(),1), 
           " years). For black females, the change was from ", fblack.y1()," years to ", fblack.y2(), 
           " years (", round(fblack.y2()-fblack.y1(),1),
           " years change). The black-white difference in female life expectancy changed by ",
           round((fwhite.y2() - fblack.y2()) - (fwhite.y1() - fblack.y1()), 1), " years: from ",
           round(fwhite.y1() - fblack.y1(), 1), " years to ", round(fwhite.y2() - fblack.y2(), 1) , " years.") 
    
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
  
  cod.contribution.data.react <- reactive({
    temp2 <- contribution.to.gap.change(type.of.decomp = "COD", 
                                       list.cod.marginal.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                paired.ids$Year3 == input$year1 & 
                                                                                paired.ids$Sex2 == input$selected_sex)]],
                                       list.cod.marginal.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                                paired.ids$Year3 == input$year2 & 
                                                                                paired.ids$Sex2 == input$selected_sex)]])
    
    temp2[["change.x"]] <- switch(input$contribution_type,
                                 "Years" = temp2[["Contribution.to.change"]],
                                 "Proportion (%)" = temp2[["Contrib.to.change.prop"]]
    )
    
    temp2
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
  
  output$cod_bayes_change <- renderPlotly({
    ggplotly(ggplot(cod.contribution.data.react(), aes(y = COD, x = change.x)) +
               geom_segment(aes(xend = 0, yend = COD, col = COD), lwd = 4) + #, col = adds_to_gap
               #scale_color_manual(values = c("#d1e5f0", "#2166ac")) +  
               theme_minimal() +
               geom_vline(xintercept = 0) +
               #scale_y_reverse() + 
               xlab(paste0("Contribution to change in life expectancy gap", xaxis.title())) +
               ggtitle(paste0(input$selected_sex, "s in ", input$state, " in ", input$year2)) 
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
