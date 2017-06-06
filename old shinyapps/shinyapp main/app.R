library(shiny)
library(plotly)
library(viridis)
library(scales)
library(shinythemes)
library(dplyr)
load(file = "/Volumes/Untitled/Dropbox/blackwhitegap/Data/main_datasets.Rdata")
source("/Users/corinneriddell/Documents/repos/BlackWhiteMortalityGap/Code/life_expectancy_functions.R")

ui1 <- fluidPage(theme = shinytheme("cosmo"),
                 #shinythemes::themeSelector(), 
                 titlePanel("Explore the black-white life expectancy gap in the United States"),
                 
                 sidebarLayout(
                   sidebarPanel(width = 2,
                                radioButtons(inputId = "order_states", 
                                             label = "Sort by", 
                                             #inline = T, 
                                             choices = c("Gap in first year", "Gap in last year", "Change in gap"), 
                                             selected = "Gap in first year"),
                                strong("Select years"),
                                #tags$style(type="text/css", ".form-group.shiny-input-container{ display: inline-block } strong{ display: block !important  } img{ margin-bottom: 0px }"),
                                selectInput(inputId = "year1", label = NA, choices = unique(dat.clean$Year3), width = 100),
                                selectInput(inputId = "year2", label = NA, choices = unique(dat.clean$Year3), selected = 2013, width = 100),
                                strong("Select sex"),
                                radioButtons(inputId = "selected_sex", label = NA, 
                                             inline = T, choices = c("Male", "Female"), selected = "Male"),
                                selectInput(inputId = "state", label = "Select a state", width = 200, choices = unique(levels(dat.clean$State2))),
                                strong("Display contribution in"),
                                radioButtons(inputId = "contribution_type", label = NA, 
                                             inline = T, choices = c("Years", "Proportion (%)"), selected = "Years")

                      ),
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("State summary: COD",
                                strong("Which causes of death contributed most to narrowing the life expectancy gap?"),
                                textOutput("description_cod_summary"),
                                plotlyOutput("state_cod_summary", height = 800),
                                dataTableOutput("data.temp")),
                       
                       tabPanel("State summary: Age",
                                strong("Which age groups contributed most to narrowing the life expectancy gap?"),
                                textOutput("description_age_summary"),
                                plotlyOutput("state_age_summary", height = 800),
                                dataTableOutput("data.temp2")),
                       
                       tabPanel("Trends in COD",
                                strong("How has each cause of death contributed to the difference in life expectancy over time?
                                       First, here is a graph of the life expectancy gap over time, where each line is a state:"),
                                plotlyOutput("le_gap_all_states", height = 400), 
                                strong("Select the cause of death you're interested in to see how many years of the total gap
                                       is due the selected cause, and how this changes over time. You can also view the proportional
                                       contribution by selecting 'Proportion (%)' on the panel to the left."),
                                radioButtons(inputId = "COD", label = NA, inline = T,
                                             choices = levels(cod.contributions$Cause.of.death)),
                                plotlyOutput("contribution_all_states", height = 400)),
                       
                       tabPanel("Life Expectancy Gap",
                                radioButtons(inputId = "LE_type", label = "Model choice", inline = T, 
                                             choices = c("Impute 1", "Impute 5", "Impute 9"), 
                                             selected = "Impute 5"),
                                strong("Legend\n"),
                                img(src="legend.png"),
                                strong("Life expectancy over time"),
                                plotlyOutput("life_expectancy"),
                                textOutput("Explain_LE_males"),
                                textOutput("Explain_LE_females"),
                                strong("Population trends over time"),
                                plotlyOutput("population_trend", height = 300, width = 400)),
                       
                       tabPanel("Decomposition by Age",
                                plotlyOutput("age_bayes"),
                                textOutput("Explain_Age_Gap")),
                       
                       tabPanel("Decomposition by Cause",
                                plotlyOutput("cod_bayes"),
                                textOutput("Explain_COD_Gap")),
                       
                       tabPanel("Decomposition by Age & Cause",
                                plotlyOutput("age_cod_bayes"))
                    
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
    p %>% layout(showlegend = F)
   
  })

  ##########################################
  ##         COD and Age summary tabs     ##
  ##########################################

  summary.react <- reactive({
    temp <- BlackWhite %>% 
      filter(Year3 %in% c(input$year1, input$year2), Sex2 == input$selected_sex, is.na(WBgap.s) == F) %>%
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
    
    temp$order.states2 <- switch(input$order_states, 
                                    "Gap in first year" = temp$State.g1.order,
                                    "Gap in last year" = temp$State.g2.order,
                                    "Change in gap" = temp$State.gdiff.order)
    temp$order.states <- switch(input$order_states, 
                                   "Gap in first year" = temp$State.g1.num,
                                   "Gap in last year" = temp$State.g2.num,
                                   "Change in gap" = temp$State.gdiff.num)
    
    temp
  })
  
  summary.contrib.data.react <- reactive({
    
    temp.df <- data.frame(State2 = factor(), COD = factor(), Contribution.to.change = numeric(), Contrib.to.change.prop = numeric(), narrowed_gap = logical())
    temp.df2 <- data.frame(State2 = factor(), Ages = factor(), Contribution.to.change = numeric(), Contrib.to.change.prop = numeric(), narrowed_gap = logical())
    for (state.i in levels(BlackWhite$State2)) {
      first.index = which(paired.ids$State2 ==  state.i & paired.ids$Year3 == input$year1 & paired.ids$Sex2 == input$selected_sex)
      second.index = which(paired.ids$State2 ==  state.i & paired.ids$Year3 == input$year2 & paired.ids$Sex2 == input$selected_sex)
      
      if(is.null(list.cod.marginal.tables.smoothed[[first.index]][1]) == FALSE & 
         is.null(list.cod.marginal.tables.smoothed[[second.index]][1]) == FALSE) {
        
        temp2 <- data.frame(State2 = state.i,
                            contribution.to.gap.change(type.of.decomp = "COD",
                                                       list.cod.marginal.tables.smoothed[[first.index]],
                                                       list.cod.marginal.tables.smoothed[[second.index]],
                                                       type.of.data = "practice")
                            )
        
        temp3 <- data.frame(State2 = state.i,
                            contribution.to.gap.change(type.of.decomp = "Age",
                                                       list.age.decomp.tables.smoothed[[first.index]],
                                                       list.age.decomp.tables.smoothed[[second.index]],
                                                       type.of.data = "practice")
                            )
        
        temp.df <- rbind(temp.df, temp2)
        temp.df2 <- rbind(temp.df2, temp3)
      }
      
    }
    
    ##### COD ####
    # temp.df <- make_dataset_cod_plot(cod.decomp.table = temp.df, age.groups = "State2", 
    #                                  cause.of.death = "COD", sign.var = "narrowed_gap", 
    #                                  decomp.var = "Contribution.to.change", decomp.var.prop = "Contrib.to.change.prop")
    
    temp.df <- make_pretty_decomp_plot(decomp.table = temp.df, strat.var.1 = "State2", 
                                       partition.bar.var = "COD", sign.var = "narrowed_gap",
                                       decomp.var = "Contribution.to.change", decomp.var.prop = "Contrib.to.change.prop",
                                       type.of.data = "practice")
    
    temp.df <- merge(temp.df, summary.react(), by = "State2")
    
    temp.df <- temp.df %>% mutate(new.start = -start + first.gap, new.finish = -finish + first.gap)
    
    #### Age ####
    # temp.df2 <- make_dataset_cod_plot(cod.decomp.table = temp.df2, age.groups = "State2", 
    #                                  cause.of.death = "Ages", sign.var = "narrowed_gap", 
    #                                  decomp.var = "Contribution.to.change", decomp.var.prop = "Contrib.to.change.prop")
    
    temp.df2 <- make_pretty_decomp_plot(decomp.table = temp.df2, strat.var.1 = "State2", 
                                        partition.bar.var = "Ages", sign.var = "narrowed_gap", 
                                        decomp.var = "Contribution.to.change", decomp.var.prop = "Contrib.to.change.prop", 
                                        type.of.data = "practice")
    
    temp.df2 <- merge(temp.df2, summary.react(), by = "State2")
    
    temp.df2 <- temp.df2 %>% mutate(new.start = -start + first.gap, new.finish = -finish + first.gap)
  
    return(list(temp.df, temp.df2))
  })
  
  x.labels <- reactive({levels(factor(summary.contrib.data.react()[[2]]$order.states2)) })
  x.num.breaks <- reactive({length(levels(factor(summary.contrib.data.react()[[2]]$order.states2))) })

  output$state_cod_summary <- renderPlotly({
    ggplotly(
      ggplot(summary.contrib.data.react()[[1]], aes(y = order.states, x = new.start)) +
        geom_rect(aes(xmin = new.start, ymin = order.states - 0.45, ymax = order.states + 0.45, xmax = new.finish, fill = COD), color = "white") +
        geom_segment(data = summary.react(), aes(x = second.gap, xend= second.gap, y = order.states - 0.5, yend = order.states + 0.5)) +
        geom_segment(data = summary.react(), aes(x = first.gap, xend= first.gap, y = order.states - 0.5, yend = order.states + 0.5), lty = 3) +
               theme_minimal() + scale_y_continuous(breaks = 1:x.num.breaks(), labels = x.labels()) ) %>% 
      layout(xaxis = list(title = "Life expectancy gap (years)"), yaxis = list(title = NA, autorange = "reversed"))
  })  
    
  output$description_cod_summary <- renderText({
    paste0("This graphic depicts how the gap in life expectancy for ", input$selected_sex, "s changed between ", 
           input$year1, " (dashed vertical line) and ", input$year2, " (solid vertical line). Causes to the left of the dashed line narrowed the gap, whereas causes to the right exacerbated it.")
  })
  
    output$state_age_summary <- renderPlotly({
      ggplotly(                                     
        ggplot(summary.contrib.data.react()[[2]], aes(y = order.states, x = new.start)) +
          geom_rect(aes(xmin = new.start, ymin = order.states - 0.45, ymax = order.states + 0.45, xmax = new.finish, fill = Ages), color = "white") + 
          geom_segment(data = summary.react(), aes(x = second.gap, xend= second.gap, y = order.states - 0.5, yend = order.states + 0.5)) + 
          geom_segment(data = summary.react(), aes(x = first.gap, xend= first.gap, y = order.states - 0.5, yend = order.states + 0.5), lty = 3) + 
          theme_minimal() + scale_y_continuous(breaks = 1:x.num.breaks(), labels = x.labels()) + scale_fill_viridis(discrete = T, direction = -1) )%>% 
        layout(xaxis = list(title = "Life expectancy gap (years)"), yaxis = list(title = NA, autorange = "reversed")
        )
    })

    output$description_age_summary <- renderText({
      paste0("This graphic depicts how the gap in life expectancy for ", input$selected_sex, "s changed between ", 
             input$year1, " (dashed vertical line) and ", input$year2, " (solid vertical line). Age groups where 
mortality rates changed in a direction benefiting black life expectancy are shown to the level of the dashed line, 
whereas those that exacerbated the gap are shown to the right.")
    })
    
   #output$data.temp2 <- renderDataTable({ summary.contrib.data.react()[[2]] })
  
  
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
  ##            COD trends tab            ##
  ##########################################
  
  output$le_gap_all_states <- renderPlotly({
    ggplotly(ggplot(data = subset(BlackWhite, Sex2 == input$selected_sex), aes(y=WBgap.s, x = Year3)) + 
               geom_line(aes(col = State2)) +
               facet_grid(. ~ Census_Region) +
               ylab("Black-white life expectancy gap (years)") +
               xlab("Year") +
               geom_text(data = subset(BlackWhite, Year3 == 2013 & Sex2 == input$selected_sex), aes(label = stabbrs), check_overlap = T, size = 2.5) +
               theme_minimal() + theme(axis.text.x = element_text(angle = 45)) +
               geom_hline(yintercept = 0)
    ) 
  })
  
  contrib.data.react <- reactive({
    temp <- data.frame(subset(cod.contributions, Sex2 == input$selected_sex & Cause.of.death == input$COD))
    temp["y1"] <-  switch(input$contribution_type,
                          "Years" = temp[["C_x_COD"]],
                          "Proportion (%)" = temp[["C_x_COD_proportion"]]
    )
    temp
  })
  
  output$contribution_all_states <- renderPlotly({
    ggplotly(ggplot(contrib.data.react(), aes(x = Year3, y = y1)) + 
               geom_line(aes(col = State2)) + 
               facet_grid(. ~ Census_Region) +
               ylab(paste0("Contribution to LE Gap", xaxis.title())) +
               xlab("Year") +
               geom_text(data = subset(contrib.data.react(), Year3 == 2013 & Sex2 == input$selected_sex & Cause.of.death == input$COD), 
                          aes(label = stabbrs), check_overlap = T, size = 2.5) +
                theme_minimal() +  theme(axis.text.x = element_text(angle = 40)) +
                geom_hline(yintercept = 0))
    #  %>% add_annotations(x = contrib.data.react()$Year3, y = contrib.data.react()$y1, text = contrib.data.react()$stabbrs,
    #                       xref = "x",
    #                       yref = "y",
    #                       showarrow = TRUE,
    #                       arrowhead = 7,
    #                       ax = 20,
    #                       ay = -40)
  })
  
  ##########################################
  ##              Age tab                 ##
  ##########################################
  
  decomp.data.react <- reactive({
    temp <- data.frame(list.age.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                            paired.ids$Year3 == input$year1 & 
                                            paired.ids$Sex2 == input$selected_sex)]])
    temp["x1"] <-  switch(input$contribution_type,
                         "Years" = temp[["C_x"]],
                         "Proportion (%)" = temp[["C_x_proportion"]]
    )

    temp[["year"]] <- input$year1
    temp
  })
  
  decomp.data.react2 <- reactive({
    temp <- data.frame(list.age.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                paired.ids$Year3 == input$year2 & 
                                                                paired.ids$Sex2 == input$selected_sex)]])
    temp["x1"] <-  switch(input$contribution_type,
                          "Years" = temp[["C_x"]],
                          "Proportion (%)" = temp[["C_x_proportion"]]
    )
    temp[["year"]] <- input$year2
    temp
  })
  
  contribution.data.react <- reactive({
    temp <- contribution.to.gap.change(type.of.decomp = "Age", 
                                       decomp.data.react(),
                                       decomp.data.react2(), type.of.data = "practice")
    
    temp[["x1"]] <- switch(input$contribution_type,
                                 "Years" = temp[["Contribution.to.change"]],
                                 "Proportion (%)" = temp[["Contrib.to.change.prop"]]
                                 )
                             
    temp[["year"]] <- "Change"
    temp
  })
  
  decomp.data.react3 <- reactive({
    temp <- rbind(decomp.data.react()[ , c("Ages", "x1", "year")], decomp.data.react2()[ , c("Ages", "x1", "year")],
                  contribution.data.react()[ , c("Ages", "x1", "year")])
    temp
  })

  xaxis.title <- reactive({ 
    switch(input$contribution_type,
           "Years" = "(years)",
           "Proportion (%)" = "(%)")
    })
  
  #label1 <- reactive({BlackWhite$WBgap.s[BlackWhite$State2 == input$state & BlackWhite$Year3 == input$year1 & BlackWhite$Sex2 == intpu$selected_sex]})
  #label2 <- reactive({BlackWhite$WBgap.s[BlackWhite$State2 == input$state & BlackWhite$Year3 == input$year2 & BlackWhite$Sex2 == intpu$selected_sex]})
  #label3 <- reactive({label1-label2})
  
  output$age_bayes <- renderPlotly({
    
    p3 <- ggplotly(ggplot(decomp.data.react3(), aes(y = Ages, x = x1)) + 
                   geom_segment(aes(xend = 0, yend = Ages), lwd = 3, col = "#2166ac") + 
                     theme_minimal() +
                     geom_vline(xintercept = 0) +
                     xlab(paste0("Contribution to life expectancy gap ", xaxis.title())) +
                     facet_wrap(~ year)) %>% 
      layout(title = paste0(input$selected_sex, "s in ", input$state),
             yaxis = list(autorange = "reversed"))
    
    p3
  })
  
  white.y3 <- reactive({
    round(BlackWhite %>% 
            filter(State2 == input$state,Year3 == input$year1, Sex2 == input$selected_sex) %>% 
            select(le.smoothed.white), 1)
  })
  
  white.y4 <- reactive({
    round(BlackWhite %>% 
            filter(State2 == input$state, Year3 == input$year2, Sex2 == input$selected_sex) %>% 
            select(le.smoothed.white), 1)
  })
  
  black.y3 <- reactive({
    round(BlackWhite %>% 
            filter(State2 == input$state,Year3 == input$year1, Sex2 == input$selected_sex) %>% 
            select(le.smoothed.black), 1)
  })
  
  black.y4 <- reactive({
    round(BlackWhite %>% 
            filter(State2 == input$state, Year3 == input$year2, Sex2 == input$selected_sex) %>% 
            select(le.smoothed.black), 1)
  })
  
  output$Explain_Age_Gap <- renderText({
    paste0("The above bar chart illustrates the contribution of each age at death to the total difference in life expectancy between blacks and whites. 
            The first panel is for ", input$year1, " where the total life expectancy difference of ", round(white.y3() - black.y3(), 2), " years is partitioned by age. 
           The second panel is for ", input$year2, " where the total life expectancy difference of ", round(white.y4() - black.y4(), 2), " years is partitioned by age. 
           Finally, the last panel partitions the change in the life expectancy gap, where the change equalled ", 
           round((white.y3() - black.y3()) - (white.y4() - black.y4()), 2), " years.")
  })
  
  #output$Explain_Age_Gap <- renderText({paste0("The life expectancy gap in ", input$year1, " was ", label1, " and in ", input$year2, "was ", input$ )})
  #, col = adds_to_gap
  #           #scale_color_manual(values = c("#d1e5f0", "#2166ac")) +  

  ##########################################
  ##         Cause of death tab           ##
  ##########################################
  
  cod.decomp.data.react <- reactive({
    temp <- data.frame(list.cod.marginal.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                paired.ids$Year3 == input$year1 & 
                                                                paired.ids$Sex2 == input$selected_sex)]])
    temp["x1"] <-  switch(input$contribution_type,
                          "Years" = temp[["C_x_COD"]],
                          "Proportion (%)" = temp[["C_x_COD_proportion"]]
    )
    
    temp[["year"]] <- input$year1
    temp
  })
 
  cod.decomp.data.react2 <- reactive({
    temp <- data.frame(list.cod.marginal.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                  paired.ids$Year3 == input$year2 & 
                                                                  paired.ids$Sex2 == input$selected_sex)]])
    temp["x1"] <-  switch(input$contribution_type,
                          "Years" = temp[["C_x_COD"]],
                          "Proportion (%)" = temp[["C_x_COD_proportion"]]
    )
    
    temp[["year"]] <- input$year2
    temp
  }) 
  
  cod.contribution.data.react <- reactive({
    temp <- contribution.to.gap.change(type.of.decomp = "COD", 
                                       cod.decomp.data.react(),
                                       cod.decomp.data.react2(), type.of.data = "practice")
    
     temp[["x1"]] <- switch(input$contribution_type,
                            "Years" = temp[["Contribution.to.change"]],
                            "Proportion (%)" = temp[["Contrib.to.change.prop"]]
     )

    temp[["year"]] <- "Change"
    temp[["Cause.of.death"]] <- temp[["COD"]]
    temp
  })
  
  cod.decomp.data.react3 <- reactive({
    temp <- rbind(cod.decomp.data.react()[ , c("Cause.of.death", "x1", "year")], cod.decomp.data.react2()[ , c("Cause.of.death", "x1", "year")],
                  cod.contribution.data.react()[ , c("Cause.of.death", "x1", "year")])
    temp
  })
  
  output$cod_bayes <- renderPlotly({  
    ggplotly(ggplot(cod.decomp.data.react3(), aes(y = Cause.of.death, x = x1)) + 
               geom_segment(aes(xend = 0, yend = Cause.of.death, col = Cause.of.death), lwd = 3) + 
               theme_minimal() + theme(legend.position = "none") + 
               geom_vline(xintercept = 0) +
               xlab(paste0("Contribution to life expectancy gap ", xaxis.title())) +
               ylab("Cause of death") +
               facet_wrap( ~ year) 
             ) %>% layout(title = paste0(input$selected_sex, "s in ", input$state), yaxis = list(autorange = "reversed"))
    
  })   

  output$Explain_COD_Gap <- renderText({
    paste0("The above bar chart illustrates the contribution of each cause of death to the total difference in life expectancy between blacks and whites. 
           The first panel is for ", input$year1, " where the total life expectancy difference of ", round(white.y3() - black.y3(), 2), " years is partitioned by cause. 
           The second panel is for ", input$year2, " where the total life expectancy difference of ", round(white.y4() - black.y4(), 2), " years is partitioned by cause. 
           Finally, the last panel partitions the change in the life expectancy gap, where the change equalled ", 
           round((white.y3() - black.y3()) - (white.y4() - black.y4()), 2), " years.")
  })
  
  ##########################################
  ##    Age and cause of death tab        ##
  ##########################################
  
  age.cod.data.react <- reactive({ 
    
    temp <- data.frame(list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                paired.ids$Year3 == input$year1 & 
                                                                paired.ids$Sex2 == input$selected_sex)]])
    
    temp["start.new"] <-  switch(input$contribution_type,
                          "Years" = temp[["start"]],
                          "Proportion (%)" = temp[["start2"]])
    
    temp["finish.new"] <-  switch(input$contribution_type,
                                 "Years" = temp[["finish"]],
                                 "Proportion (%)" = temp[["finish2"]])    
    
    temp["year"] <- input$year1
    
    temp
  })

  age.cod.data.react2 <- reactive({ 
    
    temp <- data.frame(list.cod.decomp.tables.smoothed[[which(paired.ids$State2 ==  input$state & 
                                                                paired.ids$Year3 == input$year2 & 
                                                                paired.ids$Sex2 == input$selected_sex)]])
    
    temp["start.new"] <-  switch(input$contribution_type,
                                 "Years" = temp[["start"]],
                                 "Proportion (%)" = temp[["start2"]])
    
    temp["finish.new"] <-  switch(input$contribution_type,
                                  "Years" = temp[["finish"]],
                                  "Proportion (%)" = temp[["finish2"]])  
    
    temp["year"] <- input$year2
    
    temp
  })
  
  age.cod.data.react3 <- reactive({
    temp <- rbind(age.cod.data.react()[ , c("Cause.of.death", "Ages", "start.new", "finish.new", "year")],
                  age.cod.data.react2()[ , c("Cause.of.death", "Ages", "start.new", "finish.new", "year")])
    temp
  })
  
  output$age_cod_bayes <- renderPlotly({
    
    ggplotly(ggplot(age.cod.data.react3(),
                    aes(y = Ages, x = start.new)) + 
               geom_segment(aes(xend = finish.new, col = Cause.of.death, yend = Ages), lwd = 3) +
               xlab(paste0("Contribution to life expectancy gap ", xaxis.title())) +
               ylab("Age group\n") + theme_minimal() +
               geom_vline(xintercept = 0) + 
               facet_wrap(~ year) +
               ggtitle(paste0(input$selected_sex, "s in ", input$state)) 
             ) 
  })  

}

shinyApp(ui = ui1, server = server)
