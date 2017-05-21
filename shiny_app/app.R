
#note to Corinne: The old versions of the commented packages are stores in ~/.../library/old_versions
#these "old" versions were the ones I originally developed the app using but have since updated to the new versions
#and all the new versions are at the main .libPaths() pathway.
#if you have trouble viewing any of the plots print sessionInfo() -- it could be that packages loaded via namespace
#are not updated (try updating DBI, jsonlite, and R6 to their latest versions.)

library(shiny) #, lib.loc = "/Library/Frameworks/R.framework/Versions/3.3/Resources/library/old_versions") #OLD version: 0.14.1, NEW: 1.0.3
library(ggplot2)#, lib.loc = "/Library/Frameworks/R.framework/Versions/3.3/Resources/library/old_versions") #OLD: 2.2.1, NEW: ggplot2_2.2.1.9000 (install dev version using devtools::install_github)
library(plotly)#, lib.loc = "/Library/Frameworks/R.framework/Versions/3.3/Resources/library/old_versions") #OLD: 4.5.6, NEW: 4.6.0
library(crosstalk)#, lib.loc = "/Library/Frameworks/R.framework/Versions/3.3/Resources/library/old_versions") #1.0.0
library(viridis)
library(scales)
library(shinythemes)
library(dplyr)
library(grid)
library(gridExtra)
library(png)
library(forcats)

source("./www/Code/life_expectancy_functions.R")
#source("./shiny_app/Rsource/SwitchButton.R")

mortality.rates <- read.csv("./www/Results2/mortality_rates_combined.csv")
mortality.rates.diff <- read.csv("./www/Results2/mortality_rates_diff_combined.csv")

mortality.rates <- mortality.rates %>% mutate(state.reorder = reorder(state, as.numeric(Census_Division))) %>% rename(Race = race)
mortality.rates.diff <- mortality.rates.diff %>% mutate(state.reorder = reorder(state, as.numeric(Census_Division))) 

levels(mortality.rates$stabbrs) <- c(levels(mortality.rates$stabbrs), "DC")
levels(mortality.rates.diff$stabbrs) <- c(levels(mortality.rates.diff$stabbrs), "DC")
mortality.rates$stabbrs[mortality.rates$state == "Washington DC"] <- "DC"
mortality.rates.diff$stabbrs[mortality.rates.diff$state == "Washington DC"] <- "DC"

mortality.rates <- reorder.as.map2(mortality.rates, "state", "stabbrs")
mortality.rates.diff <- reorder.as.map2(mortality.rates.diff, "state", "stabbrs")

age_cod_results_female <- read.csv("./www/Results2/age_cod_results_female.csv")
age_cod_results_male <- read.csv("./www/Results2/age_cod_results_male.csv")
age_cod_results <- rbind(age_cod_results_female, age_cod_results_male)
rm(age_cod_results_female, age_cod_results_male)

age_cod_results$age <- forcats::fct_relevel(f = age_cod_results$age, "<1 year", "1-4 years", "5-9 years", "10-14 years", 
                                            "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", 
                                            "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", 
                                            "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years")

age_decomp_results <- read.csv("./www/Results2/age_decomp_results.csv")

age_decomp_results$age <- forcats::fct_relevel(f = age_decomp_results$age, "<1 year", "1-4 years", "5-9 years", "10-14 years", 
                                            "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", 
                                            "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", 
                                            "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years")

cod_decomp_results <- read.csv("./www/Results2/cod_decomp_results.csv")
cod_change_results <- read.csv("./www/Results2/cod_change_results.csv")
BlackWhite_results <- read.csv("./www/Results2/BlackWhite_results.csv")
dat.aggregated <- read.csv("./www/Data/dat_aggregated.csv") 

age_cod_results$COD <- factor(age_cod_results$COD, levels(age_cod_results$COD)[c(3, 2, 4, 6, 5, 1)])
cod_decomp_results$COD <- factor(cod_decomp_results$COD, levels(cod_decomp_results$COD)[c(3, 2, 4, 6, 5, 1)])
#age_decomp_results$age <- factor(age_decomp_results$age, levels = levels(age_decomp_results$age)[c(1, 2, 11, 3:10, 12:19)])
#age_cod_results$age <- factor(age_cod_results$age, levels = levels(age_cod_results$age)[c(1, 2, 11, 3:10, 12:19)])                                    

cod_change_results$Cause.of.death <- factor(cod_change_results$Cause.of.death, levels(cod_change_results$Cause.of.death)[c(3, 2, 4, 6, 5, 1)])

cod_marginal_results <- cod_decomp_results %>% 
  group_by(stratum.id) %>% 
  summarise(state = first(state), sex= first(sex), year = first(year), total_Cx = sum(COD_cont_yrs_mean))

#cod_marginal_results_female <- cod_marginal_results %>% filter(sex == "Female") %>% mutate(reorder(state, total_Cx))

cod_decomp_results <- merge(cod_decomp_results, BlackWhite_results %>% select(stratum.id, LE_white_mean, LE_black_mean, LE_wbgap_mean), 
      by = "stratum.id")

cod_decomp_results$new.start = cod_decomp_results$start + cod_decomp_results$LE_black_mean
cod_decomp_results$new.start2 = cod_decomp_results$start2 + cod_decomp_results$LE_black_mean
cod_decomp_results$new.finish = cod_decomp_results$finish + cod_decomp_results$LE_black_mean
cod_decomp_results$new.finish2 = cod_decomp_results$finish2 +cod_decomp_results$ LE_black_mean

cod_decomp_results$LE_black_mean[cod_decomp_results$COD != "Cardiovascular"] <- NA
cod_decomp_results$LE_white_mean[cod_decomp_results$COD != "Cardiovascular"] <- NA

cod_decomp_results <- reorder.as.map2(cod_decomp_results, "state", "stabbrs")

age_decomp_results <- merge(age_decomp_results, BlackWhite_results %>% select(stratum.id, LE_white_mean, LE_black_mean, LE_wbgap_mean), 
                            by = "stratum.id")

age_decomp_results$new.start = age_decomp_results$start + age_decomp_results$LE_black_mean
age_decomp_results$new.start2 = age_decomp_results$start2 + age_decomp_results$LE_black_mean
age_decomp_results$new.finish = age_decomp_results$finish + age_decomp_results$LE_black_mean
age_decomp_results$new.finish2 = age_decomp_results$finish2 + age_decomp_results$ LE_black_mean

age_decomp_results$LE_black_mean[age_decomp_results$age != "<1 year"] <- NA
age_decomp_results$LE_white_mean[age_decomp_results$age != "<1 year"] <- NA

BlackWhite_results <- reorder.as.map2(BlackWhite_results, "state", "stabbrs")



ui1 <- fluidPage(theme = "cosmo-customized.css",
                 pageWithSidebar(
                 
                 #shinythemes::themeSelector(), 
                 headerPanel("Explore the black-white life expectancy gap in the United States"),
                 
                   sidebarPanel(width = 2,
                                conditionalPanel(condition = "input.tab != 'more' & input.tab != 'state.dashboard'",
                                                 radioButtons(inputId = "sex", label = "Gender:", 
                                                              choices = c("Male", "Female"), selected = "Male")),
                                
                                conditionalPanel(condition = "input.tab == 'LE.summary' || input.tab == 'COD.summary' || input.tab == 'Mortality.trends' || input.tab == 'state.dashboard'",
                                                 sliderInput(inputId = "years_LEgap", label = "Years:", sep = "",
                                                             min = 1969, max = 2013, value = c(1969, 2013))),
                                
                                conditionalPanel(condition = "input.tab == 'LE.summary' || input.tab == 'COD.summary' || input.tab == 'Mortality.trends'",
                                                 radioButtons(inputId = "plot_choice", 
                                                              label = "Plot style:", 
                                                              choices = c("Grid", "Map"), 
                                                              selected = "Grid")),
                                
                                conditionalPanel(condition = "input.tab == 'COD.snapshot' || input.tab == 'Age.snapshot' ",
                                                 selectInput(inputId = "year", label = "Year:", 
                                                  choices = unique(BlackWhite_results$year), width = 100#, sep = "" 
                                                  )
                                                 ),
                                
                                conditionalPanel(condition = "input.tab == 'Mortality.trends' || input.tab == 'COD.summary'",
                                                 selectInput(inputId = "COD", 
                                                             choices = levels(cod_decomp_results$COD), 
                                                             label = "Cause of death: ", width = '150px')),
 
                                conditionalPanel(condition = "input.tab == 'Mortality.trends'",
                                                 radioButtons(inputId = "gap", 
                                                              choices = c("Both races", "Excess risk in blacks"), 
                                                              label = "Trends for: ", width = '150px')),
                                
                                conditionalPanel(condition = "input.tab == 'COD.summary'",
                                                 radioButtons(inputId = "pop_model",
                                                              label = "Population curve:",
                                                              choices = c("Hide", "National"),
                                                              selected = "Hide")),
                                
                                conditionalPanel(condition = "input.tab == 'state.dashboard'",
                                                 selectInput(inputId = "state", label = "State:", 
                                                             choices = levels(BlackWhite_results$state))),
                                absolutePanel(bottom = 0, left = 0, draggable = T,
                                  conditionalPanel(condition = "input.tab == 'state.dashboard'")

                                )
                                ),
                 
                   
                   mainPanel(
                    
                     tabsetPanel(id = "tab",

                                 
                                 tabPanel(title = "Explore a state", value = "state.dashboard",
                                          htmlOutput("description_state_snapshot"),
                                          plotlyOutput("population_trend", height = 300, width = 500),
                                          htmlOutput("title_LE_trends"),
                                          plotlyOutput("life_expectancy"),
                                          htmlOutput("Explain_LE_females"),
                                          htmlOutput("Explain_LE_males"),
                                          plotlyOutput("age_cod", height = 700, width = 1100),
                                          htmlOutput("Alabama")
                                 ),
                                 
                                 tabPanel(title = "Trends in life expectancy gap", value = "LE.summary",
                                          htmlOutput("description_LE_summary"),
                                          plotlyOutput("state_LEsummary", height = 700, width = 900),
                                          uiOutput("excluded")
                                 ),
                                 
                                 tabPanel(title = "Trends in cause contribution", value = "COD.summary",
                                          htmlOutput("description_cod_trends"),
                                          plotlyOutput("contribution_plot", height = 700, width = 900)
                                 ),
                                 
                                 tabPanel(title = "Trends in mortality", value = "Mortality.trends",
                                          htmlOutput("description_mortality_trends"),
                                          plotlyOutput("mortality_plot", height = 700, width = 900)
                                 ),
                                 
                                 tabPanel(title = "Cross-sectional cause contribution", value = "COD.snapshot",
                                          htmlOutput("description_cod_summary"),
                                          plotlyOutput("state_cod_summary", height = 800)),
                                 #dataTableOutput("data.temp")),
                                 
                                 tabPanel(title = "Cross-sectional age contribution", value = "Age.snapshot",                                
                                          htmlOutput("description_age_summary"),
                                          plotlyOutput("state_age_summary", height = 800),
                                          dataTableOutput("data.temp2")),
                                 
                                 tabPanel(title = "More information", value = "more",
                                          htmlOutput("app_description"),
                                          img(src = "CClicense.png")
                                 )
                                 
                     )
                   )
                 
                 )
  
)

server <- function(input, output) {

  ##########################################
  ##  State summary: Life expectancy gap  ##
  ########################################## 
  
  lowercase.sex <- reactive({
    sex <- switch(input$sex,
                  "Male" = "male",
                  "Female" = "female")
  })
  
  lowercase.COD <- reactive({
    COD <- switch(input$COD,
                  "Cardiovascular" = "cardiovascular disease",
                  "Communicable" = "communicable disease",
                  "Cancers" = "cancer",
                  "Injuries" = "injuries",
                  "Non-communicable" = "non-communicable disease",
                  "All other causes" = "all other causes")
  })
  
  output$description_LE_summary <- renderUI({ 
    HTML(paste0("<br/><b>How has the difference in life expectancy between black and white ", lowercase.sex(),
                "s changed over time?</b>",
                "<br/><br/>", 
                "This graph depicts state-level trends in the life expectancy gap between ", 
                input$years_LEgap[1], " and ", input$years_LEgap[2], " for ", lowercase.sex(), "s.",
                ifelse(input$plot_choice == "Grid",
                " Use your mouse to hover over the trend lines to view the estimate of the life expectancy gap for each year and state. Switch to the Map 'Plot style' to display separate panels for every state organized like a map, and bands around these lines showing statistical precision. <br/><br/>",
                " Here, states with less precise estimates (often due to small black populations) have wider ribbons, where these ribbons display the 95% credible brand for the trend line.<br/><br/>"),
                "<h2>State-level trends in the black-white life expectancy gap in ", 
                lowercase.sex(), "s, United States, ", input$years_LEgap[1], "-",
                input$years_LEgap[2], "</h2>"))
  })
  
  grid.contribution.LE <- reactive({
    grid.p <- ggplotly(ggplot(subset(BlackWhite_results, sex == input$sex & year >= input$years_LEgap[1] & year <= input$years_LEgap[2]),
                    aes(x = year, y = LE_wbgap_mean)) + 
               geom_line(aes(col = state)) + 
               facet_wrap(~ Census_Division) +
               ylab("Life expectancy gap (years)") +
               xlab(paste0("Year (", input$years_LEgap[1], "-", input$years_LEgap[2], ")")) +
               geom_text(data = subset(BlackWhite_results, 
                                       year == input$years_LEgap[2]-2 & sex == input$sex), 
                         aes(label = stabbrs), check_overlap = T, size = 2.5) +
               theme_minimal() +  
                 theme(axis.text.x = element_text(angle = 40),
                       panel.background = element_rect(fill = "transparent", colour = NA), 
                       plot.background = element_rect(fill = "transparent", colour = NA)
                       ) +
               geom_hline(yintercept = 0)
             )
    
    for(i in 1:length(grid.p$x$data)){
      grid.p$x$data[[i]]$text <- gsub("LE_wbgap_mean", "Mean life expectancy gap", grid.p$x$data[[i]]$text)
    }
    return(grid.p)
  })
  
  map.contribution.LE <- reactive({
    gg.p <- ggplot(data = subset(BlackWhite_results, sex == input$sex & year >= input$years_LEgap[1] & year <= input$years_LEgap[2]),
                          aes(y = LE_wbgap_mean, x = year)) +
    geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = input$years_LEgap[1])) +
    geom_ribbon(aes(ymin = LE_wbgap_lcl, ymax = LE_wbgap_ucl, fill = Census_Division)) +
    #geom_line(aes(col = Census_Division)) + 
    facet_wrap(~ stabbrs.map.order, ncol = 11, drop = F) +
    theme_classic(base_size = 10) +
    theme(axis.text.x = element_blank(),
          strip.background=element_blank(),
          axis.line=element_blank(),
          axis.ticks=element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = "transparent", colour = NA), 
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA)) +
    ylab("Life expectancy gap (years)") +
    xlab(paste0("Year (", input$years_LEgap[1], "-", input$years_LEgap[2], ")")) #+
    
    
    if(input$sex == "Male"){
      interactive.p <- ggplotly(gg.p +     
        geom_hline(aes(yintercept = 5), lwd = 0.5, col = "grey", alpha = 0.5) +
        geom_hline(aes(yintercept = 10), lwd = 0.5, col = "grey", alpha = 0.5) +
        geom_hline(aes(yintercept = 15), lwd = 0.5, col = "grey", alpha = 0.5))
    } else {
      interactive.p <- ggplotly(gg.p +     
        geom_hline(aes(yintercept = -5), lwd = 0.5, col = "grey", alpha = 0.5) +
        geom_hline(aes(yintercept = 5), lwd = 0.5, col = "grey", alpha = 0.5) +
        geom_hline(aes(yintercept = 10), lwd = 0.5, col = "grey", alpha = 0.5))
    }
    
    for(i in 1:length(interactive.p$x$data)){
      if (interactive.p$x$data[[i]]$line$color %in% c("rgba(0,0,0,1)", "rgba(190,190,190,0.5)")) {
        interactive.p$x$data[[i]]$hoverinfo <- "none"
        interactive.p$x$data[[i]]$line$width <- 1
      }
      
      interactive.p$x$data[[i]]$text <- gsub("Census_Division", "Census Division", interactive.p$x$data[[i]]$text)
      interactive.p$x$data[[i]]$text <- gsub("LE_wbgap_mean", "Mean life expectancy gap", interactive.p$x$data[[i]]$text)
      interactive.p$x$data[[i]]$text <- gsub("LE_wbgap_ucl", "Upper credible limit", interactive.p$x$data[[i]]$text)
      interactive.p$x$data[[i]]$text <- gsub("LE_wbgap_lcl", "Lower credible limit", interactive.p$x$data[[i]]$text)
    }
    
    return(interactive.p)
  })
  

  
  plot.chosen.LE <- reactive({
    plot.LE <- switch(input$plot_choice,
                   "Map" = map.contribution.LE(), 
                   "Grid" = grid.contribution.LE())
    return(plot.LE)
  })
  
  output$state_LEsummary <- renderPlotly({
    plot.chosen.LE() %>% layout(margin = list(l = 75, b = 100)) #add space between plotly yaxis and the UI sidebar
  })
  
  output$excluded <- renderUI({
    HTML(paste0("11 states were excluded from the analysis because they had too small black populations which would have led to very imprecise estimates",
                " that would be difficult to interpret. These states include: Maine, Vermont, New Hampshire, Alaska, Hawaii, Idaho, Montana, North Dakota,",
                " South Dakota, Wyoming, and Utah."))
  })
  ##########################################
  ##            State summary: COD        ##
  ##########################################
  
  output$description_cod_trends <- renderUI({
    HTML(paste0("<br/><b>How many years does each major cause of death contribute to the life expectancy gap in ", lowercase.sex(), 
               "s ?</b><br/><br/>
               Select the cause of death you're interested in to see how many years of the total gap
               is contributed by the selected cause, and how this contributed changed over time.", 
               ifelse(input$pop_model == "Hide", 
                      " To add the national trend to the plot, click the button for 'National' population curve.",
                      ""),
               ifelse(input$plot_choice == "Grid",
               " Use your mouse to hover over the trend lines to view the estimate of the contribution for each year and state. Switch to the Map 'Plot style' to display separate panels for every state organized like a map, and bands around these lines showing statistical precision.",
               " Here, states with less precise estimates (often due to small black populations) have wider ribbons, where these ribbons display the 95% credible brand for the trend line.<br/><br/>"),
               "<h2>Contributions of ", lowercase.COD(), " to the black-white life expectancy gap in ", 
               lowercase.sex(), "s, United States, ", input$years_LEgap[1], "-",
               input$years_LEgap[2], "</h2>"))
  })
  
  contrib.data.react <- reactive({
    temp <- data.frame(subset(cod_decomp_results, sex == input$sex & COD == input$COD & 
                                year >= input$years_LEgap[1] & year <= input$years_LEgap[2]))
    temp["y1"] <-  temp[["COD_cont_yrs_mean"]]
    temp["y1_lcl"] <- temp[["COD_cont_yrs_lcl"]]
    temp["y1_ucl"] <- temp[["COD_cont_yrs_ucl"]]
    temp["y1_for_area"] <- ifelse(temp[["COD_cont_yrs_mean"]] > 0, 
                                  temp[["COD_cont_yrs_lcl"]],
                                  temp[["COD_cont_yrs_ucl"]])
    temp
  })
  
  grid.contribution <- reactive({
    plot <- ggplot(contrib.data.react(), aes(x = year, y = y1)) + 
      geom_line(aes(col = state)) + 
      facet_wrap(~ Census_Division) +
      ylab(paste0("Contribution to life expectancy gap (years)")) +
      xlab(paste0("Year (", input$years_LEgap[1], "-", input$years_LEgap[2], ")")) +
      geom_text(data = subset(contrib.data.react(), 
                              year == 2013 & sex == input$sex & COD == input$COD), 
                aes(label = stabbrs), check_overlap = T, size = 2.5) +
      theme_minimal() +  
      theme(axis.text.x = element_text(angle = 40),
            panel.background = element_rect(fill = "transparent", colour = NA), 
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA)
            ) +
      geom_hline(yintercept = 0) 

    if(input$pop_model == "National"){
      plot <- plot + geom_line(data = contrib.data.react(), aes(x = year, y = fitted.RE - RE.estimates), col = "black", lty = 2)
    }
    
    ggp <- ggplotly(plot)
    
    for(i in 1:length(ggp$x$data)){
      if(ggp$x$data[[i]]$mode != "text" ){
        if (ggp$x$data[[i]]$line$color == "rgba(0,0,0,1)" & ggp$x$data[[i]]$line$dash == "dashed") {
            ggp$x$data[[i]]$hoverinfo <- "none"
            ggp$x$data[[i]]$line$width <- 1
            
        }
      }
      if(ggp$x$data[[i]]$mode == "text" ){
        ggp$x$data[[i]]$hoverinfo <- "none"
      }
      
       ggp$x$data[[i]]$text <- gsub("y1", "Contribution to gap", ggp$x$data[[i]]$text)
       ggp$x$data[[i]]$text <- gsub("fitted.RE - RE.estimates", "National estimate", ggp$x$data[[i]]$text)
       ggp$x$data[[i]]$text <- gsub("fitted.RE", "State estimate", ggp$x$data[[i]]$text)
    }
    
    return(ggp)
  })
  
  
  map.contribution <- reactive({
    plot <- ggplot(contrib.data.react(), aes(x = year, y = y1)) +
      geom_ribbon(aes(ymin = y1_lcl, ymax = y1_ucl, fill = Census_Division)) +
      geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 1969)) +
      facet_wrap(~stabbrs.map.order, ncol = 11, drop = F) +
      theme_classic(base_size = 10) +
      theme(axis.text.x = element_blank(),
            strip.background=element_blank(),
            axis.line=element_blank(),
            axis.ticks=element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA), 
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA),
            legend.position = "none") +
      ylab("Contribution to the life expectancy gap (years)") +
      xlab(paste0("Year (", input$years_LEgap[1], "-", input$years_LEgap[2], ")"))
    
    if(input$pop_model == "National"){
      plot <- plot + geom_line(data = contrib.data.react(), aes(x = year, y = fitted.RE - RE.estimates), col = "black", lty = 2)
    }
    
    ggp <- ggplotly(plot)
     
    for(i in 1:length(ggp$x$data)){
      if (ggp$x$data[[i]]$line$color == "rgba(0,0,0,1)" & ggp$x$data[[i]]$line$dash != "dashed") {
        ggp$x$data[[i]]$hoverinfo <- "none"
        ggp$x$data[[i]]$line$width <- 1
        
      }
      
      ggp$x$data[[i]]$text <- gsub("y1", "Contribution to gap", ggp$x$data[[i]]$text)
      ggp$x$data[[i]]$text <- gsub("fitted.RE - RE.estimates", "National estimate", ggp$x$data[[i]]$text)
      ggp$x$data[[i]]$text <- gsub("fitted.RE", "State estimate", ggp$x$data[[i]]$text)
    }
    
    
    return(ggp)

  })
  
  plot.chosen <- reactive({
    plot <- switch(input$plot_choice,
                   "Map" = map.contribution(), 
                   "Grid" = grid.contribution())
  })
  
  
  output$contribution_plot <- renderPlotly({
    plot.chosen() %>% layout(margin = list(l = 75, b = 100))
  })
  

  ##########################################
  ##     Trends in mortality              ##
  ########################################## 
  
  output$description_mortality_trends <- renderUI({ 
    HTML(paste0("<b>How do the trends in age-standardized mortality differ between blacks and white ", lowercase.sex(), "s for ", lowercase.COD(), "?</b><br/><br/>",
                ifelse(input$gap == "Both races","This plot depicts the cause-specific mortality rates for each race. You can alternatively view the excess risk of mortality among blacks using the selection button 'Excess risk in blacks'.<br/><br/>",
                       "This plot depicts the excess number of deaths among blacks for the selected cause of death. You can alternatively view the rates of mortality for both blacks and whites using the selection button 'Both races'<br/><br/>"),
                "<h2>Age-adjusted mortality (per 100,000) in ", lowercase.sex(), "s for ", lowercase.COD(),"</h2>"))
 })
 
  mortality_plot1 <- reactive({
    
    if(input$plot_choice == "Map"){
      plot <- ggplotly(ggplot(subset(mortality.rates, sex == input$sex & COD == input$COD & year >= input$years_LEgap[1] & year <= input$years_LEgap[2]), 
                              aes(x = year, y = rate.per.100k_mean)) + 
                         geom_ribbon(aes(ymin = rate.per.100k_lcl, ymax = rate.per.100k_ucl, group = Race, fill = Race), col = NA) +
                         facet_wrap( ~ stabbrs.map.order, ncol = 11, drop = F) +
                         xlab(paste0("Year (", input$years_LEgap[1], "-", input$years_LEgap[2], ")")) +
                         ylab("Age-standardized mortality rate (per 100,000)") + 
                         geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 1969)) +  
                         theme_classic(base_size = 10) +
                         theme(axis.text.x = element_blank(),
                               strip.background=element_blank(),
                               axis.line=element_blank(),
                               axis.ticks=element_blank(),
                               panel.background = element_rect(fill = "transparent", colour = NA), 
                               plot.background = element_rect(fill = "transparent", colour = NA),
                               legend.background = element_rect(fill = "transparent", colour = NA)))# %>% layout(legend = list(x = 0.5, y = 0.95))
    }else{ #grid
      plot <- ggplotly(ggplot(subset(mortality.rates, sex == input$sex & COD == input$COD & year >= input$years_LEgap[1] & year <= input$years_LEgap[2]), 
                              aes(x = year, y = rate.per.100k_mean)) + 
                         #geom_ribbon(aes(ymin = rate.per.100k_lcl, ymax = rate.per.100k_ucl, group = interaction(Race, state), fill = state), col = NA, alpha = 0.5) +
                         geom_line(aes(col = state, lty = Race)) +
                         facet_grid(Census_Region ~ Race) +
                         xlab(paste0("Year (", input$years_LEgap[1], "-", input$years_LEgap[2], ")")) + 
                         ylab("Age-standardized mortality rate (per 100,000)") + 
                         geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 1969)) +  
                         #ggtitle(paste0("Age-adjusted mortality (per 100,000) in ", lowercase.sex(), "s for ", lowercase.COD())) +
                         theme_classic(base_size = 10) +
                         theme(axis.text.x = element_blank(),
                               strip.background=element_blank(),
                               axis.line=element_blank(),
                               axis.ticks=element_blank(),
                               panel.background = element_rect(fill = "transparent", colour = NA), 
                               plot.background = element_rect(fill = "transparent", colour = NA),
                               legend.background = element_rect(fill = "transparent", colour = NA))) 
    }
    
    for(i in 1:length(plot$x$data)){
      if (plot$x$data[[i]]$line$color == "rgba(0,0,0,1)") {
        plot$x$data[[i]]$hoverinfo <- "none"
        plot$x$data[[i]]$line$width <- 1
      }
      
      plot$x$data[[i]]$text <- gsub("rate.per.100k_mean", "Mean mortality rate", plot$x$data[[i]]$text)
      plot$x$data[[i]]$text <- gsub("rate.per.100k_lcl", "Lower credible limit", plot$x$data[[i]]$text)
      plot$x$data[[i]]$text <- gsub("rate.per.100k_ucl", "Upper credible limit", plot$x$data[[i]]$text)
    }
    
    return(plot)
  })
  
  mortality_plot2 <- reactive({
    if(input$plot_choice == "Map"){
      plot <- ggplotly(ggplot(subset(mortality.rates.diff, sex == input$sex & COD == input$COD & year >= input$years_LEgap[1] & year <= input$years_LEgap[2]), 
                              aes(x = year, y = rate.difference_mean)) + 
                         geom_ribbon(aes(ymin = rate.difference_LCL, ymax = rate.difference_UCL, fill = Census_Division), col = NA) +
                         facet_wrap( ~ stabbrs.map.order, ncol = 11, drop = F) +
                         xlab(" ") + 
                         ylab("Excess mortality among blacks (per 100,000)") + 
                         geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 1969)) +  
                         theme_classic(base_size = 10) +
                         theme(axis.text.x = element_blank(),
                               strip.background=element_blank(),
                               axis.line=element_blank(),
                               axis.ticks=element_blank(),
                               panel.background = element_rect(fill = "transparent", colour = NA), 
                               plot.background = element_rect(fill = "transparent", colour = NA),
                               legend.background = element_rect(fill = "transparent", colour = NA)))
    }else { #grid
      plot <- ggplotly(ggplot(subset(mortality.rates.diff, sex == input$sex & COD == input$COD & year >= input$years_LEgap[1] & year <= input$years_LEgap[2]), 
                              aes(x = year, y = rate.difference_mean)) + 
                         #geom_ribbon(aes(ymin = rate.difference_LCL, ymax = rate.difference_UCL, group = state), fill = "grey", col = NA, alpha = 0.5) +
                         geom_line(aes(col = state)) +
                         facet_wrap( ~ Census_Division) +
                         xlab(" ") + 
                         ylab("Excess mortality among blacks (per 100,000)") + 
                         geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 1969)) +  
                         theme_classic(base_size = 10) +
                         theme(axis.text.x = element_blank(),
                               strip.background=element_blank(),
                               axis.line=element_blank(),
                               axis.ticks=element_blank(),
                               panel.background = element_rect(fill = "transparent", colour = NA), 
                               plot.background = element_rect(fill = "transparent", colour = NA),
                               legend.background = element_rect(fill = "transparent", colour = NA)))     
    }
    
    for(i in 1:length(plot$x$data)){
      if (plot$x$data[[i]]$line$color == "rgba(0,0,0,1)") {
        plot$x$data[[i]]$hoverinfo <- "none"
        plot$x$data[[i]]$line$width <- 1
      }
      
      plot$x$data[[i]]$text <- gsub("rate.difference_mean", "Mean excess mortality", plot$x$data[[i]]$text)
      plot$x$data[[i]]$text <- gsub("rate.difference_LCL", "Lower credible limit", plot$x$data[[i]]$text)
      plot$x$data[[i]]$text <- gsub("rate.difference_UCL", "Upper credible limit", plot$x$data[[i]]$text)
    }
    
    return(plot)
  })
  
  
  plot.chosen.mortality <- reactive({
    plot.mort <- switch(input$gap,
                      "Both races" = mortality_plot1(), 
                      "Excess risk in blacks" = mortality_plot2())
    return(plot.mort)
  })
  
  output$mortality_plot <- renderPlotly({
    plot.chosen.mortality() %>% layout(margin = list(l = 75))
    })
  
  
  ##########################################
  ##  Cross-sectional cause contribution  ##
  ########################################## 
  
  output$description_cod_summary <- renderUI({ 
           HTML(paste0("<b>Which causes of death contributed most to the ", lowercase.sex()," life expectancy gap 
                                                      between blacks and whites in ", input$year, "?</b>",
                      "<br/><br/>", "This graph depicts the difference in life expectancy between white ", lowercase.sex(), 
                             "s (vertical black line) and black ", lowercase.sex(), "s (dashed black line). 
                             Causes to the left of the dashed line narrow the gap in ", input$year, " 
                             whereas causesto the right exacerbate it. Use these buttons to change the gender or year being examined.<br/><br/>
                             <h3>Contribution of major causes of death to the life expectancy gap (years)</h3>"))
  })
  
  temp.df <- reactive({
    temp <- data.frame(subset(cod_decomp_results, sex == input$sex & year == input$year))
    temp["state.reorder2"] <- reorder(temp$state, temp$LE_black_mean, max, na.rm = T)
    temp["state.reorder2.n"] <- as.numeric(temp[["state.reorder2"]])
    temp
  })
  
  output$state_cod_summary <- renderPlotly({
    interactive.plot <- ggplotly(ggplot(temp.df(), 
                    aes(x = new.start, y = state.reorder2.n)) + 
               geom_segment(aes(x = LE_black_mean, xend = LE_white_mean, 
                                y = state.reorder2.n + 0.46, yend = state.reorder2.n + 0.46)) + 
               
               geom_rect(aes(xmin = new.start, 
                             ymin = state.reorder2.n - 0.44, 
                             ymax = state.reorder2.n + 0.44, 
                             xmax = new.finish, fill = COD)) +
               scale_y_continuous(breaks = 1:length(levels(factor(temp.df()$state.reorder2))), 
                                  labels = levels(factor(temp.df()$state.reorder2))) +
               theme_minimal() + xlab(" ") + 
                 theme(legend.title = element_blank(),
                       panel.background = element_rect(fill = "transparent", colour = NA), 
                       plot.background = element_rect(fill = "transparent", colour = NA),
                       legend.background = element_rect(fill = "transparent", colour = NA)) +
               geom_segment(aes(x = LE_white_mean, xend = LE_white_mean, y = state.reorder2.n - 0.46, yend = state.reorder2.n + 0.46)) +
               geom_segment(aes(x = LE_black_mean, xend = LE_black_mean, y = state.reorder2.n - 0.46, yend = state.reorder2.n + 0.46), lty = 3) 
    )
    
    for(i in 1:length(interactive.plot$x$data)){
      if (interactive.plot$x$data[[i]]$line$color == "rgba(0,0,0,1)") {
        interactive.plot$x$data[[i]]$line$width <- 1
        if(interactive.plot$x$data[[i]]$line$dash == "dot"){
          interactive.plot$x$data[[i]]$text <- "Black Life Expectancy"  
        }
        if(interactive.plot$x$data[[i]]$line$dash == "solid"){
          interactive.plot$x$data[[i]]$text <- "White Life Expectancy"  
        }
      }
    }
    
    interactive.plot %>% 
      layout(xaxis = list(title = " ", side = "top"), yaxis = list(title = NA, autorange = "reversed"))
    
    
  })

  ##########################################
  ##   Cross-sectional age contribution   ##
  ########################################## 
  
  temp.df2 <- reactive({
    temp <- data.frame(subset(age_decomp_results, sex == input$sex & year == input$year))
    temp["state.reorder2"] <- reorder(temp$state, temp$LE_black_mean, max, na.rm = T)
    temp["state.reorder2.n"] <- as.numeric(temp[["state.reorder2"]])
    temp
  })
  
  output$description_age_summary <- renderUI({HTML(paste0("<b>Which age group contributed most to the ", lowercase.sex()," life expectancy gap 
                                                      between blacks and whites in ", input$year, "?</b><br/><br/> This graph depicts the difference in 
                                                      life expectancy between white ", lowercase.sex(), "s (vertical red line) and black ", 
                                                      lowercase.sex(), "s (dashed red line). Ages to the left of the dashed line narrow 
                                                      the gap in ", input$year, " whereas ages to the right exacerbate it.<br/><br/>
                                                      <h3>Contribution of age to the life expectancy gap (years)</h3>"))
  })

  output$state_age_summary <- renderPlotly({
    ly <- ggplotly(ggplot(temp.df2(), 
                    aes(x = new.start, y = state.reorder2.n)) + 
               geom_segment(aes(x = LE_black_mean, xend = LE_white_mean, 
                                y = state.reorder2.n + 0.46, yend = state.reorder2.n + 0.46), col = "red") + 
               
               geom_rect(aes(xmin = new.start, 
                             ymin = state.reorder2.n - 0.44, 
                             ymax = state.reorder2.n + 0.44, 
                             xmax = new.finish, fill = age)) +
               scale_y_continuous(breaks = 1:length(levels(factor(temp.df()$state.reorder2))), 
                                  labels = levels(factor(temp.df()$state.reorder2))) +
               theme_minimal() + 
                 theme(legend.title = element_blank(),
                       panel.background = element_rect(fill = "transparent", colour = NA), 
                       plot.background = element_rect(fill = "transparent", colour = NA),
                       legend.background = element_rect(fill = "transparent", colour = NA)) +
               geom_segment(aes(x = LE_white_mean, xend = LE_white_mean, y = state.reorder2.n - 0.46, yend = state.reorder2.n + 0.46), col = "red") +
               geom_segment(aes(x = LE_black_mean, xend = LE_black_mean, y = state.reorder2.n - 0.46, yend = state.reorder2.n + 0.46), col = "red", lty = 3) +
               scale_fill_viridis(discrete = T, direction = -1) 
    )  
    
    for(i in 1:length(ly$x$data)){
      if (ly$x$data[[i]]$line$color == "rgba(255,0,0,1)") {
        ly$x$data[[i]]$line$width <- 1
        if(ly$x$data[[i]]$line$dash == "dot"){
          ly$x$data[[i]]$text <- "Black Life Expectancy"  
        }
        if(ly$x$data[[i]]$line$dash == "solid"){
          ly$x$data[[i]]$text <- "White Life Expectancy"  
        }
        }
    }
    
    ly %>% layout(xaxis = list(title = " ", side = "top", xpad = 20), yaxis = list(title = NA, autorange = "reversed"))
  })
  
  
##########################################
##            Explore a state           ##
##########################################

output$description_state_snapshot <- renderUI({
  HTML(paste0("<br/><h2>Understanding life expectancy differences between blacks and whites in ", input$state,"</h2><br/>This report brings together key metrics for ", input$state,
              " between ", input$years_LEgap[1], " and ", input$years_LEgap[2],
              ". In terms of population growth, many states have relatively fewer black people, or smaller populations overall.",
              " Hover over the trend lines in this graph to see the population estimates by race and gender, and remember that",
              " states with fewer people offer less precise estimates of health inequalities.<br/><br/>",
              "<h2>Population Growth</h2>"))
})

facet_names <- list(
  'Male' = "Male Life Expectancy (years)",
  'Female' = "Female Life Expectancy (years)"
)

output$population_trend <- renderPlotly({
  p <- ggplotly(
    ggplot(subset(dat.aggregated, state == input$state & age_minbin == 0 & 
                    year >= input$years_LEgap[1] & year <= input$years_LEgap[2]),
           aes(y = pop_across_age, x = year)) + 
      geom_line(aes(col = sex, lty = race)) +
      scale_y_continuous(name = "", label = comma) + 
      scale_x_continuous("") + 
      theme_minimal() + 
      theme(legend.title = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA), 
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))) 
  
  for(i in 1:length(p$x$data)){
    p$x$data[[i]]$text <- gsub("pop_across_age", "Population Size", p$x$data[[i]]$text)
  }

  p %>% layout(margin = list(l = 100))
  
})

output$title_LE_trends <- renderUI({
  HTML(paste0("</br></br>The following graph depicts life expectancy for blacks and whites, and illustrates the black-white life expectancy ",
              "difference (also known as the life expectancy 'gap'). In many states, life expectancy for blacks and whites increased throughout",
              " history, and the gap decreased, although the size of the current gap and the pattern of change varies by state.",
              " The colored bands around each trend line denote the 95% credible region for the estimates, such that states with smaller black populations ",
              "offer less precise estimates that should be interpreted more cautiously.",
              "<h2>Trends in life expectancy in ", input$state,"</h2>"))
})

output$life_expectancy <- renderPlotly({
  
  p1 <- ggplotly(ggplot(subset(BlackWhite_results, state == input$state & year >= input$years_LEgap[1] &
                                 year <= input$years_LEgap[2]), aes(x = year, y = LE_white_mean)) +
                   geom_line(col = "black") + 
                   geom_ribbon(aes(ymin = LE_white_lcl, ymax = LE_white_ucl, fill = sex), alpha = 0.3) +
                   geom_line(aes(y = LE_black_mean), lty = 2, col = "black") + 
                   geom_ribbon(aes(ymin = LE_black_lcl, ymax = LE_black_ucl, fill = sex), alpha = 0.3) +
                   facet_grid(. ~ sex, labeller = as_labeller(facet_names)) +
                   geom_text(data = subset(BlackWhite_results, year == 2013 & state == input$state),
                             aes(y = LE_white_mean - 1),
                             label = "White", check_overlap = T, size = 2.5) +
                   geom_text(data = subset(BlackWhite_results, year == 2013 & state == input$state),
                             aes(y = LE_black_mean - 1),
                             label = "Black", check_overlap = T, size = 2.5) +
                   theme_minimal() +
                   theme(legend.title = element_blank(),
                         panel.background = element_rect(fill = "transparent", colour = NA), 
                         plot.background = element_rect(fill = "transparent", colour = NA),
                         legend.background = element_rect(fill = "transparent", colour = NA))
  ) %>% 
    layout(yaxis = list(title = "Life expectancy"))
  
  p1
  
  p2 <- ggplotly(ggplot(subset(BlackWhite_results, state == input$state & year >= input$years_LEgap[1] &
                                 year <= input$years_LEgap[2]), aes(x = year, y = LE_wbgap_mean)) + 
                   geom_ribbon(aes(ymin = LE_wbgap_lcl, ymax = LE_wbgap_ucl, fill = sex)) +
                   geom_line(col = "black", lty = 3) +
                   facet_grid(. ~ sex) +
                   expand_limits(y = 0) +
                   geom_hline(yintercept = 0, lwd = 0.5) + 
                   theme_minimal() + 
                   theme(legend.title=element_blank(), 
                         strip.text.x = element_blank(),
                         panel.background = element_rect(fill = "transparent", colour = NA), 
                         plot.background = element_rect(fill = "transparent", colour = NA),
                         legend.background = element_rect(fill = "transparent", colour = NA))) %>% 
    layout(yaxis = list(title = "Difference"), xaxis = list(title = " "))
  
  for(i in 1:length(p1$x$data)){
    p1$x$data[[i]]$text <- gsub("LE_white_mean", "Mean life expectancy, white", p1$x$data[[i]]$text)
    p1$x$data[[i]]$text <- gsub("LE_black_mean", "Mean life expectancy, black", p1$x$data[[i]]$text)
    p1$x$data[[i]]$text <- gsub("LE_white_ucl", "Upper credible limit", p1$x$data[[i]]$text)
    p1$x$data[[i]]$text <- gsub("LE_black_ucl", "Upper credible limit", p1$x$data[[i]]$text)
    p1$x$data[[i]]$text <- gsub("LE_white_lcl", "Lower credible limit", p1$x$data[[i]]$text)
    p1$x$data[[i]]$text <- gsub("LE_black_lcl", "Lower credible limit", p1$x$data[[i]]$text)
  }
  
  for(i in 1:length(p2$x$data)){
    p2$x$data[[i]]$text <- gsub("LE_wbgap_mean", "Mean life expectancy gap", p2$x$data[[i]]$text)
    p2$x$data[[i]]$text <- gsub("LE_wbgap_lcl", "Lower credible limit", p2$x$data[[i]]$text)
    p2$x$data[[i]]$text <- gsub("LE_wbgap_ucl", "Upper credible limit", p2$x$data[[i]]$text)
  }
  
  subplot(p1, p2, shareX = T, nrows = 2, titleY = T) %>% layout(margin = list(t=50, b=50, l = 50))
})

white.y1 <- reactive({
  round(BlackWhite_results %>% 
          filter(state == input$state, year == input$years_LEgap[1], sex == "Male") %>% 
          select(LE_white_mean), 1)
})

white.y2 <- reactive({
  round(BlackWhite_results %>% 
          filter(state == input$state, year == input$years_LEgap[2], sex == "Male") %>% 
          select(LE_white_mean), 1)
})

black.y1 <- reactive({
  round(BlackWhite_results %>% 
          filter(state == input$state, year == input$years_LEgap[1], sex == "Male") %>% 
          select(LE_black_mean), 1)
})

black.y2 <- reactive({
  round(BlackWhite_results %>% 
          filter(state == input$state, year == input$years_LEgap[2], sex == "Male") %>% 
          select(LE_black_mean), 1)
}) 



fwhite.y1 <- reactive({
  round(BlackWhite_results %>% 
          filter(state == input$state, year == input$years_LEgap[1], sex == "Female") %>% 
          select(LE_white_mean), 1)
})

fwhite.y2 <- reactive({
  round(BlackWhite_results %>% 
          filter(state == input$state, year == input$years_LEgap[2], sex == "Female") %>% 
          select(LE_white_mean), 1)
})

fblack.y1 <- reactive({
  round(BlackWhite_results %>% 
          filter(state == input$state, year == input$years_LEgap[1], sex == "Female") %>% 
          select(LE_black_mean), 1)
})

fblack.y2 <- reactive({
  round(BlackWhite_results %>% 
          filter(state == input$state, year == input$years_LEgap[2], sex == "Female") %>% 
          select(LE_black_mean), 1)
}) 

direction.female <- reactive({
  sign <- "decreased"
  if((fwhite.y1() - fblack.y1()) < (fwhite.y2() - fblack.y2())){ 
    sign <- "increased"
  }
  return(sign)
})

direction.male <- reactive({
  sign <- "decreased"
  if((white.y1() - black.y1()) < (white.y2() - black.y2())){ 
    sign <- "increased"
  }
  return(sign)
})

output$Explain_LE_females <- renderUI({
  HTML(paste0("In ", input$state, " between ", input$years_LEgap[1],
              " and ", input$years_LEgap[2],", life expectancy at birth changed from ", fwhite.y1(), 
         " years to ", fwhite.y2(), " years (for a change of ", round(fwhite.y2()-fwhite.y1(),1), 
         " years) for white females. For black females, the change was from ", fblack.y1()," years to ", fblack.y2(), 
         " years (", round(fblack.y2()-fblack.y1(),1),
         " years change). The black-white difference in female life expectancy ", direction.female()," by ",
         round((fwhite.y1() - fblack.y1()) - (fwhite.y2() - fblack.y2()), 1), " years: from ",
         round(fwhite.y1() - fblack.y1(), 1), " years to ", round(fwhite.y2() - fblack.y2(), 1) , " years.<br/><br/>"
         ) )
  
})

output$Explain_LE_males <- renderUI({
  HTML(paste0("For white males, life expectancy at birth changed from ",  white.y1(), 
              " years to ", white.y2(), " years (for a change of ", round(white.y2()-white.y1(),1), 
              " years) for white males. For black males, the change was from ", black.y1()," years to ", black.y2(), 
              " years (", round(black.y2()-black.y1(),1),
              " years change). The black-white difference in male life expectancy ", direction.male()," by ",
              round((white.y1() - black.y1()) - (white.y2() - black.y2()), 1), " years: from ",
              round(white.y1() - black.y1(), 1), " years to ", round(white.y2() - black.y2(), 1) , " years.<br/><br/>",
              "<h2>Age and cause-of-death contribution to the black-white life expectancy gap for ", input$state," in ", input$years_LEgap[1], " vs. ",
              input$years_LEgap[2], "</h2>",
              "The following plots contrast how black-white differences in mortality across the life span contribute to the overall difference ",
              "in life expectancy that was portrayed in the previous plot. For each age band, the total length of the bar indicates how may years ",
              "of the total life expectancy gap can be attributed to differences in mortality between blacks in whites at the specified ages.",
              " Each age bar is further partitioned (by color) according to the cause of death, such that ",
              " the cause associated with the greatest inequality in deaths defines the largest portion of the total bar.",
              " Use these plots to understand how the black-white difference in life expectancy has changed over time, by comparing the length of",
              " the bars at a given age group across years, and how these bars are partitioned by cause of death.")) 
  
})

output$age_cod <- renderPlotly({
  sub1 <- subset(age_cod_results, year %in% c(input$years_LEgap[1], input$years_LEgap[2]) & state == input$state)
  ly1 <- ggplotly(
    ggplot(data = sub1,
           aes(x=age, y = age_COD_cont_yrs_mean, fill = COD)) +
      geom_bar(stat = "identity") + coord_flip() + theme_minimal()  + geom_vline(aes(xintercept = 0)) +
      theme(legend.title = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA), 
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA)) +
      ylab("Contribution to the life expectancy gap (years)") +
      xlab("") +
      facet_wrap(year ~ sex) 
  ) 
  
  for(i in 1:length(ly1$x$data)){
    ly1$x$data[[i]]$text <- gsub("age_COD_cont_yrs_mean", "Contribution to gap (yrs)", ly1$x$data[[i]]$text)
  }
  
  ly1 %>% layout(margin = list(b = 100))
  
})

output$Alabama <- renderUI({
  if(input$state != "Alabama"){
    HTML(paste0("If you would like an example of how to interpret these plots, please select Alabama using the selection panel (scroll up!), and an interpretation will appear."))
  }else{
  HTML(paste0("<b>Sample interpretation for Alabama:</b> From the previous plot, the life expectancy gap was 7.8 years in Alabama females in 1969.",
              " This plot shows that nearly 1.5 years of this gap can be attributed to higher mortality among blacks during the first year of life.",
              " Blacks also had higher mortality rates during adulthood, and exhibit the most excess mortality related to cardiovascular disease.",
              " Among males, infant mortality also contributes greatly to the life expectancy gap in 1969, while excess injury-related deaths",
              " among black men is a large contributor to the gap, especially for young and middle-aged adult men.<br/><br/> ",
              "By contrast, the life expectancy gap is much smaller in 2013 for both men and women. Of the gap of 1.9 years in females, ",
              "about 0.5 years is attributable to differences in infant mortality. Black males continued to experience a higher risk of death ",
              "from cardiovascular disease throughout their lives (except in the oldest age group), but cancer also plays a role.",
              " White women are dying of a higher rate of injury related deaths (as depicted in the graph by negative contributions",
              " of injuries to the gap among females in 1969), which contributed to narrowing the life expectancy gap. While",
              " all other major causes of death contribute to increasing the gap. Among males, cardiovascular disease and cancer play the biggest",
              " roles in the gap in 2013, although higher injury rates among black men persist in adolescence and early adulthood."))
  }
})

##########################################
##          More information            ##
########################################## 

output$app_description <- renderUI({
  HTML(paste0("<b>The data</b><br/>We accessed the raw data using the National Cancer Institute's SEER*stat software.", 
              " We extracted mortality counts within strata of state, sex, year, race, age group, and cause of death.",
              "These data were smoothed over time using an autoreggresive Bayesian model and suppressed counts (between 
              1 and 10) were imputed using truncated Poisson regression.<br/><br/>",
              "<b>Who we are</b><br/>", 
              "We are a group of epidemiologists from McGill University in Montreal, Canada. ", 
              "These efforts were led by <b>Corinne Riddell</b>. She collected the data, coded and ", 
              "performed demographic analyses (life tables, cause of death decompositions), and ",
              "created this shiny app. <b>Kathryn Morrison</b> is our resident Bayesian, ",
              "and she coded up the Bayesian model to calculate smoothed mortality rates and their ",
              "credible intervals. This work was massively inspired by previous work of our mentors ",
              "and co-authors, <b>Sam Harper</b> and <b>Jay Kaufman</b>. They provided mentorship, ",
              "guidance, and boat-loads of background reading.<br/><br/> ",
              
              "<b>Many thanks</b><br/> ...to the folks at RStudio for the creation and maintenance of ggplot2 and shiny, ",
              "our friends at Plotly, whose interactive plots are top-notch, and, Martyn ",
              "Plummer, the R package maintainer for JAGS who also manages the JAGS forum on sourceforge. <br/><br/>",
 
              "<b>License</b><br/> This software created by Corinne Riddell is licensed under a Creative Commons Attribution-Noncommercial 4.0 International License."
              
              
  ))
})

}
shinyApp(ui = ui1, server = server)