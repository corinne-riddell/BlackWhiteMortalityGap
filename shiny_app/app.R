library(shiny)
library(plotly)
library(viridis)
library(scales)
library(shinythemes)
library(dplyr)

source(".././Code/life_expectancy_functions.R")
#source("./shiny_app/Rsource/SwitchButton.R")

mortality.rates <- read.csv(".././Results/mortality_rates_combined.csv")
mortality.rates.diff <- read.csv(".././Results/mortality_rates_diff_combined.csv")

mortality.rates <- mortality.rates %>% mutate(state.reorder = reorder(state, as.numeric(Census_Division))) %>% rename(Race = race)
mortality.rates.diff <- mortality.rates.diff %>% mutate(state.reorder = reorder(state, as.numeric(Census_Division))) 

levels(mortality.rates$stabbrs) <- c(levels(mortality.rates$stabbrs), "DC")
levels(mortality.rates.diff$stabbrs) <- c(levels(mortality.rates.diff$stabbrs), "DC")
mortality.rates$stabbrs[mortality.rates$state == "Washington DC"] <- "DC"
mortality.rates.diff$stabbrs[mortality.rates.diff$state == "Washington DC"] <- "DC"

mortality.rates <- reorder.as.map(mortality.rates, "state", "stabbrs")
mortality.rates.diff <- reorder.as.map(mortality.rates.diff, "state", "stabbrs")

age_cod_results_female <- read.csv(".././Results2/age_cod_results_female.csv")
age_cod_results_male <- read.csv(".././Results2/age_cod_results_male.csv")
age_cod_results <- rbind(age_cod_results_female, age_cod_results_male)
rm(age_cod_results_female, age_cod_results_male)

age_decomp_results <- read.csv(".././Results2/age_decomp_results.csv")
cod_decomp_results <- read.csv(".././Results2/cod_decomp_results.csv")
cod_change_results <- read.csv(".././Results2/cod_change_results.csv")
BlackWhite_results <- read.csv(".././Results2/BlackWhite_results.csv")
dat.aggregated <- read.csv(".././Data/dat_aggregated.csv") 

age_cod_results$COD <- factor(age_cod_results$COD, levels(age_cod_results$COD)[c(3, 2, 4, 6, 5, 1)])
cod_decomp_results$COD <- factor(cod_decomp_results$COD, levels(cod_decomp_results$COD)[c(3, 2, 4, 6, 5, 1)])
age_decomp_results$age <- factor(age_decomp_results$age, levels = levels(age_decomp_results$age)[c(1, 2, 11, 3:10, 12:19)])
age_cod_results$age <- factor(age_cod_results$age, levels = levels(age_cod_results$age)[c(1, 2, 11, 3:10, 12:19)])                                    

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

cod_decomp_results <- reorder.as.map(cod_decomp_results, "state", "stabbrs")

age_decomp_results <- merge(age_decomp_results, BlackWhite_results %>% select(stratum.id, LE_white_mean, LE_black_mean, LE_wbgap_mean), 
                            by = "stratum.id")

age_decomp_results$new.start = age_decomp_results$start + age_decomp_results$LE_black_mean
age_decomp_results$new.start2 = age_decomp_results$start2 + age_decomp_results$LE_black_mean
age_decomp_results$new.finish = age_decomp_results$finish + age_decomp_results$LE_black_mean
age_decomp_results$new.finish2 = age_decomp_results$finish2 + age_decomp_results$ LE_black_mean

age_decomp_results$LE_black_mean[age_decomp_results$age != "<1 year"] <- NA
age_decomp_results$LE_white_mean[age_decomp_results$age != "<1 year"] <- NA

BlackWhite_results <- reorder.as.map(BlackWhite_results, "state", "stabbrs")



ui1 <- fluidPage(theme = shinytheme("cosmo"),
                 pageWithSidebar(
                 
                 #shinythemes::themeSelector(), 
                 headerPanel("Explore the black-white life expectancy gap in the United States"),
                 
                   sidebarPanel(width = 2,
                                conditionalPanel(condition = "input.tab != 'more' & input.tab != 'state.dashboard' ",
                                                 radioButtons(inputId = "sex", label = "Gender:", 
                                                              inline = T, choices = c("Male", "Female"), selected = "Male")),
                                
                                conditionalPanel(condition = "input.tab == 'LE.summary' || input.tab == 'COD.summary' || input.tab == 'Mortality.trends'",
                                                 sliderInput("years_LEgap", label = "Years:",
                                                             min = 1969, max = 2013, value = c(1969, 2013)),
                                                 radioButtons(inputId = "plot_choice", 
                                                              label = "Plot style:", 
                                                              inline = T, choices = c("Map", "Grid"))),
                                
                                conditionalPanel(condition = "input.tab == 'COD.snapshot' || input.tab == 'Age.snapshot' ",
                                                 selectInput(inputId = "year", label = "Year:", 
                                                  choices = unique(BlackWhite_results$year), width = 100)),
                                
                                conditionalPanel(condition = "input.tab == 'Mortality.trends' || input.tab == 'COD.summary'",
                                                 selectInput(inputId = "COD", 
                                                             choices = levels(cod_decomp_results$COD), 
                                                             label = "Cause of death: ", width = '150px')),
 
                                conditionalPanel(condition = "input.tab == 'Mortality.trends'",
                                                 radioButtons(inputId = "gap", 
                                                              choices = c("Both races", "Excess risk"), 
                                                              label = "Trends for: ", width = '150px')),
                                
                                conditionalPanel(condition = "input.tab == 'COD.summary'",
                                                 # switchButton(inputId = "pop_model",
                                                 #              label = "Show population curve?",
                                                 #              value = FALSE, col = "GB", type = "OO"),
                                                 radioButtons(inputId = "pop_model",
                                                              label = "Population curve:",
                                                              choices = c("Hide", "State-specific", "Aggregated"), inline = F,
                                                              selected = "Hide"),
                                                 radioButtons(inputId = "contribution_type", 
                                                              label = "Contribution format:", 
                                                              choices = c("Years", "Proportion (%)")
                                                              ))
                                ),
                   
                   mainPanel(
                    
                     tabsetPanel(id = "tab",

                       tabPanel(title = "Trends in life expectancy gap", value = "LE.summary",
                                htmlOutput("description_LE_summary"),
                                plotlyOutput("state_LEsummary", height = 700, width = 1100)
                                ),
                       
                       tabPanel(title = "Trends in COD contribution", value = "COD.summary",
                                htmlOutput("description_cod_trends"),
                                plotlyOutput("contribution_plot", height = 700, width = 1100)
                                ),
                       
                       tabPanel(title = "Trends in mortality", value = "Mortality.trends",
                                htmlOutput("description_mortality_trends"),
                                plotlyOutput("mortality_plot", height = 700, width = 1100)
                       ),
                       
                       tabPanel(title = "Cross-sectional COD contribution", value = "COD.snapshot",
                                htmlOutput("description_cod_summary"),
                                plotlyOutput("state_cod_summary", height = 800)),
                                #dataTableOutput("data.temp")),
                       
                       tabPanel(title = "Cross-sectional age contribution", value = "Age.snapshot",                                
                                htmlOutput("description_age_summary"),
                                plotlyOutput("state_age_summary", height = 800),
                                dataTableOutput("data.temp2")),
                       
                       tabPanel(title = "State dashboard", value = "state.dashboard",
                                selectInput(inputId = "dashboard_state", label = "State:", 
                                            choices = levels(BlackWhite_results$state)),
                                htmlOutput("description_state_snapshot"),
                                plotlyOutput("population_trend", height = 300, width = 500),
                                plotlyOutput("life_expectancy"),
                                textOutput("Explain_LE_males"),
                                textOutput("Explain_LE_females"),
                                plotOutput("age_cod1")
                                # plotlyOutput("age_cod2"),
                                # textOutput("Explain_Age_Gap"),
                                # plotlyOutput("cod_bayes"),
                                #textOutput("Explain_COD_Gap"))
                       ),
                       
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
    HTML(paste0("<b>How has the difference in life expectancy between black and white ", lowercase.sex(),
                "s changed over time?</b>",
                "<br/><br/>", 
                "This graph depicts state-level trends in the life expectancy gap between ", 
                input$years_LEgap[1], " and ", input$years_LEgap[2], " for ", lowercase.sex(), "s. 
                The grey band represents the 95% credible interval for the estimated mean trend line.<br/>"))
  })
  
  grid.contribution.LE <- reactive({
    ggplotly(ggplot(subset(BlackWhite_results, sex == input$sex & year >= input$years_LEgap[1] & year <= input$years_LEgap[2]),
                    aes(x = year, y = LE_wbgap_mean)) + 
               geom_line(aes(col = state)) + 
               facet_wrap(~ Census_Division) +
               ylab("Life expectancy gap (years)") +
               xlab(paste0("Year (", input$years_LEgap[1], "-", input$years_LEgap[2], ")")) +
               geom_text(data = subset(BlackWhite_results, 
                                       year == input$years_LEgap[2]-2 & sex == input$sex), 
                         aes(label = stabbrs), check_overlap = T, size = 2.5) +
               theme_minimal() +  theme(axis.text.x = element_text(angle = 40)) +
               geom_hline(yintercept = 0)
             )
  })
  
  map.contribution.LE <- reactive({
    interactive.p <- ggplotly(ggplot(data = subset(BlackWhite_results, sex == input$sex & year >= input$years_LEgap[1] & year <= input$years_LEgap[2]),
                          aes(y = LE_wbgap_mean, x = year)) +
    geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = input$years_LEgap[1])) +
    geom_ribbon(aes(ymin = LE_wbgap_lcl, ymax = LE_wbgap_ucl), fill = "grey") +
    geom_line(aes(col = Census_Division)) + 
    facet_wrap(~ stabbrs.map.order, ncol = 11, drop = F) +
    theme_classic(base_size = 10) +
    theme(axis.text.x = element_blank(),
          strip.background=element_blank(),
          axis.line=element_blank(),
          axis.ticks=element_blank()) +
    ylab("Life expectancy gap (years)") +
    xlab(paste0("Year (", input$years_LEgap[1], "-", input$years_LEgap[2], ")")))
    
    for(i in 1:length(interactive.p$x$data)){
      if (interactive.p$x$data[[i]]$line$color == "rgba(0,0,0,1)") {
        interactive.p$x$data[[i]]$hoverinfo <- "none"
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
    plot.chosen.LE()
  })
  
  ##########################################
  ##            State summary: COD        ##
  ##########################################
  
  output$description_cod_trends <- renderUI({
    HTML(paste0("<b>How many years does each major cause of death contribute to the life expectancy gap in ", lowercase.sex(), 
               "s ?</b><br/><br/>
               Select the cause of death you're interested in to see how many years of the total gap
               is due the selected cause, and how this changed over time. You can also view the proportional
               contribution by selecting 'Proportion (%)'"))
  })
  
  contrib.data.react <- reactive({
    temp <- data.frame(subset(cod_decomp_results, sex == input$sex & COD == input$COD & 
                                year >= input$years_LEgap[1] & year <= input$years_LEgap[2]))
    temp["y1"] <-  switch(input$contribution_type,
                          "Years" = temp[["COD_cont_yrs_mean"]],
                          "Proportion (%)" = temp[["COD_cont_prop_mean"]]
    )
    temp["y1_lcl"] <- switch(input$contribution_type,
                             "Years" = temp[["COD_cont_yrs_lcl"]],
                             "Proportion (%)" = temp[["COD_cont_prop_lcl"]]
    )
    temp["y1_ucl"] <- switch(input$contribution_type,
                             "Years" = temp[["COD_cont_yrs_ucl"]],
                             "Proportion (%)" = temp[["COD_cont_prop_ucl"]]
    )
    temp["y1_for_area"] <- switch(input$contribution_type,
                                  "Years" = ifelse(temp[["COD_cont_yrs_mean"]] > 0, temp[["COD_cont_yrs_lcl"]],
                                                   temp[["COD_cont_yrs_ucl"]]),
                                  "Proportion (%)" = ifelse(temp[["COD_cont_yrs_mean"]] > 0, temp[["COD_cont_prop_lcl"]],
                                                            temp[["COD_cont_prop_ucl"]])
    )
    temp
  })
  
  xaxis.title <- reactive({ 
    switch(input$contribution_type,
           "Years" = "(years)",
           "Proportion (%)" = "(%)")
  })
  
  grid.contribution <- reactive({
    plot <- ggplot(contrib.data.react(), aes(x = year, y = y1)) + 
      geom_line(aes(col = state)) + 
      facet_wrap(~ Census_Division) +
      ylab(paste0("Contribution to LE Gap", xaxis.title())) +
      xlab(paste0("Year (", input$years_LEgap[1], "-", input$years_LEgap[2], ")")) +
      geom_text(data = subset(contrib.data.react(), 
                              year == 2013 & sex == input$sex & COD == input$COD), 
                aes(label = stabbrs), check_overlap = T, size = 2.5) +
      theme_minimal() +  theme(axis.text.x = element_text(angle = 40)) +
      geom_hline(yintercept = 0)
    
    if(input$pop_model == "Aggregated"){
      plot <- plot + geom_line(data = contrib.data.react(), aes(x = year, y = fitted.RE - RE.estimates), col = "black", lty = 2)
    }else if(input$pop_model == "State-specific"){
      plot <- plot + geom_line(data = contrib.data.react(), aes(x = year, y = fitted.RE, group = state, col = state),  lty = 3)      
    }
    
    return(ggplotly(plot))
  })
  
  
  map.contribution <- reactive({
    plot <- ggplot(contrib.data.react(), aes(x = year, y = y1)) +
      geom_ribbon(aes(ymin = y1_lcl, ymax = y1_ucl), fill = "grey", alpha = 0.5) +
      geom_line(aes(col = Census_Division)) + 
      geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 1969)) +
      facet_wrap(~stabbrs.map.order, ncol = 11, drop = F) +
      theme_classic(base_size = 10) +
      theme(axis.text.x = element_blank(),
            strip.background=element_blank(),
            axis.line=element_blank(),
            axis.ticks=element_blank()) +
      ylab("Contribution to the LE gap") +
      xlab(paste0("Year (", input$years_LEgap[1], "-", input$years_LEgap[2], ")"))
    
    if(input$pop_model == "Aggregated"){
      plot <- plot + geom_line(data = contrib.data.react(), aes(x = year, y = fitted.RE - RE.estimates), col = "black", lty = 2)
    }else if(input$pop_model == "State-specific"){
      plot <- plot + geom_line(data = contrib.data.react(), aes(x = year, y = fitted.RE), col = "black", lty = 3)        
    }    
           
    interactive.plot2 <- ggplotly(plot)
    
    for(i in 1:length(interactive.plot2$x$data)){
      if (interactive.plot2$x$data[[i]]$line$color == "rgba(0,0,0,1)") {
        interactive.plot2$x$data[[i]]$hoverinfo <- "none"
        interactive.plot2$x$data[[i]]$line$width <- 1

      }
      
      interactive.plot2$x$data[[i]]$text <- gsub("Census_Division", "Census Division", interactive.plot2$x$data[[i]]$text)
      interactive.plot2$x$data[[i]]$text <- gsub("y1_ucl", "Upper credible limit", interactive.plot2$x$data[[i]]$text)
      interactive.plot2$x$data[[i]]$text <- gsub("y1_lcl", "Lower credible limit", interactive.plot2$x$data[[i]]$text)
      interactive.plot2$x$data[[i]]$text <- gsub("y1", "Contribution to gap", interactive.plot2$x$data[[i]]$text)
    }
  
    interactive.plot2
  })
  
  plot.chosen <- reactive({
    plot <- switch(input$plot_choice,
                   "Map" = map.contribution(), 
                   "Grid" = grid.contribution())
    plot
  })
  
  
  output$contribution_plot <- renderPlotly({
    plot.chosen()
  })
  

  ##########################################
  ##     Mortality rates                  ##
  ########################################## 
  
  mortality_plot1 <- reactive({
    
    if(input$plot_choice == "Map"){
      plot <- ggplotly(ggplot(subset(mortality.rates, sex == input$sex & COD == input$COD & year >= input$years_LEgap[1] & year <= input$years_LEgap[2]), 
                              aes(x = year, y = rate.per.100k_mean)) + 
                         geom_ribbon(aes(ymin = rate.per.100k_lcl, ymax = rate.per.100k_ucl, group = Race), fill = "grey", col = NA, alpha = 0.5) +
                         geom_line(aes(col = Race)) +
                         facet_wrap( ~ stabbrs.map.order, ncol = 11, drop = F) +
                         xlab(paste0("Year (", input$years_LEgap[1], "-", input$years_LEgap[2], ")")) +
                         ylab("Age-standardized mortality rate (per 100,000)") + 
                         geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 1969)) +  
                         ggtitle(paste0("Age-adjusted mortality (per 100,000) in ", lowercase.sex(), "s for ", lowercase.COD())) +
                         theme_classic(base_size = 10) +
                         theme(axis.text.x = element_blank(),
                               strip.background=element_blank(),
                               axis.line=element_blank(),
                               axis.ticks=element_blank())) %>% layout(legend = list(x = 0.5, y = 0.95))
    }else{ #grid
      plot <- ggplotly(ggplot(subset(mortality.rates, sex == input$sex & COD == input$COD & year >= input$years_LEgap[1] & year <= input$years_LEgap[2]), 
                              aes(x = year, y = rate.per.100k_mean)) + 
                         #geom_ribbon(aes(ymin = rate.per.100k_lcl, ymax = rate.per.100k_ucl, group = interaction(Race, state), fill = state), col = NA, alpha = 0.5) +
                         geom_line(aes(col = state, lty = Race)) +
                         facet_grid(Census_Region ~ Race) +
                         xlab(paste0("Year (", input$years_LEgap[1], "-", input$years_LEgap[2], ")")) + 
                         ylab("Age-standardized mortality rate (per 100,000)") + 
                         geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 1969)) +  
                         ggtitle(paste0("Age-adjusted mortality (per 100,000) in ", lowercase.sex(), "s for ", lowercase.COD())) +
                         theme_classic(base_size = 10) +
                         theme(axis.text.x = element_blank(),
                               strip.background=element_blank(),
                               axis.line=element_blank(),
                               axis.ticks=element_blank())) 
    }
    
    for(i in 1:length(plot$x$data)){
      if (plot$x$data[[i]]$line$color == "rgba(0,0,0,1)") {
        plot$x$data[[i]]$hoverinfo <- "none"
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
                         geom_ribbon(aes(ymin = rate.difference_LCL, ymax = rate.difference_UCL), fill = "grey", col = NA, alpha = 0.5) +
                         geom_line(aes(col = Census_Division)) +
                         facet_wrap( ~ stabbrs.map.order, ncol = 11, drop = F) +
                         xlab(" ") + 
                         ylab("Excess mortality among blacks (per 100,000)") + 
                         geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 1969)) +  
                         ggtitle(paste0("Age-adjusted mortality (per 100,000) in ", lowercase.sex(), "s for ", lowercase.COD())) +
                         theme_classic(base_size = 10) +
                         theme(axis.text.x = element_blank(),
                               strip.background=element_blank(),
                               axis.line=element_blank(),
                               axis.ticks=element_blank()))
    }else { #grid
      plot <- ggplotly(ggplot(subset(mortality.rates.diff, sex == input$sex & COD == input$COD & year >= input$years_LEgap[1] & year <= input$years_LEgap[2]), 
                              aes(x = year, y = rate.difference_mean)) + 
                         #geom_ribbon(aes(ymin = rate.difference_LCL, ymax = rate.difference_UCL, group = state), fill = "grey", col = NA, alpha = 0.5) +
                         geom_line(aes(col = state)) +
                         facet_wrap( ~ Census_Division) +
                         xlab(" ") + 
                         ylab("Excess mortality among blacks (per 100,000)") + 
                         geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 1969)) +  
                         ggtitle(paste0("Age-adjusted mortality (per 100,000) in ", lowercase.sex(), "s for ", lowercase.COD())) +
                         theme_classic(base_size = 10) +
                         theme(axis.text.x = element_blank(),
                               strip.background=element_blank(),
                               axis.line=element_blank(),
                               axis.ticks=element_blank()))     
    }
    
    for(i in 1:length(plot$x$data)){
      if (plot$x$data[[i]]$line$color == "rgba(0,0,0,1)") {
        plot$x$data[[i]]$hoverinfo <- "none"
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
                      "Excess risk" = mortality_plot2())
    return(plot.mort)
  })
  
  output$mortality_plot <- renderPlotly({plot.chosen.mortality()})
  
  
  ##########################################
  ##     State snapshot: COD              ##
  ########################################## 
  
  output$description_cod_summary <- renderUI({ 
           HTML(paste0("<b>Which causes of death contributed most to the ", lowercase.sex()," life expectancy gap 
                                                      between blacks and whites in ", input$year, "?</b>",
                      "<br/><br/>", "This graph depicts the difference in life expectancy between white ", lowercase.sex(), 
                             "s (vertical black line) and black ", lowercase.sex(), "s (dashed black line). 
                             Causes to the left of the dashed line narrow the gap in ", input$year, " 
                             whereas causesto the right exacerbate it. Use these buttons to change the gender or year being examined.<br/>"))
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
                                y = state.reorder2.n, yend = state.reorder2.n)) + 
               
               geom_rect(aes(xmin = new.start, 
                             ymin = state.reorder2.n - 0.45, 
                             ymax = state.reorder2.n + 0.45, 
                             xmax = new.finish, fill = COD), color = "white") +
               scale_y_continuous(breaks = 1:length(levels(factor(temp.df()$state.reorder2))), 
                                  labels = levels(factor(temp.df()$state.reorder2))) +
               theme_minimal() + 
               geom_segment(aes(x = LE_white_mean, xend = LE_white_mean, y = state.reorder2.n - 0.5, yend = state.reorder2.n + 0.5)) +
               geom_segment(aes(x = LE_black_mean, xend = LE_black_mean, y = state.reorder2.n - 0.5, yend = state.reorder2.n + 0.5), lty = 3) 
    )
    
    interactive.plot %>% 
      layout(xaxis = list(title = "Life expectancy gap (years)"), yaxis = list(title = NA, autorange = "reversed"))
    
    
  })
  
  #output$data.temp <- renderDataTable(temp.df())

  ##########################################
  ##          State snapshot: Age         ##
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
                                                      the gap in ", input$year, " whereas ages to the right exacerbate it."))
  })

  output$state_age_summary <- renderPlotly({
    ggplotly(ggplot(temp.df2(), 
                    aes(x = new.start, y = state.reorder2.n)) + 
               geom_segment(aes(x = LE_black_mean, xend = LE_white_mean, 
                                y = state.reorder2.n, yend = state.reorder2.n), col = "red") + 
               
               geom_rect(aes(xmin = new.start, 
                             ymin = state.reorder2.n - 0.45, 
                             ymax = state.reorder2.n + 0.45, 
                             xmax = new.finish, fill = age), color = "white") +
               scale_y_continuous(breaks = 1:length(levels(factor(temp.df()$state.reorder2))), 
                                  labels = levels(factor(temp.df()$state.reorder2))) +
               theme_minimal() + 
               geom_segment(aes(x = LE_white_mean, xend = LE_white_mean, y = state.reorder2.n - 0.5, yend = state.reorder2.n + 0.5), col = "red") +
               geom_segment(aes(x = LE_black_mean, xend = LE_black_mean, y = state.reorder2.n - 0.5, yend = state.reorder2.n + 0.5), col = "red", lty = 3) +
               scale_fill_viridis(discrete = T, direction = -1)
             
    )  %>% 
      layout(xaxis = list(title = "Life expectancy gap (years)"), yaxis = list(title = NA, autorange = "reversed"))
  })
  
  
  ##########################################
  ##             State dashboard          ##
  ##########################################

  output$description_state_snapshot <- renderUI({
    HTML(paste0("<b>", input$dashboard_state,"</b><br/>This dashboard brings together key metrics for ", input$dashboard_state, "."))
  })
  
  facet_names <- list(
    'Male'="Male Life Expectancy (years)",
    'Female'="Female Life Expectancy (years)"
  )
  
  output$life_expectancy <- renderPlotly({
    
    p1 <- ggplotly(ggplot(subset(BlackWhite_results, state == input$state), aes(x = year, y = LE_white_mean)) +
                     geom_line(col = "black") + 
                     geom_ribbon(aes(ymin = LE_white_lcl, ymax = LE_white_ucl, fill = sex), alpha = 0.3) +
                     geom_line(aes(y = LE_black_mean), lty = 2, col = "black") + 
                     geom_ribbon(aes(ymin = LE_black_lcl, ymax = LE_black_ucl, fill = sex), alpha = 0.3) +
                     #scale_color_manual(values = c("#67a9cf", "#ef8a62", "black")) +
                     facet_grid(. ~ sex, labeller = as_labeller(facet_names)) +
                     #scale_x_continuous(name = "Year") + 
                     #scale_y_continuous(name = "Life expectancy at birth (years)") + 
                     geom_text(data = subset(BlackWhite_results, year == 2013 & state == input$state),
                               aes(y = LE_white_mean - 1),
                               label = "White", check_overlap = T, size = 2.5) +
                     geom_text(data = subset(BlackWhite_results, year == 2013 & state == input$state),
                               aes(y = LE_black_mean - 1),
                               label = "Black", check_overlap = T, size = 2.5) +
                     theme_minimal() + ggtitle(paste0("Trends in life expectancy in ", input$state))
    ) %>% 
      layout(yaxis = list(title = "Life expectancy"))
    
    p1
    
    p2 <- ggplotly(ggplot(subset(BlackWhite_results, state == input$state), aes(x = year, y = LE_wbgap_mean)) + 
                     geom_ribbon(aes(ymin = LE_wbgap_lcl, ymax = LE_wbgap_ucl, fill = sex)) +
                     geom_line(col = "black", lty = 3) +
                     facet_grid(. ~ sex) + #scale_x_continuous(name = "Year") + 
                     expand_limits(y = 0) +
                     geom_hline(yintercept = 0, lwd = 0.5) + 
                     theme_minimal() + theme(legend.title=element_blank(), strip.text.x = element_blank())) %>% 
      layout(yaxis = list(title = "Difference"), xaxis = list(title = "Year"))
    
    subplot(p1, p2, shareX = T, nrows = 2, titleY = T) %>% layout(margin = list(t=50, b=50))
  })
  
  white.y1 <- reactive({
    round(BlackWhite_results %>% 
            filter(state == input$state, year == input$year1, sex == "Male") %>% 
            select(LE_white_mean), 1)
  })
  
  white.y2 <- reactive({
    round(BlackWhite_results %>% 
            filter(state == input$state, year == input$year2, sex == "Male") %>% 
            select(LE_white_mean), 1)
  })
  
  black.y1 <- reactive({
    round(BlackWhite_results %>% 
            filter(state == input$state, year == input$year1, sex == "Male") %>% 
            select(LE_black_mean), 1)
  })
  
  black.y2 <- reactive({
    round(BlackWhite_results %>% 
            filter(state == input$state, year == input$year2, sex == "Male") %>% 
            select(LE_black_mean), 1)
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
  round(BlackWhite_results %>% 
          filter(state == input$state, year == input$year1, sex == "Female") %>% 
          select(LE_white_mean), 1)
})

fwhite.y2 <- reactive({
  round(BlackWhite_results %>% 
          filter(state == input$state, year == input$year2, sex == "Female") %>% 
          select(LE_white_mean), 1)
})

fblack.y1 <- reactive({
  round(BlackWhite_results %>% 
          filter(state == input$state, year == input$year1, sex == "Female") %>% 
          select(LE_black_mean), 1)
})

fblack.y2 <- reactive({
  round(BlackWhite_results %>% 
          filter(state == input$state, year == input$year2, sex == "Female") %>% 
          select(LE_black_mean), 1)
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
##            State Snapshot            ##
##########################################

output$population_trend <- renderPlotly({
  p <- ggplotly(
    ggplot(subset(dat.aggregated, state == input$state & age_minbin == 0), aes(y = pop_across_age, x = year)) + 
      geom_line(aes(col = sex, lty = race)) +
      #scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
      ylab("Population Size") + 
      scale_y_continuous(label = comma) + 
      scale_x_continuous(name = "Year") + 
      theme_minimal()) 
  p # %>% layout(legend = list(x = 0.5, y = -2))
  #p %>% layout(showlegend = F)
  
})

output$age_cod1 <- renderPlot({
  age_cod_plot <- ggplot(data = subset(age_cod_results, sex == input$selected_sex & year == input$year1 & 
                                         state == input$state), 
                         aes(x=age, y=age_COD_cont_yrs_mean, fill=COD)) + 
                         geom_bar(stat = "identity", col = "white") + coord_flip() + theme_minimal() +
                         ylab("Contribution to the life expectancy gap (years)") +
                         xlab("")
  age_cod_plot
})

output$age_cod2 <- renderPlotly({
  ggplotly(age_cod_plot)
})


##########################################
##          More information            ##
########################################## 

output$app_description <- renderUI({
  HTML(paste0("<b>The data</b><br/>Data for this app is publicly available and accessed using the NCI's SEER*stat software. 
              We extracted mortality counts within strata of state, sex, year, race, age group, and cause of death.
              There data were smoothed over time using an autoreggresive Bayesian model and suppressed counts (between 
              1 and 10) were imputed using truncated Poisson regression.<br/><br/>
              
              <b>Who we are</b><br/> 
              We are a group of epidemiologists from McGill University in Montreal, Canada. 
              These efforts were led by <b>Corinne Riddell</b>. She collected the data, coded and 
              performed demographic analyses (life tables, cause of death decompositions), and 
              created this shiny app. <b>Kathryn Morrison</b> is our resident Bayesian,
              and she coded up the Bayesian model to calculate smoothed mortality rates and their 
              credible intervals. This work was massively inspired by previous work of our mentors
              and co-authors, <b>Sam Harper</b> and <b>Jay Kaufman</b>. They provided mentorship, 
              guidance, and boat-loads of background reading.<br/><br/> 
              
              <b>Many thanks</b><br/> ...to the folks at RStudio for the creation and maintenance of ggplot2 and shiny, 
              our friends at Plotly, whose interactive plots are top-notch, and, JAGS maintainer Martyn 
              Plummer for making JAGS easy to use in R!<br/><br/>
 
              <b>License</b><br/> This software created by Corinne Riddell is licensed under a Creative Commons Attribution-Noncommercial 4.0 International License.
              "
              
  ))
})

}
shinyApp(ui = ui1, server = server)