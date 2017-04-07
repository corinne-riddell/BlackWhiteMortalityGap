library(shiny)
library(plotly)
library(viridis)
library(scales)
library(shinythemes)
library(dplyr)

source("/Users/corinneriddell/Documents/repos/BlackWhiteMortalityGap/Code/life_expectancy_functions.R")

age_cod_results_female <- read.csv("/Users/corinneriddell/Documents/repos/BlackWhiteMortalityGap/Results/age_cod_results_female.csv")
age_cod_results_male <- read.csv("/Users/corinneriddell/Documents/repos/BlackWhiteMortalityGap/Results/age_cod_results_male.csv")
age_cod_results <- rbind(age_cod_results_female, age_cod_results_male)
rm(age_cod_results_female, age_cod_results_male)

age_decomp_results <- read.csv("/Users/corinneriddell/Documents/repos/BlackWhiteMortalityGap/Results/age_decomp_results.csv")
cod_decomp_results <- read.csv("/Users/corinneriddell/Documents/repos/BlackWhiteMortalityGap/Results/cod_decomp_results.csv")
cod_change_results <- read.csv("/Users/corinneriddell/Documents/repos/BlackWhiteMortalityGap/Results/cod_change_results.csv")
BlackWhite_results <- read.csv("/Users/corinneriddell/Documents/repos/BlackWhiteMortalityGap/Results/BlackWhite_results.csv")
dat.aggregated <- read.csv('/Users/corinneriddell/Dropbox/BlackWhiteGap/Data/dat_aggregated.csv') 

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




ui1 <- fluidPage(theme = shinytheme("cosmo"),
                 #shinythemes::themeSelector(), 
                 titlePanel("Explore the black-white life expectancy gap in the United States"),
                 
                 sidebarLayout(
                   sidebarPanel(width = 2,
                                strong("Select year"),
                                #tags$style(type="text/css", ".form-group.shiny-input-container{ display: inline-block } strong{ display: block !important  } img{ margin-bottom: 0px }"),
                                selectInput(inputId = "year1", label = NA, 
                                            choices = unique(BlackWhite_results$year), width = 100),
                                selectInput(inputId = "year2", label = NA, 
                                            choices = unique(BlackWhite_results$year), width = 100, selected = "2013"),
                                strong("Select sex"),
                                radioButtons(inputId = "selected_sex", label = NA, 
                                             inline = T, choices = c("Male", "Female"), selected = "Male"),
                                selectInput(inputId = "state", label = "Select a state", width = 200, choices = levels(BlackWhite_results$state)),
                                strong("Display contribution in"),
                                radioButtons(inputId = "COD", label = NA, inline = T,
                                             choices = levels(cod_decomp_results$COD)),
                                radioButtons(inputId = "contribution_type", label = NA, 
                                             inline = T, choices = c("Years", "Proportion (%)"), selected = "Years")
                              
                                
                   ),
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("State summary: COD",
                                textOutput("description_cod_summary"),
                                plotlyOutput("state_cod_summary", height = 800)),
                                #dataTableOutput("data.temp")),
                       
                       tabPanel("State summary: Age",
                                textOutput("description_age_summary"),
                                plotlyOutput("state_age_summary", height = 800),
                                dataTableOutput("data.temp2")),
                    
                       tabPanel("Trends in COD",
                                strong("How has each cause of death contributed to the difference in life expectancy over time?
                                       First, here is a graph of the life expectancy gap over time, where each line is a state:"),
                                plotlyOutput("le_gap_all_states", height = 700, width = 1100), 
                                strong("Select the cause of death you're interested in to see how many years of the total gap
                                       is due the selected cause, and how this changed over time. You can also view the proportional
                                       contribution by selecting 'Proportion (%)' on the panel to the left."),
                                radioButtons(inputId = "plot_choice", label = "Choose the plot style", 
                                             inline = T, choices = c("Arrange as map", "Arrange as grid")),
                                plotlyOutput("contribution_plot", height = 700, width = 1100)#,
                                #dataTableOutput("temp2")
                                ),
                       
                       tabPanel("Life Expectancy Gap",
                                plotlyOutput("life_expectancy"),
                                textOutput("Explain_LE_males"),
                                textOutput("Explain_LE_females")
                                ), #height = 300, width = 400
                       
                       tabPanel("State snapshot",
                                plotlyOutput("population_trend", height = 300, width = 500),
                                plotOutput("age_cod1"),
                                plotlyOutput("age_cod2"),
                                textOutput("Explain_Age_Gap"),
                                plotlyOutput("cod_bayes"),
                                textOutput("Explain_COD_Gap"))
                       
                     )
                   )
                 )
)

server <- function(input, output) {
 
  ##########################################
  ##                 COD tab              ##
  ########################################## 
  
  temp.df <- reactive({
    temp <- data.frame(subset(cod_decomp_results, sex == input$selected_sex & year == input$year1))
    temp["state.reorder2"] <- reorder(temp$state, temp$LE_black_mean, max, na.rm = T)
    temp["state.reorder2.n"] <- as.numeric(temp[["state.reorder2"]])
    temp
  })
  
  lowercase.sex <- reactive({
    sex <- switch(input$selected_sex,
                  "Male" = "male",
                  "Female" = "female")
  })
  
  output$description_cod_summary <- renderText(paste0("Which causes of death contributed most to the ", lowercase.sex()," life expectancy gap 
                                                      between blacks and whites in ", input$year1, "? This graph depicts the difference in life expectancy
                                                      between white ", lowercase.sex(), "s (vertical black line) and black ", lowercase.sex(), "s (dashed black 
                                                      line). Causes to the left of the dashed line narrow the gap in ", input$year1, " whereas causes
                                                      to the right exacerbate it."))
  
  output$state_cod_summary <- renderPlotly({
    ggplotly(ggplot(temp.df(), 
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
    )  %>% 
      layout(xaxis = list(title = "Life expectancy gap (years)"), yaxis = list(title = NA, autorange = "reversed"))
  })
  
  #output$data.temp <- renderDataTable(temp.df())

  ##########################################
  ##                 Age tab              ##
  ########################################## 
  
  temp.df2 <- reactive({
    temp <- data.frame(subset(age_decomp_results, sex == input$selected_sex & year == input$year1))
    temp["state.reorder2"] <- reorder(temp$state, temp$LE_black_mean, max, na.rm = T)
    temp["state.reorder2.n"] <- as.numeric(temp[["state.reorder2"]])
    temp
  })
  
  output$description_age_summary <- renderText(paste0(h1("Which age group contributed most to the ", lowercase.sex()," life expectancy gap 
                                                      between blacks and whites in ", input$year1, "?"), br(), "This graph depicts the difference in 
                                                      life expectancy between white ", lowercase.sex(), "s (vertical red line) and black ", 
                                                      lowercase.sex(), "s (dashed red line). Ages to the left of the dashed line narrow 
                                                      the gap in ", input$year1, " wherease ages to the right exacerbate it."))

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
  ##            Life expectancy tab       ##
  ##########################################

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
##            COD trends tab            ##
##########################################

output$le_gap_all_states <- renderPlotly({
  pb <- ggplotly(ggplot(data = subset(BlackWhite_results, sex == input$selected_sex), aes(y = LE_wbgap_mean, x = year)) + 
             geom_line(aes(col = state)) +
             facet_wrap(~ Census_Division) +
             ylab(" ") +
             xlab(" ") +
             geom_text(data = subset(BlackWhite_results, year == 2013 & sex == input$selected_sex), 
                       aes(label = stabbrs), check_overlap = T, size = 2.5) +
             theme_minimal() + theme(axis.text.x = element_text(angle = 45)) +
             geom_hline(yintercept = 0)
  ) %>% layout(margin = list(l=50, b=100), xaxis = list(title = "            Year"), 
               yaxis = list(title = "Life expectancy gap (years)"))
})

contrib.data.react <- reactive({
  temp <- data.frame(subset(cod_decomp_results, sex == input$selected_sex & COD == input$COD))
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
  ggplotly(ggplot(contrib.data.react(), aes(x = year, y = y1)) + 
             geom_line(aes(col = state)) + 
             facet_wrap(~ Census_Division) +
             ylab(paste0("Contribution to LE Gap", xaxis.title())) +
             xlab("Year") +
             geom_text(data = subset(contrib.data.react(), 
                                     year == 2013 & sex == input$selected_sex & COD == input$COD), 
                       aes(label = stabbrs), check_overlap = T, size = 2.5) +
             theme_minimal() +  theme(axis.text.x = element_text(angle = 40)) +
             geom_hline(yintercept = 0))
})


map.contribution <- reactive({
  ggplotly(ggplot(contrib.data.react(), aes(x = year, y = y1)) +
  geom_ribbon(aes(ymin = y1_lcl, ymax = y1_ucl, fill = Census_Division)) +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 1969)) +
  facet_wrap(~stabbrs.map.order, ncol = 11, drop = F) +
  theme_classic(base_size = 10) +
  theme(axis.text.x = element_blank(),
        strip.background=element_blank(),
        axis.line=element_blank(),
        axis.ticks=element_blank()) +
  ylab("Contribution to the LE gap") +
  xlab("Year (1969-2013)")
  )
})

# map2 <- reactive({
#   ggplot(contrib.data.react(), aes(x = year, y = y1)) + 
#   geom_area(aes(y = y1_for_area), fill = "#41b6c4") +
#   geom_ribbon(aes(ymin = y1_lcl, ymax = y1_ucl), fill = "#ffffb2", alpha = 0.5) +
#   geom_line() +
#   geom_hline(aes(yintercept = 0), col = "black") + 
#   geom_vline(aes(xintercept = 1969), col = "black") + 
#   facet_wrap(~stabbrs.map.order, ncol = 11, drop = F) +
#   theme_classic(base_size = 15) +
#   theme(axis.text.x = element_blank(),
#         strip.background=element_blank(),
#         axis.line=element_blank(),
#         axis.ticks=element_blank()) +
#   ylab("Contribution to the LE gap") +
#   xlab("Year (1969-2013)")  
# })


plot.chosen <- reactive({
  plot <- switch(input$plot_choice,
                "Arrange as map" = map.contribution(), 
                "Arrange as grid" = grid.contribution())
  plot
})


output$contribution_plot <- renderPlotly({
 plot.chosen()
})

#output$temp2 <- renderDataTable(contrib.data.react())

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

}
shinyApp(ui = ui1, server = server)