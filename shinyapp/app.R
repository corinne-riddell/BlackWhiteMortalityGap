library(shiny)
library(plotly)
library(viridis)
library(scales)
#
#load("/Users/corinneriddell/Dropbox/BlackWhiteGap/Data/Sep6_BWgap.Rdata")
load("/Users/corinneriddell/Dropbox/BlackWhiteGap/Data/alabama_only.Rdata")

dat.clean.alabama$id_cod <- as.integer(dat.clean.alabama$COD2)

ui1 <- fluidPage("Explore cause of death by age and year in the United States",
                 
                 # *Input() functions,
                 selectInput(inputId = "sex", label = "Select a sex", choices = unique(levels(dat.clean.alabama$Sex2))),
                 selectInput(inputId = "race", label = "Select a race", choices = unique(levels(dat.clean.alabama$Race2))),
                 selectInput(inputId = "cod", label = "Select a cause of death", choices = unique(levels(dat.clean.alabama$COD2))),
                 #                selectInput(inputId = state1, label = "Select a state", choices = dat.clean$State2)
                 #*Output() functions,
                 plotlyOutput("heatmapCOD"),
                 plotlyOutput("heatmapsCOD6")
                 
)

#need them to share a scale - 0 - 90% say
#need the panels to be named

server <- function(input, output) {
  
  output$heatmapCOD <- renderPlotly({
    plot_ly(data = subset(dat.clean.alabama, Race2 == input$race & Sex2 == input$sex & COD2 == input$cod),
            x = Year3, y = Age3, z = cod_prop_death5, type = "heatmap") %>% 
      layout(xaxis = list(title = "Year"), yaxis = list(title = "Age (years)"))
  })
  
  output$heatmapsCOD6 <- renderPlotly({  
    cods <- unique(levels(dat.clean.alabama$COD2))
#    vals <- unique(dat.clean.alabama$cod_prop_death5)
#    o <- order(vals, decreasing = FALSE)
 #   cols <- scales::col_numeric("Blues", domain = NULL)(vals)
 #   colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    plots <- lapply(cods, function(cod){
      plot_ly(subset(dat.clean.alabama, Race2 == input$race & Sex2 == input$sex & COD2 == cod),
              x = Year3, y = Age3, z = cod_prop_death5, type = "heatmap")#, colorscale = colz)
    })
    subplot(plots)
  })
  
  
  
}

shinyApp(ui = ui1, server = server)
