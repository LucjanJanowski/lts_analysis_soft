# File created to select sequences based on sa, ta, E, h with support from shiny 
# Created by Lucjan Janowski 30.06.2022


library(tidyverse)
library(shiny)

all_data = read_csv("si_ti_4ph.csv", col_types = "cnnf")

# plot for sa_ta
{
  ui<- shinyUI(
    fluidPage(
      plotOutput("grafica", hover="clickGrafica"), tableOutput("miverbatini")
    )
  )
  
  server<- shinyServer(
    function(input,output) { 
      output$grafica <- renderPlot(
        {
          ggplot(all_data, aes(x = si, y = ti, color = content_type)) + 
            geom_point() + 
            xlab("si") + 
            ylab("ti") 
        }
      )
      output$miverbatini <- renderTable(
        { 
          nearPoints(
            all_data, input$clickGrafica, threshold = 10
          ) 
        }
      ) 
    }
  ) 
  shinyApp(ui, server)
}
