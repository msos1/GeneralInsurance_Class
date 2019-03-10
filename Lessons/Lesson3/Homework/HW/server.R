library(shiny)
library(ggplot2)

function(input, output) {
  
  output$gg <- renderPlot({

    # setwd('C:\\Users\\Matej\\Documents\\github\\GeneralInsurance_Class\\Lessons\\Lesson3\\Homework\\HW')
    dt_KPI <- read.csv("lesson3_KPI.csv")
    ggplot(data = dt_KPI, mapping = aes(x = Premium, y = Expenses, colour = factor(dt_KPI[,input$filter]))) +
         geom_point() +
         geom_smooth() +
         labs(title = input$filter, color = input$filter)
  })
}