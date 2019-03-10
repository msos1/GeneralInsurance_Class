library(shiny)
library(ggplot2)

function(input, output) {
  
  output$gg <- renderPlot({

    #setwd('C:\\Users\\Matej\\Documents\\github\\GeneralInsurance_Class\\Lessons\\Lesson3\\Homework\\HW')
    dt_KPI <- read.csv("lesson3_KPI.csv")
    ggplot(data = dt_KPI, mapping = aes(x = Premium, y = Expenses, colour = factor(dt_KPI[,input$filter]))) +
         geom_point() +
         geom_smooth() +
         labs(title = input$filter, color = input$filter, caption = 'Data source: Zurich, GeneralInsurance_Class')
  })
}


# Some comments...:
# For this homework, I will not adjust/change anyhow our data (e.g. negative Expenses, negative Premium,
# missing values, NAs, ... because we are just plotting graphs and it won't make much difference at this point, and we are also not sure if there really are any mistakes in the data)

# Looking closely to specific years, when comparing Premium and Expenses, we can say that our Expenses per Premium are higher in last years than it was in Years 2012 and 2013.
# (What happened? Inflation and higher concurention? Less customers? ... Maybe it will be better in next years as we did something in last years...)