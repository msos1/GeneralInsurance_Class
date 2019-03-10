library(shiny)


ui <- fluidPage('Hello World',
                sliderInput(inputId = 'num',
                            label = 'Choose a number',
                            value = 50, min = 1, max = 100),
                plotOutput('hist'),
                plotOutput('gg'),
                tableOutput('table'))

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
  output$table <- renderTable({
    dt_KPI <- read.csv("C:\\Users\\Matej\\Documents\\github\\GeneralInsurance_Class\\Data\\lesson2_KPI.csv")
  })
  output$gg <- renderPlot({
    dt_KPI <- read.csv("C:\\Users\\Matej\\Documents\\github\\GeneralInsurance_Class\\Data\\lesson2_KPI.csv")
    ggplot(data = dt_KPI, mapping = aes(x = Premium, y = Expenses, colour = Region)) +
      geom_point() +
      geom_smooth()
  })
}

shinyApp(ui = ui, server = server)
