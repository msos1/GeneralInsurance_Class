
fluidPage(    
  
  # Give the page a title
  titlePanel("Scatter plot with colour"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("filter", "Filter:", 
                  choices=names(dt_KPI[,1:5])),
      hr(),
      helpText("Insurance data filter")
    ),
    
    # Create a spot for the plot
    mainPanel(
      plotOutput("gg")  
    )
    
  )
)
