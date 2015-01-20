library(shiny)


shinyUI(fluidPage(
  
  titlePanel("Histogram"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "n_breaks", label = "Number of bins:",
                  choices = c(10, 20, 35, 50), selected = 20),
      
      sliderInput(inputId = "bw_adjust", label = "Bandwidth adjustment:",
                  min = 0.2, max = 2, value = 1, step = 0.2,
                  animate = animationOptions(interval = 1000, loop = TRUE, 
                                             playButton = NULL, pauseButton = NULL))
    ),
    mainPanel(plotOutput("histo")
    )
  )
)
)
