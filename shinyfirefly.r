### Code for creating shiny app that calculates statistics for firefly flashes ###

library(shiny)

ui <- fluidPage(
  # App title ----
  titlePanel("Firefly Flash Statistics"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: 
      numericInput("start", "Start time", value = 8, min = 1, max = 10000),
      numericInput("end", "End time", value = 38, min = 1, max = 10000)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "flashplot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$flashplot <- renderPlot({FLASH <- paste0(getwd(),"/Dunkard_potomaca.WAV")
  train_audio = readWave(FLASH)
  str(train_audio)
  s1 <- train_audio/2^(train_audio@bit -1)
  timeArray <- (0:(length(train_audio@left)-1)) / train_audio@samp.rate
  #Plot the wave
  plot(x=timeArray, y=train_audio@left, type='l',
       col='black', xlab='Seconds', ylab='Amplitude', xlim=c(input$start, input$end))
    
  })
  
}

shinyApp(ui = ui, server = server)
