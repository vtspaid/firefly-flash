### Code for creating shiny app that calculates statistics for firefly flashes ###

library(shiny)
library(tuneR)


FLASH = readWave(paste0(getwd(),"/Dunkard_potomaca.WAV"))

ui <- fluidPage(
  # App title ----
  titlePanel("Firefly Flash Statistics"),
  
  # Sidebar layout with input and output definitions ----
    sidebarPanel(
      tabsetPanel(
        tabPanel("plot times",
                 numericInput("start", "plot start time", value = 8, min = 1, max = 10000),
                 numericInput("end", "plot end time", value = 38, min = 1, max = 10000)),
        tabPanel("Flash calculations",
                 numericInput("tstart", "start time", value = 8, min = 1, max = 10000),
                 numericInput("tend", "end time", value = 38, min = 1, max = 10000),
                 textInput("species", "species", value = "", width = NULL, placeholder = NULL),
                 numericInput("sample", "sample #", value = 1, min=1, max = 100000),
                 textInput("site", "site name", value = "", width = NULL, placeholder = NULL),
                 numericInput("temp", "sample #", value = 1, min=1, max = 100000))
        
        
      )),
    
    # Main panel for displaying outputs ----
    mainPanel( 
      # Output: Histogram ----
      plotOutput(outputId = "flashplot"),

      tableOutput(outputId = "flash_stats") )
      
    )
  


# Define server logic required to draw a histogram ----
server <- function(input, output, output2) {
 
  
  output$flashplot <- renderPlot({
  train_audio = FLASH
  str(train_audio)
  s1 <- train_audio/2^(train_audio@bit -1)
  timeArray <- (0:(length(train_audio@left)-1)) / train_audio@samp.rate
  #Plot the wave
  plot(x=timeArray, y=train_audio@left, type='l',
       col='black', xlab='Seconds', ylab='Amplitude', xlim=c(input$start, input$end))
    
  })
  
  output$flash_stats <- renderTable({
    singleflash(FLASH, start=input$tstart, end=input$tend, species=input$species, sample=input$sample, 
                site=input$site, temp=input$temp)})
  
  
}

shinyApp(ui = ui, server = server)
