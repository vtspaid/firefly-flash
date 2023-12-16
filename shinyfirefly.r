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
                 numericInput("start", "plot start time", value = 0, min = 1, max = length(FLASH@left)/FLASH@samp.rate),
                 numericInput("end", "plot end time", value = length(FLASH@left)/FLASH@samp.rate, min = 1, max = 42),
                  actionButton("AUDIO", "Play recording")),
        tabPanel("Flash calculations",
                 numericInput("tstart", "start time", value = 0, min = 1, max = length(FLASH@left)/FLASH@samp.rate), # start time of recording to use
                 numericInput("tend", "end time", value = length(FLASH@left)/FLASH@samp.rate, min = 1, max = length(FLASH@left)/FLASH@samp.rate), # end time of recording to use
                 textInput("species", "species", value = "", width = NULL, placeholder = NULL), # set species
                 numericInput("sample", "sample #", value = 1, min=1, max = 100000), # set sample number
                 textInput("site", "site name", value = "", width = NULL, placeholder = NULL), # set site
                 numericInput("temp", "Temperature", value = "", min=1, max = 100000)) # set temp
        
        
      )),
    
    # Main panel for displaying outputs ----
    mainPanel( 
      # Output: Histogram ----
      p("This plot is controlled by the 'plot times' tab on the left. It is purely for visulization of the audio"),
      plotOutput(outputId = "flashplot"),

      p("This is the output you get using the start and end times selected in the 'flash calculations' tab on the left.
      You can supply the start and end times for the calculations, and the species, site and temperature in case you 
      want to copy and paste the results into a spreadsheet"),
      tableOutput(outputId = "flash_stats"),
      
      br(),
      br(),
      p("This plot is of the audio used in the flash calculations, the red lines are where the r function believes a 
         flash occured"),
      plotOutput(outputId = "resultsplot"))
      
    )
  


# Define server logic required to draw a histogram ----
server <- function(input, output, output2, session) {
 
  
  output$flashplot <- renderPlot({
  train_audio = FLASH
  str(train_audio)
  s1 <- train_audio/2^(train_audio@bit -1)
  timeArray <- (0:(length(train_audio@left)-1)) / train_audio@samp.rate
  #Plot the wave
  plot(x=timeArray, y=train_audio@left, type='l',
       col='black', xlab='Seconds', ylab='Amplitude', xlim=c(input$start, input$end))
    
  })
  
  output$resultsplot <- renderPlot({flashcheck(FLASH, start=input$tstart, end=input$tend)})
  
  
  output$flash_stats <- renderTable({
    singleflash(FLASH, start=input$tstart, end=input$tend, species=input$species, sample=input$sample, 
                site=input$site, temp=input$temp)})
  

observeEvent(input$AUDIO, {
   insertUI(selector = "#AUDIO",
            where = "afterEnd",
           ui = play( Wave(FLASH@left[c(input$start*FLASH@samp.rate):c(input$end*FLASH@samp.rate)], 
                           FLASH@right[c(input$start*FLASH@samp.rate):c(input$end*FLASH@samp.rate)],
                           FLASH@samp.rate, bit = FLASH@bit, pcm = TRUE)))
 })

}

shinyApp(ui = ui, server = server)

tags$audio(src = "dunkard_potomaca.wav", type = "audio/wav", autoplay = NA, controls = NA)
