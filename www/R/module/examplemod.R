
ExampleUI <- function(id) {
  ns <- NS(id)
  list(
    h3("Choose a flash category to explore"),
    fluidRow(column(6,
      
    radioButtons(ns("flashtype"),
                 "", 
                 choices = c("single flash", "complex flash", "glow"),
                 inline = TRUE),
    uiOutput(ns("audioplayer")),
                    ), # End of first column
    column(6, HTML(single_flash_example)), # End of second column
    ), # End of fluidRow
    plotOutput(ns("flashplot")),
    p("Set the plot start and end times to 0 and 32 respectively. Then do the
      same for the calculation start and end times and hit \"Run Flash 
      Calculations\"")
  )
}


ExampleServer <- function(id, input2) {
  moduleServer(id, function(input, output, session) {
    flash <- reactive({
      req(input$flashtype)
      if (input$flashtype == "single flash") {
        infile <- "www/example single flash recording.WAV"
      } else if (input$flashtype == "complex flash") {
        infile <- "www/example_complex_flash.wav"
      } else {
        infile <- "www/example slow glow flash.wav"
      }
      print(infile)
      
      output$audioplayer <- renderUI({
        tags$audio(src = flash()$file, type = "audio/wav", controls = NA)
      })
      
      audio <- readWave(infile)
      
      audio <- Wave(left = audio@left[seq(1, length(audio@left), by = 4)],
                    samp.rate = audio@samp.rate / 4, 
                    bit = 16)
      
      # Update max length of file
      duration <- length(audio@left) / audio@samp.rate
      updateNumericInput(session, "controls-end_m", 
                         value = duration, max = duration)
      
      data <- list(file = infile,
                   audio = audio,
                   start = input2[["controls-start_m"]],
                   end = input2[["controls-end_m"]])
      print("ending infile server")
      data
    })
    
    
    output$flashplot <- renderPlot({
      req(flash())
      train_audio <- flash()$audio
      timeArray <- (0:(length(train_audio@left) - 1)) / train_audio@samp.rate
      # Plot the wave
      plot(x = timeArray, 
           y = train_audio@left,
           type = "l",
           col = "black",
           xlab = "Seconds",
           ylab = "Amplitude",
           xlim = c(flash()$start, flash()$end))
    })
    flash
  })
  
}
