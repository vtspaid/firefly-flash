
ExampleUI <- function(id) {
  ns <- NS(id)
  list(
    h3("Choose a flash category to explore"),
    fluidRow(column(4,
      
    radioButtons(ns("flashtype"),
                 "", 
                 choices = c("single flash", "complex flash", "glow"),
                 inline = TRUE),
    uiOutput(ns("audioplayer")),
    ), # End of first column
    column(8, uiOutput(ns("instructions"))), # End of second column
    ), # End of fluidRow
    plotOutput(ns("flashplot"), height = "300px"),
    fluidRow(column(1),
             column(11,  
                    div(class = "duration_slider",
                        sliderInput(ns("duration"), label = NULL,
                                    min = 0, max = 100, value = c(0, 100),
                                    widt = "100%"))))
    )
}


ExampleServer <- function(id, input2) {
  moduleServer(id, function(input, output, session) {
    flash <- reactive({
      req(input$flashtype)
      if (input$flashtype == "single flash") {
        infile <- "www/example single flash recording.WAV"
        output$instructions <- renderUI(HTML(single_flash_example))
      } else if (input$flashtype == "complex flash") {
        infile <- "www/example_complex_flash.wav"
        output$instructions <- renderUI(HTML(complex_flash_example))
      } else {
        infile <- "www/example slow glow flash.wav"
        output$instructions <- renderUI(HTML(glow_flash_example))
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
      updateNumericInput(session, "duration", 
                         value = c(0, duration), min = 0, max = duration)
      
      data <- list(file = infile,
                   audio = audio)
      print("ending infile server")
      data
    })
    
    
    output$flashplot <- renderPlot({
      req(flash())
      train_audio <- flash()$audio
      timeArray <- (0:(length(train_audio@left) - 1)) / train_audio@samp.rate
      # Plot the wave
      flash_plot(timeArray, train_audio@left, input$duration)
    })
    
    flash
  })
  
}
