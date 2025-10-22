
ViewerUI <- function(id) {
  list(
    h3("Sound File Plot"),
    plotOutput(NS(id, "flashplot"), height = "300px"),
    fluidRow(column(1),
             column(11,  
                    div(class = "duration_slider",
                        sliderInput(NS(id, "duration"), label = NULL,
                                    min = 0, max = 100, value = c(0, 100),
                                    widt = "100%"))))
  )
}


ViewerServer <- function(id, flash) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(flash(), {
      req(flash())
      duration <- length(flash()$audio@left) / flash()$audio@samp.rate
      updateNumericInput(session, "duration", 
                         value = c(0, duration), min = 0, max = duration)
    })
    
    output$flashplot <- renderPlot({
      req(flash())
        train_audio <- flash()$audio
        timeArray <- (0:(length(train_audio@left) - 1)) / train_audio@samp.rate
        # Plot the wave
        flash_plot(timeArray, train_audio@left)
    })
  })
}
