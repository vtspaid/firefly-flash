
ViewerUI <- function(id) {
  list(
    h3("Sound File Plot"),
    plotOutput(NS(id, "flashplot"))
  )
}


ViewerServer <- function(id, flash) {
  moduleServer(id, function(input, output, session) {
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
  })
}
