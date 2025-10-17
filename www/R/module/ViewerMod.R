
ViewerUI <- function(id) {
  list(
    h3("Sound File Plot"),
    plotOutput(NS(id, "flashplot"), height = "300px")
  )
}


ViewerServer <- function(id, flash) {
  moduleServer(id, function(input, output, session) {
    output$flashplot <- renderPlot({
      req(flash())
        train_audio <- flash()$audio
        timeArray <- (0:(length(train_audio@left) - 1)) / train_audio@samp.rate
        # Plot the wave
        par(mar = c(4, 4, 1, 0))
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
