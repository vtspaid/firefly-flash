
ViewerUI <- function(id) {
  list(
    p("This plot is controlled by the 'plot times' tab on the left. 
      It is purely for visualization of the audio"),
    plotOutput(NS(id, "flashplot"))
  )
}


ViewerServer <- function(id, flash, input2, xlims) {
  moduleServer(id, function(input, output, session) {
    output$flashplot <- renderPlot({
      req(flash())
        print("what is the length")
        print(length(flash()$audio@left))
        train_audio <- flash()$audio
        timeArray <- (0:(length(train_audio@left) - 1)) / train_audio@samp.rate
        # Plot the wave
        plot(x = timeArray, 
             y = train_audio@left,
             type = "l",
             col = "black",
             xlab = "Seconds",
             ylab = "Amplitude",
             xlim = c(xlims()$start, xlims()$end))
    })
  })
}
