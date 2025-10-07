
ListenUI <- function(id) {
  list (
    numericInput(NS(id, "start"), "plot start time", value = 0, min = 0, max = 10000),
    numericInput(NS(id, "end"), "plot end time", value = 10, min = 1, max = 10000),
    actionButton(NS(id, "plotaudio"), "Plot audio"),
    
    actionButton(NS(id, "audio"), "Play audio"),
    actionButton(NS(id, "clearaudio"), "Remove audio player")
  )
}


ListenServer <- function(id, FLASH, input2) {
  moduleServer(id, function(input, output, session) {
    
    # Update max length of file
    observeEvent(input2[["GrabFile-inputfile"]], {
      print("is this running")
      req(FLASH())
      req(FLASH()$audio)
      duration <- length(FLASH()$audio@left) / FLASH()$audio@samp.rate
      updateNumericInput(session, "end", value = duration, max = duration)
     # updateNumericInput(session, "tend", value = duration, max = duration)
    })
    
    
    # Play the sound
    observeEvent(input$audio, {
      print("starting viewplot Server")
      req(FLASH())
      req(input2[["GrabFile-inputfile"]])
      print(FLASH()$file)
      base64 <- dataURI(file = FLASH()$file, mime = "audio/wav")
      insertUI(selector = paste0("#", id, "-audio"), where = 'afterEnd',
               ui = tags$div(id = "howleraudio", howler::howlerModuleUI(
                 id = "sound",
                 files = list("imported audio" = base64)
               )
               ))
    })
    
    # Remove the audio UI
    observeEvent(input$clearaudio, {
      removeUI( selector ="#howleraudio", immediate = T)
    })
  reactive(list(start = input$start, end = input$end))
  })
  
}