
ViewPlotUI <- function(id) {
  list (
    numericInput(NS(id, "start"), "plot start time", value = 0, min = 0, max = 10000),
    numericInput(NS(id, "end"), "plot end time", value = 10, min = 1, max = 10000),
    actionButton(NS(id, "plotaudio"), "Plot audio"),
    
    actionButton(NS(id, "audio"), "Play audio"),
    actionButton(NS(id, "clearaudio"), "Remove audio player")
  )
}


ViewPlotServer <- function(id, FLASH, infile) {
  moduleServer(id, function(input, output, session) {
    # Play the sound
    observeEvent(input$audio, {
      print("starting viewplot Server")
      req(FLASH())
      req(infile)
      print("did we get this far")
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
  })
}