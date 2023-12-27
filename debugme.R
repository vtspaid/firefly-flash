library(shiny)
library(howler)
library(tuneR)
library(dplyr)
library(base64enc)

ui <- fluidPage(
  fileInput("file1", "Choose a .wav or .mp3 file",  accept = c(".wav", ".mp3")),

 tags$div(id = "removenow", howler::howlerBasicModuleUI(
    id = "rmv",
    files = list(
      "Winning Elevation" = "https://cdn.pixabay.com/download/audio/2022/05/16/audio_db6591201e.mp3"
    )
  )),
  sidebarPanel("sidebar",
               actionButton("AUDIO2", "Play recording2"),
                 actionButton("clearaudio", "clear audio"),
                actionButton("rmv", "remove me"),
               numericInput("rmv", "start", value=1),
                numericInput("end", "end", value =20))
        
)

server <- function(input, output, session) {
  # read in file
  FLASH <- reactive({
    req(input$file1)
    infile <- input$file1
    audio <- if(grepl('.*\\.wav', ignore.case=TRUE, infile$datapath)) {readWave(infile$datapath)} else
    {readMP3(infile$datapath)}
    return(audio)
  }) 
  
  observeEvent(input$AUDIO2, {
    req( input$file1 )
    base64 <- dataURI(file = input$file1$datapath, mime = "audio/wav")
    insertUI(selector = '#AUDIO2', where = 'afterEnd',
                                     ui = tags$div(id = "playme", howler::howlerModuleUI(
                                       id = "sound",
                                       files = list("imported audio" = base64),
                                       
                                     ),
                                     howlerSeekSlider("playme")
    ))
  })
  observeEvent(input$clearaudio, {
    removeUI( selector ="#playme", immediate = T)
  })
}

shinyApp(ui = ui, server = server)
