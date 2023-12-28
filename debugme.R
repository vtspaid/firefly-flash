library(shiny)
library(howler)
library(tuneR)
library(dplyr)
library(base64enc)

ui <- fluidPage(
  fileInput("file1", "Choose a .wav or .mp3 file",  accept = c(".wav", ".mp3")),

  sidebarPanel("sidebar",
               actionButton("needremove", "remove sound"),
               actionButton("removeme", "removeme")
        
))

server <- function(input, output, session) {
  # read in file
  FLASH <- reactive({
    req(input$file1)
    infile <- input$file1
    audio <- if(grepl('.*\\.wav', ignore.case=TRUE, infile$datapath)) {readWave(infile$datapath)} else
    {readMP3(infile$datapath)}
    return(audio)
  }) 
  
  observeEvent(input$needremove, {
insertUI( selector = "#needremove", where = 'afterEnd',
  ui =  fluidRow( tags$div(id="removeall", column(4, numericInput("rmstart1", "Remove noise", value=0.01, min=0.1, max=1000)),
           column(4, numericInput("rmend1", "Remove noise2", value=0.01, min=0.1, max=1000))
  )))
  })
  
  observeEvent(input$removeme, {
    removeUI( selector ="#removeall", immediate = T)
  })
}

shinyApp(ui = ui, server = server)
