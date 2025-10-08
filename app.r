### Code for creating shiny app that calculates statistics for firefly flashes ###

# Load necessary packages
library(shiny)
library(tuneR)
library(dplyr)
library(base64enc)
library(howler)
library(shinyjs)

# Source necessary scripts
source("www/R/flash_functions.R")
source("www/R/example_text.R")
source("www/R/module/InputFileMod.R")
source("www/R/module/ListenMod.R")
source("www/R/module/ViewerMod.R")
source("www/R/module/ControlerMod.R")
source("www/R/module/outputMod.R")

# UI ---------------------------
ui <- fluidPage(
  
  useShinyjs(),
  
  # Link to style sheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/app_styles.css")
  ),
  
  # App title ----
  div(class = "myheader", fluidRow(column(12, column(1),
            column(10, titlePanel(p("Firefly Flash Statistics", 
                                    style='font-weight: bold; 
                                    font-size:120%; 
                                    text-align:center'))),
            column(1, img(src='potomaca_pic.png', height='80pts'))))
      ),
  navbarPage("",
    tabPanel("Manual Method",
             p("Manual Method Landing Page")),
    
             tabPanel("Automatic Method", 

InputFileUI("GrabFile"), 
 

  # Sidebar layout with input and output definitions ----
    sidebarPanel(
      tabsetPanel(
        tabPanel("Plot start and end times",
                  # The next two lines are inputs for the xlim of the audio plot. The end gets updated later
                 ListenUI("viewplot")
                 ),

      
        tabPanel("Flash calculations",
                 ControllerUI("controls")
                 )

      )),

    
    # Main panel for displaying outputs ----
    mainPanel( 
      
      # Example tab ----
      tabsetPanel(
        tabPanel( "Example",
                  example_text(),
                  ),

        tabPanel( "Run Flash Calculations",
                  ViewerUI("fullview"),
                  OutputUI("output")),
    
      tabPanel("Details",
               details_text()
       )
   )
   )
             ) # end of originanl tabPanel
  ) # End of navbarPage
)
  
##### SERVER #######

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 15 * 1024^2)
  
 # Read in file
  FLASH <- InputFileServer("GrabFile")

  # Insert audio UI
  xlims <- ListenServer("viewplot", FLASH, input)
  
  observeEvent(input[["GrabFile-inputfile"]], {
    print("How about this")
  })
  # Initilize a reactive value for removing noise
  counter <- reactiveValues(countervalue = 0)
  
  # intialize another reactive value for adding flash
  counterflash <- reactiveValues(countervalue = 0)
  
  flashtype <- reactiveValues(flashtype = "single flash")
  controls <- reactiveValues(tstart = 0, 
                             tend = 30, 
                             pause = 1, 
                             quant = 0.999,
                             freq = 9)
  
  ControllerServer("controls", counter, counterflash, flashtype, controls)
  
  # Create plot of overall audio file
  ViewerServer("fullview", FLASH, input, xlims)

  # Create table of flash statistics
  OutputServer("output", input, FLASH, counter, counterflash, flashtype, 
               controls)
}


shinyApp(ui = ui, server = server)
#runApp("app.r")

# to run app on your machine
#runApp(appDir="C:/Users/xxx/xxx/app.r")

# to deploy app (only for app developer)
#library(rsconnect)
#deployApp()
