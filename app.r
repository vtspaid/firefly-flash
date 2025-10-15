
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
source("www/R/module/ViewerMod.R")
source("www/R/module/ControlsMod.R")
source("www/R/module/outputMod.R")

# UI ---------------------------
ui <- fluidPage(
  
  useShinyjs(),
  
  # Link to style sheet
  tags$head(
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "styles/app_styles.css")
  ),
  
  # App Header ----
  div(class = "myheader",
      fluidRow(column(1),
               column(10, titlePanel("Firefly Flash Statistics")),
               column(1, img(src = "potomaca_pic.png", height = "80pts")))
  ),
  
  # App Body ----
  # Sidebar layout with input and output definitions ----
  sidebarPanel(ControlsUI("controls")),
  
  # Main panel for displaying outputs ----
  mainPanel(
    tabsetPanel(
      tabPanel("Calculate Statistics",
               ViewerUI("fullview"),
               OutputUI("output")),
      
      tabPanel("Example", example_text()),
      
      tabPanel("Details", details_text())
    )
  )
)


# Server --------------
server <- function(input, output, session) {

  options(shiny.maxRequestSize = 15 * 1024^2)

  # Initilize a reactive values
  app_values <- reactiveValues(countervalue = 0,
                               addcounter = 0,
                               flashtype = "single flash",
                               tstart = 0,
                               tend = 30,
                               pause = 1,
                               quant = 0.999,
                               freq = 9)

  flash <- ControlsServer("controls", input, app_values)

  # Create plot of overall audio file
  ViewerServer("fullview", flash, input)

  # Create table of flash statistics and plot of where falshes were detected
  OutputServer("output", input, flash, app_values)
}


shinyApp(ui = ui, server = server)
