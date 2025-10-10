
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
  navbarPage("",

             # # Panel for manually drawing on plot and calculating time
             # tabPanel("Manual Method",
             #          p("Manual Method Landing Page")
             # ), # end of manual tabPanel

             # Panel for automatically calculating time
             tabPanel("Automatic Method",

                      InputFileUI("GrabFile"),

                      # Sidebar layout with input and output definitions ----
                      sidebarPanel(
                        tabsetPanel(
                          tabPanel("Plot start and end times",
                                   ListenUI("viewplot")),

                          tabPanel("Flash calculations",
                                   ControllerUI("controls"))
                        )
                      ), # end of sidebar panel


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
             ) # end of automatic tabPanel
  ) # End of navbarPage
)


##### SERVER #######
server <- function(input, output, session) {

  options(shiny.maxRequestSize = 15 * 1024^2)

  # Read in file
  flash <- InputFileServer("GrabFile")

  # Insert audio UI
  xlims <- ListenServer("viewplot", flash, input)

  # Initilize a reactive values
  app_values <- reactiveValues(countervalue = 0,
                               addcounter = 0,
                               flashtype = "single flash",
                               tstart = 0,
                               tend = 30,
                               pause = 1,
                               quant = 0.999,
                               freq = 9)

  ControllerServer("controls", app_values)

  # Create plot of overall audio file
  ViewerServer("fullview", flash, input, xlims)

  # Create table of flash statistics
  OutputServer("output", input, flash, app_values)
}


shinyApp(ui = ui, server = server)
