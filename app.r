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
source("www/R/module/viewPlotMod.R")

# UI ---------------------------
ui <- fluidPage(
  
  # Add color
  tags$style('.container-fluid {
                             background-color: #BDF5BD;
              }'),
  # App title ----
  fluidRow( column(1),
            column(10, titlePanel(p("Firefly Flash Statistics", 
                                    style='font-weight: bold; 
                                    font-size:120%; 
                                    text-align:center'))),
            column(1, img(src='potomaca_pic.png', height='100%', width='100%'))),

InputFileUI("GrabFile"), 
 

  # Sidebar layout with input and output definitions ----
    sidebarPanel(
      tabsetPanel(
        tabPanel("Plot start and end times",
                  # The next two lines are inputs for the xlim of the audio plot. The end gets updated later
                  ViewPlotUI("viewplot")
                 ),

      
        tabPanel("Flash calculations",
                 numericInput("tstart", "start time", value = 0, min = 1, max = 30), # start time of recording to use
                 numericInput("tend", "end time", value = 10, min = 1, max = 10000), # end time of recording to use
                 numericInput("quant", "amplitude quantile", value=0.999, min=0.85, max=1, step = 0.001), # quantile to use for amplitude cutoff
                 numericInput("freq", "frequency (slow glow only)", value = 9, min = 0, max = 25, step = 1), # frequency to use as cutoff
                 numericInput("pause", "Group flashes if less than x seconds (complex flash only)", value=1, min=0, max=100, step=0.1),
                 radioButtons("flashtype", "tlash pattern", choices = c("single flash", "complex flash", "glow")),
                 fluidRow( column(12, actionButton("cancelnoise", "remove noise"))),
                           fluidRow(column(8, actionButton("rmv_cancelnoise", "restore noise"))),
                            br(),
                           fluidRow(column(8, actionButton("addflash", "add flash/noise")),
                                    column(6)),
                           fluidRow(column(1, actionButton("rmv_addflash", "remove added flash/noise"))),
                            br(),
                           fluidRow(column(1, actionButton("flash_calc", "Run flash calculations", style='font-weight: bold; font-size:120%'))),
                 useShinyjs()
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
      p("This plot is controlled by the 'plot times' tab on the left. It is purely for visulization of the audio"),
      plotOutput(outputId = "flashplot"),
      
      br(),
      flash_stats_text,
      tableOutput(outputId = "flash_stats"),
      
      br(),
      br(),
      p("This plot is of the audio used in the flash calculations, the red lines are where the r function believes a 
         flash occured"),
      plotOutput(outputId = "resultsplot"),
      br(), br(),br(),br(),br(),br(),br(),br(),br(),br()),
      
      tabPanel("Details",
               details_text()
      )
   )
   )
)
  

##### SERVER #######

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 15 * 1024^2)
  
 # Read in file
  FLASH <- InputFileServer("GrabFile")

  # Insert audio UI
  ViewPlotServer("viewplot", FLASH, input[["GrabFile-inputfile"]])
  
  # Initilize a reactive value for removing noise
  counter <- reactiveValues(countervalue = 0)
  
  # intialize another reactive value for adding flash
  counterflash <- reactiveValues(countervalue = 0)
  
  # insert numeric inputs to remove background noise
  observeEvent(input$cancelnoise, {
    req(input$cancelnoise)
    counter$countervalue <- counter$countervalue + 1 
    num <- counter$countervalue
    insertUI( selector = "#cancelnoise", where = 'afterEnd',
              ui =  tags$div(id="removeall", fluidRow( column(6,numericInput(paste0("rmstart",num), "Remove noise from", value=0.01, min=0.1, max=1000)),
                                        column(6, numericInput(paste0("rmend", num), "Remove noise to", value=0.01, min=0.1, max=1000))
              )))
  })
  
  # remove UI inputs for background noise
  observeEvent(input$rmv_cancelnoise, {
     updateNumericInput(session, "rmstart1", value =0)
     updateNumericInput(session, "rmstart2", value =0)
     updateNumericInput(session, "rmstart3", value =0)
     updateNumericInput(session, "rmstart4", value =0)
     updateNumericInput(session, "rmstart5", value =0)
     updateNumericInput(session, "rmstart6", value =0)
     updateNumericInput(session, "rmstart7", value =0)
     updateNumericInput(session, "rmstart8", value =0)
     updateNumericInput(session, "rmstart9", value =0)
     updateNumericInput(session, "rmstart10", value =0)
     updateNumericInput(session, "rmstart11", value =0)
     updateNumericInput(session, "rmstart12", value =0)
     updateNumericInput(session, "rmstart13", value =0)
     updateNumericInput(session, "rmstart14", value =0)
     updateNumericInput(session, "rmstart15", value =0)
     updateNumericInput(session, "rmend1", value =0)
     updateNumericInput(session, "rmend2", value =0)
     updateNumericInput(session, "rmend3", value =0)
     updateNumericInput(session, "rmend4", value =0)
     updateNumericInput(session, "rmend5", value =0)
     updateNumericInput(session, "rmend6", value =0)
     updateNumericInput(session, "rmend7", value =0)
     updateNumericInput(session, "rmend8", value =0)
     updateNumericInput(session, "rmend9", value =0)
     updateNumericInput(session, "rmend10", value =0)
     updateNumericInput(session, "rmend11", value =0)
     updateNumericInput(session, "rmend12", value =0)
     updateNumericInput(session, "rmend13", value =0)
     updateNumericInput(session, "rmend14", value =0)
     updateNumericInput(session, "rmend15", value =0)
    removeUI( selector ="#removeall", immediate = F)
  })
  
  # insert numeric inputs to add noise
  observeEvent(input$addflash, {
    req(input$addflash)
    counterflash$countervalue <- counterflash$countervalue +1
    num1 <- counterflash$countervalue
    insertUI(selector = "#addflash", where = 'afterEnd',
             ui = tags$div(id="flashadd", numericInput(paste0("added", num1), 
                                                                          "add a flash at x time", value=NA, min=0, max=1000)))
  })
  
  # Remove UI inputs for adding flash
  observeEvent(input$rmv_addflash, {
    updateNumericInput(session, "added1", value = NA)
    updateNumericInput(session, "added2", value = NA)
    updateNumericInput(session, "added3", value = NA)
    updateNumericInput(session, "added4", value = NA)
    updateNumericInput(session, "added5", value = NA)
    updateNumericInput(session, "added6", value = NA)
    updateNumericInput(session, "added7", value = NA)
    updateNumericInput(session, "added8", value = NA)
    updateNumericInput(session, "added9", value = NA)
    updateNumericInput(session, "added10", value = NA)
    updateNumericInput(session, "added11", value = NA)
    updateNumericInput(session, "added12", value = NA)
    updateNumericInput(session, "added13", value = NA)
    updateNumericInput(session, "added14", value = NA)
    updateNumericInput(session, "added15", value = NA)
    removeUI(selector="#flashadd", immediate=F)
    
  })
  
  # Update max length of file
  observeEvent(input[["GrabFile-inputfile"]], {
    print("trying")
    req(FLASH())
    req(FLASH()$audio@left)
    req(FLASH()$audio@samp.rate)
    
    print("starting to update max")
    duration <- length(FLASH()$audio@left) / FLASH()$audio@samp.rate
    updateNumericInput(session, "end", value = duration, max = duration)
    updateNumericInput(session, "tend", value = duration, max = duration)
    print("finished update max")
  })
  
  # Create plot of overall audio file
  output$flashplot <- renderPlot({
    print("starting_plot")
    print("input names")
    print(names(input))
    req(FLASH())
    req(input$plotaudio)
    train_audio = FLASH()$audio
    timeArray <- (0:(length(train_audio@left)-1)) / train_audio@samp.rate
    # Plot the wave
    isolate(plot(x=timeArray, y=train_audio@left, type='l',
         col='black', xlab='Seconds', ylab='Amplitude', xlim=c(input$start, input$end)))
    
  })

  # Create table of flash statistics
  output$flash_stats  <- renderTable({
    req(FLASH())
    req(input$flash_calc)
    dfflash <- FLASH()$audio
    samprate <- FLASH()$audio@samp.rate
    
    # Remove flash
    isolate(if(counter$countervalue > 0) 
    {rm_times <- lapply(1:counter$countervalue, function(x) 
      data.frame(start = eval(parse(text =paste0("input$rmstart",x))),
                 end = eval(parse(text=paste0("input$rmend",x)))
      ))
    }
    )
    isolate( if(counter$countervalue > 0)  {for (ii in 1:length(rm_times)){
      dfflash$left[c(rm_times[[ii]]$start*samprate):c(rm_times[[ii]]$end*samprate)] <- 0
    }
    })
    # Add flash
    isolate(if(counterflash$countervalue > 0) 
    {add_times <- lapply(1:counterflash$countervalue, function(x) 
      data.frame(newflash = eval(parse(text =paste0("input$added",x))))
      )
      })
    
    isolate( if(counterflash$countervalue > 0)  {for (ii in 1:length(add_times)){
      dfflash$left[c(add_times[[ii]]$newflash*samprate)] <- quantile(dfflash$left, input$quant) + 1
    }
    })
    
    # Render table
    isolate(if (input$flashtype == 'single flash') {
      singleflash(dfflash,
                              start=input$tstart, end=input$tend, quant=input$quant
      )
    } else if (input$flashtype == 'complex flash'){
      complexflash(dfflash,
                               start=input$tstart, end=input$tend, pause=input$pause, 
                                quant=input$quant
      )
    } else {slowglow(dfflash,
                                 start=input$tstart, end=input$tend, quant=input$quant, freq = input$freq
    )})

            
                                })

  # Create plot checking where the code thinks the flashes are
  output$resultsplot <- renderPlot({
    req(FLASH())
    req(input$flash_calc)
    req(input[["GrabFile-inputfile"]])
    df2 <- FLASH()$audio
    samprate <- FLASH()$audio@samp.rate
    # Remove flash
    isolate(if(counter$countervalue > 0) 
      {rm_times <- lapply(1:counter$countervalue, function(x) 
        data.frame(start = eval(parse(text =paste0("input$rmstart",x))),
        end = eval(parse(text=paste0("input$rmend",x)))
        ))
    }
    )
   isolate( if(counter$countervalue > 0)  {for (ii in 1:length(rm_times)){
      df2$left[c(rm_times[[ii]]$start*samprate):c(rm_times[[ii]]$end*samprate)] <- 0
    }
     })
   
   # Add flash
   isolate(if(counterflash$countervalue > 0) 
   {add_times <- lapply(1:counterflash$countervalue, function(x) 
     data.frame(newflash = eval(parse(text =paste0("input$added",x))))
   )
   })
   
   isolate( if(counterflash$countervalue > 0)  {for (ii in 1:length(add_times)){
     df2$left[c(add_times[[ii]]$newflash*samprate)] <- c(quantile(df2$left, probs=input$quant)+1)
   }
   })
   
    isolate(if (input$flashtype == 'single flash') {
      flashcheck(df2, start=input$tstart, end=input$tend, quant=input$quant)
    } else if (input$flashtype == 'complex flash'){
      complexflashcheck(df2, start=input$tstart, end=input$tend, quant=input$quant, pause=input$pause)
    } else {glowcheck(df2, start=input$tstart, end=input$tend, quant=input$quant, freq = input$freq)})

  })
  

}


shinyApp(ui = ui, server = server)
#runApp("app.r")

# to run app on your machine
#runApp(appDir="C:/Users/xxx/xxx/app.r")

# to deploy app (only for app developer)
#library(rsconnect)
#deployApp()




