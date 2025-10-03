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


# UI ---------------------------
ui <- fluidPage(
  
  # add color
  tags$style('.container-fluid {
                             background-color: #BDF5BD;
              }'),
  # App title ----
  fluidRow( column(1),
            column(10, titlePanel(p("Firefly Flash Statistics", style='font-weight: bold; font-size:120%; text-align:center'))),
            column(1, img(src='potomaca_pic.png', height='100%', width='100%'))),
  # add image to top right of page
  # img(src='potomaca_pic.png', height='10%', width='10%', style="position:relative; right:1px"),
  # img(src='potomaca_pic.png', height='10%', width='10%', style="position:absolute; right:-8"),
  # 
fileInput("file1", "Choose a .wav or .mp3 file",
            accept = c(".wav", ".mp3")), 
 

  # Sidebar layout with input and output definitions ----
    sidebarPanel(
      tabsetPanel(
        tabPanel("Plot start and end times",
                  # the next two lines are inputs for the xlim of the audio plot. The end gets updated later
                  numericInput("start", "plot start time", value = 0, min = 0, max = 10000),
                  numericInput("end", "plot end time", value = 10, min = 1, max = 10000),
                 actionButton("plotaudio", "Plot audio"),

        actionButton("AUDIO2", "Play audio"),
        actionButton("clearaudio", "Remove audio player")),

      
        tabPanel("Flash calculations",
                 numericInput("tstart", "start time", value = 0, min = 1, max = 30), # start time of recording to use
                 numericInput("tend", "end time", value = 10, min = 1, max = 10000), # end time of recording to use
                 numericInput("quant", "amplitude quantile", value=0.999, min=0.85, max=1, step = 0.001), # quantile to use for amplitude cutoff
                 numericInput("freq", "frequency (slow glow only)", value = 9, min = 0, max = 25, step = 1), # frequency to use as cutoff
                 numericInput("pause", "Group flashes if less than x seconds (complex flash only)", value=1, min=0, max=100, step=0.1),
                 #textInput("species", "species", value = "", width = NULL, placeholder = NULL), # set species
                 #numericInput("sample", "sample #", value = 1, min=1, max = 100000), # set sample number
                 #textInput("site", "site name", value = "", width = NULL, placeholder = NULL), # set site
                 #numericInput("temp", "temperature", value = "", min=1, max = 100000), # set temp
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
                 img(src='table_with_error_fixed.png', height='50%', width='50%'),
                 br(), br(),
                 p("We can also use the 'add flash' button to add a flash into the plot if the function is not identifying 
                   all flashes"),
                 br(), p("The radio buttons will select which of three functions to use
                         based on what type of flash pattern you recorded. The amplitude quantile
                         controls how loud a noise is before it is considered a flash; slight changes make a big
                         differences, best results tend to be between 0.995 and 0.999. The 'Group flashes if less than x seconds' only 
                         comes into play when using the 'complex flash' function, and sets the cut off point for when flashes will be 
                         considered part of the same group.
                         "),
                 br(), br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                 
                  ),

        tabPanel( "Run Flash Calculations",
      p("This plot is controlled by the 'plot times' tab on the left. It is purely for visulization of the audio"),
      plotOutput(outputId = "flashplot"),
      
      br(),
      p("This plot shows the section of the audio chosen for analysis. If the single flash method was chosen their
         will be red lines representing where flashes were detected. If the complex flash method was chosen there will 
        be green and blue lines representing where flashes occured and what group they belong to. If glow method was chosen 
        there will be a green line showing where the start of a flash is detected, and a red line showing where the end 
        of the flash is detected."),
      tableOutput(outputId = "flash_stats"),
      
      br(),
      br(),
      p("This plot is of the audio used in the flash calculations, the red lines are where the r function believes a 
         flash occured"),
      plotOutput(outputId = "resultsplot"),
      br(), br(),br(),br(),br(),br(),br(),br(),br(),br()),
      
      tabPanel("Details",
      br(),br(),
      p("If you are viewing this page on the internet instead of running it locally from you machine, you should be aware
         that the web version is much slower than running it locally. Additionally the website is limited in the number 
        of hours it can run per month. If you are familiar with program R and have R studio on your machine, I recommend 
        downloading the source code from git hub. https://github.com/vtspaid/firefly-flash."),
      br(),br(),
      h4("Amplitude quantile:"), p("This is the quantile cutoff used for deciding when a flash occured. The default
                                    is .999 meaning a noise must be above the 999th percentile to be considered a flash.
                                    small changes can make a big difference. If a flash is not being detected, try 
                                   decreasing the amplitude quantile by .005, and vice versa for a when to many non-flash 
                                   noises are being falsely flagged as flashes. For the long flash/glow method, you will 
                                   need to decrease the quantile to 0.9 or even lower. Oftentimes you will end up with a 
                                   mix of false negatives and false positives; this is where 
                                   you will need to use the remove noise, and add flash options."),
      br(),br(),
      h4("remove noise/restore noise:"),
      p("Hitting the remove noise button allows you to select a time range where you can set the amplitude to zero.
         You can remove multiple sections at once. The 'restore noise' button will restore ALL sections of removed amplitude. 
        If you wish to only remove one section, you will have to manually change the times. Note that if the remove noise button
        is clicked more than 15 times in a session it will cause the shiny app to crash. Best practice is to refresh the page 
        after completing analysis on an audio file, before importing another audio file."),
      br(),br(),
      h4("add flash/noise:"),
      p("This will add a flash at a specific location, if the function fails to detect. Currently this only works for quick 
        flashes. In the future I may add functionality for adding long, glow type flashes."),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br())
      
      
      
      ))
   )
  

##### SERVER #######

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 15 * 1024^2)
  
 # read in file
  FLASH <- reactive({
    req(input$file1)
    infile <- input$file1
    
    audio <- if(grepl('.*\\.wav', ignore.case=TRUE, infile$datapath)) {readWave(infile$datapath)} else
    {readMP3(infile$datapath)}
    audio@left <- audio@left[seq(1, length(audio@left), by =4)]
    audio@samp.rate <- audio@samp.rate/4
    return(audio)
  }) 

  # insert audio UI
  observeEvent(input$AUDIO2, {
    req( input$file1 )
    base64 <- dataURI(file = input$file1$datapath, mime = "audio/wav")
     insertUI(selector = '#AUDIO2', where = 'afterEnd',
    ui = tags$div(id = "howleraudio", howler::howlerModuleUI(
      id = "sound",
      files = list("imported audio" = base64)
      )
    ))
    })
  
  # remove audio UI
  observeEvent(input$clearaudio, {
    removeUI( selector ="#howleraudio", immediate = T)
  })
  
  #initilize a reactive value for removing noise
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
  
  # remove UI inputs for adding flash
  observeEvent(input$rmv_addflash, {
    updateNumericInput(session, "added1", value =NA)
    updateNumericInput(session, "added2", value =NA)
    updateNumericInput(session, "added3", value =NA)
    updateNumericInput(session, "added4", value =NA)
    updateNumericInput(session, "added5", value =NA)
    updateNumericInput(session, "added6", value =NA)
    updateNumericInput(session, "added7", value =NA)
    updateNumericInput(session, "added8", value =NA)
    updateNumericInput(session, "added9", value =NA)
    updateNumericInput(session, "added10", value =NA)
    updateNumericInput(session, "added11", value =NA)
    updateNumericInput(session, "added12", value =NA)
    updateNumericInput(session, "added13", value =NA)
    updateNumericInput(session, "added14", value =NA)
    updateNumericInput(session, "added15", value =NA)
    removeUI(selector="#flashadd", immediate=F)
    
  })
  

  # make max length input reactive to file input 
  observeEvent(input$file1, {  updateNumericInput(session, "end",
                                                        value = length(FLASH()@left)/FLASH()@samp.rate,
                                                        max = length(FLASH()@left)/FLASH()@samp.rate)
    updateNumericInput(session, "tend",
                       value = length(FLASH()@left)/FLASH()@samp.rate,
                       max = length(FLASH()@left)/FLASH()@samp.rate)
  })
  
  # create plot of overall audio file
  output$flashplot <- renderPlot({
    req(input$plotaudio)
    train_audio = FLASH()
    timeArray <- (0:(length(train_audio@left)-1)) / train_audio@samp.rate
    #Plot the wave
    isolate(plot(x=timeArray, y=train_audio@left, type='l',
         col='black', xlab='Seconds', ylab='Amplitude', xlim=c(input$start, input$end)))
    
  })

  # create table of flash statistics
  output$flash_stats  <- renderTable({
    req(input$flash_calc)
    req(input$file1)
    dfflash <- FLASH()
    samprate <- FLASH()@samp.rate
    
    #remove flash
    isolate(if(counter$countervalue >0) 
    {rm_times <- lapply(1:counter$countervalue, function(x) 
      data.frame(start = eval(parse(text =paste0("input$rmstart",x))),
                 end = eval(parse(text=paste0("input$rmend",x)))
      ))
    }
    )
    isolate( if(counter$countervalue >0)  {for (ii in 1:length(rm_times)){
      dfflash@left[c(rm_times[[ii]]$start*samprate):c(rm_times[[ii]]$end*samprate)] <- 0
    }
    })
    # add flash
    isolate(if(counterflash$countervalue >0) 
    {add_times <- lapply(1:counterflash$countervalue, function(x) 
      data.frame(newflash = eval(parse(text =paste0("input$added",x))))
      )
      })
    
    isolate( if(counterflash$countervalue >0)  {for (ii in 1:length(add_times)){
      dfflash@left[c(add_times[[ii]]$newflash*samprate)] <- quantile(dfflash@left, input$quant)+1
    }
    })
    
    # render table
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

  # create plot checking where the code thinks the flashes are
  output$resultsplot <- renderPlot({
    req(input$flash_calc)
    req(input$file1)
    df2 <- FLASH()
    samprate <- FLASH()@samp.rate
    #remove flash
    isolate(if(counter$countervalue >0) 
      {rm_times <- lapply(1:counter$countervalue, function(x) 
        data.frame(start = eval(parse(text =paste0("input$rmstart",x))),
        end = eval(parse(text=paste0("input$rmend",x)))
        ))
    }
    )
   isolate( if(counter$countervalue >0)  {for (ii in 1:length(rm_times)){
      df2@left[c(rm_times[[ii]]$start*samprate):c(rm_times[[ii]]$end*samprate)] <- 0
    }
     })
   
   # add flash
   isolate(if(counterflash$countervalue >0) 
   {add_times <- lapply(1:counterflash$countervalue, function(x) 
     data.frame(newflash = eval(parse(text =paste0("input$added",x))))
   )
   })
   
   isolate( if(counterflash$countervalue >0)  {for (ii in 1:length(add_times)){
     df2@left[c(add_times[[ii]]$newflash*samprate)] <- c(quantile(df2@left, probs=input$quant)+1)
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




