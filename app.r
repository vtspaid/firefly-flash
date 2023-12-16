### Code for creating shiny app that calculates statistics for firefly flashes ###

#load necessary packages
library(shiny)
library(tuneR)
library(dplyr)
library(base64enc)

# load example audio
#FLASH = readWave(paste0(getwd(),"/Dunkard_potomaca.WAV"))


##### create two custom functions needed #####

singleflash <- function(wav, start=0, end=length(wav@left)/wav@samp.rate, quant=0.998,
                        species="sample", sample=1, site="site", temp=NA){
  starting=start*wav@samp.rate+0.01 # multiply the starting input by the sample rate to get starting frame
  ending=end*wav@samp.rate # get ending frame
  amp<-wav@left[starting:ending] # creates a vector of amplitudes using provided start and end times
  
  #create time array by dividing the current frame by the sample rate and adding the initial start time
  timeArray <- ((0:(length(amp)-1)) / wav@samp.rate)+start
  
  #create dataframe of time and amplitude
  timeamp <- data.frame(Time=timeArray, Amp=amp)
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp[timeamp$Amp > quantile(timeamp$Amp, quant),]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <- c(0, diff(timeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is less than .1 sec
  # then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(samepeak = ifelse(timediff < .15, 0, 1))%>% 
    mutate(samepeak = replace(samepeak, is.na(samepeak), 1)) %>%
    mutate(grouping = cumsum(samepeak))
  
  # find median time of each sound grouping
  peak <- timeamp %>% group_by(grouping) %>% summarise(peakTime=median(Time))
  
  # find time difference in the peak times
  peak$timediff <- c(0, diff(peak$peakTime))
  
  return(output <- data.frame(Species = species, sample = sample, site=site, temp = temp, 
                              mean_interval=mean(peak$timediff[-1]), max_interval = max(peak$timediff[-1]), 
                              min = min(peak$timediff[-1]), sd = sd(peak$timediff[2:length(peak$timediff)]),
                              flash_num = (nrow(peak)-1)))
  
}





flashcheck <- function(wav, start=0, end=length(wav@left)/wav@samp.rate, quant=0.998){
  starting=start*wav@samp.rate+1 # converts seconds into the correct sample number (have to add 1 or default would be invalid)
  ending=end*wav@samp.rate
  amp<-wav@left[starting:ending] # creates a vector of amplitudes using provided start and end times
  
  #create time array
  timeArray <- ((0:(length(amp)-1)) / wav@samp.rate)+start
  
  #create dataframe of time and amplitude
  timeamp  <-data.frame(Time=timeArray, Amp=amp)
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp[timeamp$Amp>quantile(timeamp$Amp, quant),]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <-c(0, diff(timeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is less than .1 sec
  # then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(samepeak = ifelse(timediff < .15, 0, 1))%>% 
    mutate(samepeak = replace(samepeak, is.na(samepeak), 1)) %>%
    mutate(grouping = cumsum(samepeak))
  
  # find median Time of each sound grouping
  peak <- timeamp %>% group_by(grouping) %>% summarise(peakTime=median(Time))
  
  flashcheck <- plot(x=timeArray, y=amp, type='l',
                     col='black', xlab='Seconds', ylab='Amplitude', xaxt="n")
  axis(1, at = seq(1, round(max(timeArray)), by = 2), las=2)
  abline(v=peak$peakTime, col="red", lty="dotted")
  abline(h=quantile(timeamp$Amp, quant), col="blue")
  
  return(flashcheck)
}

######


ui <- fluidPage(
  # App title ----
  titlePanel("Firefly Flash Statistics"),
  fileInput("file1", "Choose a .wav file",
            accept = c(".wav")),
  # Sidebar layout with input and output definitions ----
    sidebarPanel(
      tabsetPanel(
        tabPanel("plot times",
                 # numericInput("start", "plot start time", value = 0, min = 1, max = length(FLASH@left)/FLASH@samp.rate),
                 # numericInput("end", "plot end time", value = length(FLASH@left)/FLASH@samp.rate, min = 1, max = 42),
                  numericInput("start", "plot start time", value = 0, min = 1, max = 30),
                  numericInput("end", "plot end time", value = 30, min = 1, max = 60),
                 
                  #actionButton("AUDIO", "Play recording"),
        actionButton("AUDIO2", "Play recording2")),

      
        tabPanel("Flash calculations",
                 # numericInput("tstart", "start time", value = 0, min = 1, max = length(FLASH@left)/FLASH@samp.rate), # start time of recording to use
                 # numericInput("tend", "end time", value = length(FLASH@left)/FLASH@samp.rate, min = 1, max = length(FLASH@left)/FLASH@samp.rate), # end time of recording to use
                 # textInput("species", "species", value = "", width = NULL, placeholder = NULL), # set species
                 # numericInput("sample", "sample #", value = 1, min=1, max = 100000), # set sample number
                 # textInput("site", "site name", value = "", width = NULL, placeholder = NULL), # set site
                 # numericInput("temp", "Temperature", value = "", min=1, max = 100000)) # set temp

                 numericInput("tstart", "start time", value = 1, min = 1, max = 30), # start time of recording to use
                 numericInput("tend", "end time", value = 30, min = 1, max = 60), # end time of recording to use
                 numericInput("quant", "amplitude quantile", value=0.99, min=0.85, max=1, step = 0.001), # quantile to use for amplitude cutoff
                 textInput("species", "species", value = "", width = NULL, placeholder = NULL), # set species
                 numericInput("sample", "sample #", value = 1, min=1, max = 100000), # set sample number
                 textInput("site", "site name", value = "", width = NULL, placeholder = NULL), # set site
                 numericInput("temp", "Temperature", value = "", min=1, max = 100000), # set temp
                 # actionButton('deletetime', "remove noise"))
                 fluidRow(
                   column(4, numericInput("rmstart1", "Remove noise", value="", min=0.1, max=1000)),
                   column(4, numericInput("rmend1", "Remove noise2", value="", min=0.1, max=1000))
                          )
                 )

      )),

    
    # Main panel for displaying outputs ----
    mainPanel( 
      
      # Output: Histogram ----
      p("This plot is controlled by the 'plot times' tab on the left. It is purely for visulization of the audio"),
      plotOutput(outputId = "flashplot"),
      
      br(),
      p("This is the output you get using the start and end times selected in the 'flash calculations' tab on the left.
      You can supply the start and end times for the calculations, and the species, site and temperature in case you 
      want to copy and paste the results into a spreadsheet"),
      tableOutput(outputId = "flash_stats"),
      
      br(),
      br(),
      p("This plot is of the audio used in the flash calculations, the red lines are where the r function believes a 
         flash occured"),
      plotOutput(outputId = "resultsplot"))
      
    )
  

##### RUN SHINY APP #######

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  
  FLASH <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    audio <- readWave(infile$datapath)
    return(audio)
  }) 
  

  
  output$flash_stats  <- renderTable({ 
    df <- FLASH()
    samprate <- FLASH()@samp.rate
    df@left[c(input$rmstart1*samprate):c(samprate*input$rmend1)] <- 0
    singleflash(df,
                start=input$tstart, end=input$tend, species=input$species, sample=input$sample,
               site=input$site, temp=input$temp, quant=input$quant
               )
                                })


    

  
  # updateNumericInput(
  #   session = getDefaultReactiveDomain(),
  #   inputId,
  #   label = NULL,
  #   value = NULL,
  #   min = NULL,
  #   max = NULL,
  #   step = NULL
  # )
  
  output$resultsplot <- renderPlot({
    df2 <- FLASH()
    samprate <- FLASH()@samp.rate
    df2@left[c(input$rmstart1*samprate):c(samprate*input$rmend1)] <- 0
  flashcheck(df2, start=input$tstart, end=input$tend, quant=input$quant)})
  
  
  
  output$flashplot <- renderPlot({
  train_audio = FLASH()
  str(train_audio)
  s1 <- train_audio/2^(train_audio@bit -1)
  timeArray <- (0:(length(train_audio@left)-1)) / train_audio@samp.rate
  #Plot the wave
  plot(x=timeArray, y=train_audio@left, type='l',
       col='black', xlab='Seconds', ylab='Amplitude', xlim=c(input$start, input$end))
    
  })

  
  observeEvent( input$AUDIO2, {
    
    req( input$file1 )
    
    base64 <- dataURI(file = input$file1$datapath, mime = "audio/wav")
    
    insertUI( selector = "#AUDIO2", where = "afterEnd",
              
              ui = tags$audio( src = base64, type = "audio/wav", autoplay = NA, controls = NA )  
    )
    
    # observeEvent(input$deletetime, { insertUI( selector = '#deletetime', where = "afterend",
    #                                            ui = tagList(""))
    #   
    #})
    
  })

}

shinyApp(ui = ui, server = server)

d#runApp("app.r")
