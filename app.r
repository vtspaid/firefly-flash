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

complexflash <- function(wav, start=0, end=length(wav@left)/wav@samp.rate, quant=0.998,
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
  
  # find the 70th quantil of timedifference in flash timings
  breaktime <- mean(peak$timediff)
  
  # create complex flash groupings
  peak <- peak %>% mutate(samegroup = ifelse(timediff < breaktime, 0, 1)) %>%
    mutate(samegroup = replace(samegroup, is.na(samegroup), 1)) %>%
    mutate(grouping = cumsum(samegroup))
  
  # create df of just the interflash timings
  interflash <- peak[peak$timediff < breaktime,]
  
  # create df of just the between timings
  betweengroup <- peak[peak$timediff > breaktime,]
  
  # create df of number of flashes per group
  num_flashes <- peak %>% group_by(grouping) %>% summarise(n=n())
  
  return(list(flashdata <- data.frame(Species = species, sample = sample, site=site, temp = temp, 
                                      mean_interval=mean(interflash$timediff[-1]),
                                      mean_flash_num = mean(num_flashes$n),
                                      max_interval = max(interflash$timediff[-1]), 
                                      min = min(interflash$timediff[-1]), sd = sd(interflash$timediff[-1]),
                                      flash_num = nrow(interflash)),
              
              pausedata <- data.frame(Species = species, sample = sample, site=site, temp = temp,
                                      mean_between_group = mean(betweengroup$timediff),
                                      max_pause = max(betweengroup$timediff[-1]), 
                                      min_pause = min(betweengroup$timediff[-1]), pause_sd = sd(betweengroup$timediff[-1]),
                                      pause_num = nrow(betweengroup))))
  
  
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
  segments(x0=peak$peakTime, x1=peak$peakTime, y0=min(wav@left), y1=0, col="red")
  
  return(flashcheck)
}

complexflashcheck <- function(wav, start=0, end=length(wav@left)/wav@samp.rate, quant=0.998){
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
  
  # find time difference in the peak times
  peak$timediff <- c(0, diff(peak$peakTime))
  
  # find the 70th quantil of timedifference in flash timings
  breaktime <- mean(peak$timediff)
  
  # create complex flash groupings
  peak <- peak %>% mutate(samegroup = ifelse(timediff < breaktime, 0, 1)) %>%
    mutate(samegroup = replace(samegroup, is.na(samegroup), 1)) %>%
    mutate(grouping = cumsum(samegroup)+1) 
  
  
  flashcheck <- plot(x=timeArray, y=amp, type='l',
                     col='black', xlab='Seconds', ylab='Amplitude', xaxt="n")
  axis(1, at = seq(1, round(max(timeArray)), by = 2), las=2)
  segments(x0=peak$peakTime, x1= peak$peakTime, y0=min(FLASH@left), y1=0, 
           col=rep(c("blue", "red"), length(unique(peak$grouping)/2))[peak$grouping])
  
  
  return(flashcheck)
}

###### UI #####


ui <- fluidPage(
  # App title ----
  titlePanel("Firefly Flash Statistics"),
  fileInput("file1", "Choose a .wav or .mp3 file",
            accept = c(".wav", ".mp3")),
  # Sidebar layout with input and output definitions ----
    sidebarPanel(
      tabsetPanel(
        tabPanel("plot times",
                  # the next two lines are inputs for the xlim of the audio plot. The end gets updated later
                  numericInput("start", "plot start time", value = 0, min = 0, max = 10000),
                  numericInput("end", "plot end time", value = 10, min = 1, max = 10000),
                 
                  #actionButton("AUDIO", "Play recording"),
        actionButton("AUDIO2", "Play recording2")),

      
        tabPanel("Flash calculations",
                 numericInput("tstart", "start time", value = 1, min = 1, max = 30), # start time of recording to use
                 numericInput("tend", "end time", value = 10, min = 1, max = 10000), # end time of recording to use
                 numericInput("quant", "amplitude quantile", value=0.99, min=0.85, max=1, step = 0.001), # quantile to use for amplitude cutoff
                 textInput("species", "species", value = "", width = NULL, placeholder = NULL), # set species
                 numericInput("sample", "sample #", value = 1, min=1, max = 100000), # set sample number
                 textInput("site", "site name", value = "", width = NULL, placeholder = NULL), # set site
                 numericInput("temp", "Temperature", value = "", min=1, max = 100000), # set temp
                 # actionButton('deletetime', "remove noise"))
                 fluidRow(
                   column(4, numericInput("rmstart1", "Remove noise", value=0.01, min=0.1, max=1000)),
                   column(4, numericInput("rmend1", "Remove noise2", value=0.01, min=0.1, max=1000))
                          )
                 )

      )),

    
    # Main panel for displaying outputs ----
    mainPanel( 
      
      # Output: Histogram ----
      tabsetPanel(
        tabPanel( "Example",  
                  p("just a test"),
                  tags$div(img(src='singleflash_with_error_1.png', height='80%', width='80%')),
                 img(src='singleflash_with_error_calc_plot.png', height='80%', width='80%'),
                  img(src='singleflash_witherror_fixed.png', height='80%', width='80%'),
                 img(src='singleflash_with_error_table.png', height='80%', width='80%')
                  ),
        tabPanel( "Actual data",
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
   ))
  

##### SERVER #######

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  
  FLASH <- reactive({
    req(input$file1)
    infile <- input$file1
    # if (is.null(infile)) {
    #   return(NULL)
    # }
    audio <- if(grepl('.*\\.wav', ignore.case=TRUE, infile$datapath)) {readWave(infile$datapath)} else
      {readMP3(infile$datapath)}
   # audio <- readWave(infile$datapath)
    return(audio)
  }) 

  observeEvent(input$file1, {  updateNumericInput(session, "end",
                                                        value = length(FLASH()@left)/FLASH()@samp.rate,
                                                        max = length(FLASH()@left)/FLASH()@samp.rate)
    updateNumericInput(session, "tend",
                       value = length(FLASH()@left)/FLASH()@samp.rate,
                       max = length(FLASH()@left)/FLASH()@samp.rate)
  })

  output$flash_stats  <- renderTable({
    req(input$file1)
    dfflash <- FLASH()
    samprate <- FLASH()@samp.rate
    dfflash@left[c(input$rmstart1*samprate):c(samprate*input$rmend1)] <- 0
    singleflash(dfflash,
                start=input$tstart, end=input$tend, species=input$species, sample=input$sample,
               site=input$site, temp=input$temp, quant=input$quant
               )
                                })


  output$resultsplot <- renderPlot({
    req(input$file1)
    df2 <- FLASH()
    samprate <- FLASH()@samp.rate
    df2@left[c(input$rmstart1*samprate):c(samprate*input$rmend1)] <- 0
  flashcheck(df2, start=input$tstart, end=input$tend, quant=input$quant)})



  output$flashplot <- renderPlot({
    req(input$file1)
  train_audio = FLASH()
  str(train_audio)
  s1 <- train_audio/2^(train_audio@bit -1)
  timeArray <- (0:(length(train_audio@left)-1)) / train_audio@samp.rate
  #Plot the wave
  plot(x=timeArray, y=train_audio@left, type='l',
       col='black', xlab='Seconds', ylab='Amplitude', xlim=c(input$start, input$end))

  })


# observeEvent( input$AUDIO2, {
# 
#   req( input$file1 )
# 
#   base64 <- dataURI(file = input$file1$datapath, mime = "audio/wav")
# 
#   insertUI( selector = "#AUDIO2", where = "afterEnd",
# 
#             ui = tags$audio( src = base64, type = "audio/wav", autoplay = NA, controls = NA )
#   )
# 
#  })

}


shinyApp(ui = ui, server = server)

#runApp("app.r")


# to deploy app
#library(rsconnect)
#deployApp()

#todo
# main panel with explanation and example and second panel with the interactive stuff
# work on code for complex quick flashers, one function can work for all with number of flashes as an input
# work on code for prolonged glows
# play only specific sections of audio

