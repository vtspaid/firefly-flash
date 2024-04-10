### Code for creating shiny app that calculates statistics for firefly flashes ###

#load necessary packages
library(shiny)
library(tuneR)
library(dplyr)
library(base64enc)
library(howler)
library(shinyjs)


##### create two custom functions needed #####

singleflash <- function(wav, start=0, end=length(wav@left)/wav@samp.rate, quant=0.998){
  starting=start*wav@samp.rate+0.01 # multiply the starting input by the sample rate to get starting frame
  ending=end*wav@samp.rate # get ending frame
  amp<-wav@left[starting:ending] # creates a vector of amplitudes using provided start and end times
  
  #create time array by dividing the current frame by the sample rate and adding the initial start time
  timeArray <- ((0:(length(amp)-1)) / wav@samp.rate)+start
  
  #create dataframe of time and amplitude
  timeamp <- data.frame(Time=timeArray, Amp=amp)
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp[abs(timeamp$Amp) > quantile(timeamp$Amp, quant),]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <- c(0, diff(timeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is less than .15 sec
  # then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(samepeak = ifelse(timediff < .15, 0, 1))%>% 
    mutate(samepeak = replace(samepeak, is.na(samepeak), 1)) %>%
    mutate(grouping = cumsum(samepeak))
  
  # find median time of each sound grouping
  peak <- timeamp %>% group_by(grouping) %>% summarise(peakTime=median(Time))
  
  # find time difference in the peak times
  peak$timediff <- c(0, diff(peak$peakTime))
  
  return(output <- data.frame(mean_interval=mean(peak$timediff[-1]), max_interval = max(peak$timediff[-1]), 
                              min = min(peak$timediff[-1]), sd = sd(peak$timediff[2:length(peak$timediff)]),
                              flash_num = (nrow(peak)-1)))
  
}

complexflash <- function(wav, start=0, end=length(wav@left)/wav@samp.rate, quant=0.998, pause=NA){
  starting=start*wav@samp.rate+0.01 # multiply the starting input by the sample rate to get starting frame
  ending=end*wav@samp.rate # get ending frame
  amp<-wav@left[starting:ending] # creates a vector of amplitudes using provided start and end times
  
  #create time array by dividing the current frame by the sample rate and adding the initial start time
  timeArray <- ((0:(length(amp)-1)) / wav@samp.rate)+start
  
  #create dataframe of time and amplitude
  timeamp <- data.frame(Time=timeArray, Amp=amp)
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp[abs(timeamp$Amp) > quantile(timeamp$Amp, quant),]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <- c(0, diff(timeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is less than .01 sec
  # then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(samepeak = ifelse(timediff < .05, 0, 1))%>% 
    mutate(samepeak = replace(samepeak, is.na(samepeak), 1)) %>%
    mutate(grouping = cumsum(samepeak))
  
  # find median time of each sound grouping
  peak <- timeamp %>% group_by(grouping) %>% summarise(peakTime=median(Time))
  
  # find time difference in the peak times
  peak$timediff <- c(0, diff(peak$peakTime))
  
  # create breaktime
  if(hasArg(pause)) {breaktime = pause}  else{breaktime = mean(peak$timediff)*1.1}
  
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
  
  return(list(flashdata <- data.frame(mean_interval=mean(interflash$timediff[-1]),
                                      mean_flash_num = mean(num_flashes$n),
                                      max_interval = max(interflash$timediff[-1]), 
                                      min = min(interflash$timediff[-1]), sd = sd(interflash$timediff[-1]),
                                      flash_num = nrow(interflash)-1),
              
              pausedata <- data.frame(mean_between_group = mean(betweengroup$timediff),
                                      max_pause = max(betweengroup$timediff), 
                                      min_pause = min(betweengroup$timediff), 
                                      pause_sd = sd(betweengroup$timediff),
                                      pause_num = nrow(betweengroup))))
  
  
}

slowglow <- function(wav, start=0, end=length(wav@left)/wav@samp.rate, quant=0.98){
  starting=start*wav@samp.rate+.01 # multiply the starting input by the sample rate to get starting frame
  ending=end*wav@samp.rate # get ending frame
  amp<-wav@left[starting:ending] # creates a vector of amplitudes using provided start and end times
  
  #create time array
  timeArray <- ((0:(length(amp)-1)) / wav@samp.rate)+start
  
  #create dataframe of time and amplitude for starting time
  timeamp  <- data.frame(Time=timeArray, Amp=amp)
  
  # create a df of time and amplitude for ending time
  endtimeamp <- data.frame(Time=timeArray, Amp=amp)
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp[abs(timeamp$Amp) > quantile(timeamp$Amp, quant),]
  
  # keep only amplitudes above the quant
  endtimeamp <- endtimeamp[abs(endtimeamp$Amp) > quantile(endtimeamp$Amp, quant),]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <-c(0, diff(timeamp$Time))
  
  # get time difference between remaining high level amplitudes
  endtimeamp$timediff <-c(0, diff(endtimeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is less than .3 sec
  # then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(sameflash = ifelse(timediff < .3, 0, 1))%>% 
    mutate(sameflash = replace(sameflash, is.na(sameflash), 1)) %>%
    mutate(grouping = cumsum(sameflash))
  
  # create groups based on the difference in time. If the difference in time is less than .3 sec
  # then it will be placed in the same sound group.
  endtimeamp <- endtimeamp %>% mutate(sameflash = ifelse(timediff < .3, 0, 1))%>% 
    mutate(sameflash = replace(sameflash, is.na(sameflash), 1)) %>%
    mutate(grouping = cumsum(sameflash))
  
  # get glow start times
  glowstart <- timeamp %>% group_by(grouping) %>% summarise(start=min(Time))
  
  # get glow end times
  glowend <- endtimeamp %>% group_by(grouping) %>% summarise(end=max(Time))
  
  # merge start and end times
  glowtimes <- merge(glowstart, glowend, by = "grouping")
  
  # get time diff
  glowtimes$length <- glowtimes$end - glowtimes$start 
  
  # get df of glow lengths
  glowlength <- timeamp %>% group_by(grouping) %>% 
    summarise(glowlength = Time[which.max(Time)] - Time[which.min(Time)])
  
  # get the end of the last flash onto the same line as the next glow
  glowtimes <- glowtimes %>% mutate(last_end = lag(end))
  
  # get pause length
  glowtimes$pause <- glowtimes$start-glowtimes$last_end
  
  return(list(glow_data <- data.frame(mean_glow=mean(glowtimes$length),
                                                  max_glow = max(glowtimes$length), 
                                                  min = min(glowtimes$length), sd = sd(glowtimes$length),
                                                  flash_num = nrow(glowtimes)),
              
              darkperiod_data <- data.frame(dark_period = mean(glowtimes$pause[-1]),
                                                        max_pause = max(glowtimes$pause[-1]), 
                                                        min_pause = min(glowtimes$pause[-1]), 
                                                        pause_sd = sd(glowtimes$pause[-1]),
                                                        pause_num = nrow(glowtimes)-1)))
  
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
  timeamp <- timeamp[abs(timeamp$Amp) > quantile(timeamp$Amp, quant),]
  
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

complexflashcheck <- function(wav, start=0, end=length(wav@left)/wav@samp.rate, quant=0.998, pause=NA){
  starting=start*wav@samp.rate+1 # converts seconds into the correct sample number (have to add 1 or default would be invalid)
  ending=end*wav@samp.rate
  amp<-wav@left[starting:ending] # creates a vector of amplitudes using provided start and end times
  
  #create time array
  timeArray <- ((0:(length(amp)-1)) / wav@samp.rate)+start
  
  #create dataframe of time and amplitude
  timeamp  <-data.frame(Time=timeArray, Amp=amp)
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp[abs(timeamp$Amp) > quantile(timeamp$Amp, quant),]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <-c(0, diff(timeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is less than .1 sec
  # then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(samepeak = ifelse(timediff < .05, 0, 1))%>% 
    mutate(samepeak = replace(samepeak, is.na(samepeak), 1)) %>%
    mutate(grouping = cumsum(samepeak))
  
  # find median Time of each sound grouping
  peak <- timeamp %>% group_by(grouping) %>% summarise(peakTime=median(Time))
  
  # find time difference in the peak times
  peak$timediff <- c(0, diff(peak$peakTime))
  
  # find the mean of time difference in flash timings
  if(hasArg(pause)) {breaktime = pause}  else{breaktime = mean(peak$timediff)*1.1}
  
  # create complex flash groupings
  peak <- peak %>% mutate(samegroup = ifelse(timediff < breaktime, 0, 1)) %>%
    mutate(samegroup = replace(samegroup, is.na(samegroup), 1)) %>%
    mutate(grouping = cumsum(samegroup)+1) 
  
  
  flashcheck <- plot(x=timeArray, y=amp, type='l',
                     col='black', xlab='Seconds', ylab='Amplitude', xaxt="n")
  axis(1, at = seq(1, round(max(timeArray)), by = 2), las=2)
  segments(x0=peak$peakTime, x1= peak$peakTime, y0=min(wav@left), y1=0, 
           col=rep(c("blue", "red"), length(unique(peak$grouping)/2))[peak$grouping])
  
  
  return(flashcheck)
}

glowcheck <- function(wav, start=0, end=length(wav@left)/wav@samp.rate, quant=0.98){
  starting=start*wav@samp.rate+1 # converts seconds into the correct sample number (have to add 1 or default would be invalid)
  ending=end*wav@samp.rate
  amp<-wav@left[starting:ending] # creates a vector of amplitudes using provided start and end times
  
  #create time array
  timeArray <- ((0:(length(amp)-1)) / wav@samp.rate)+start
  
  #create dataframe of time and amplitude for starting time
  timeamp  <-data.frame(Time=timeArray, Amp=amp)
  
  # create a df of time and amplitude for ending time
  endtimeamp <- data.frame(Time=timeArray, Amp=amp)
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp[abs(timeamp$Amp) > quantile(timeamp$Amp, quant),]
  
  # keep only amplitudes above the quant
  endtimeamp <- endtimeamp[endtimeamp$Amp > quantile(endtimeamp$Amp, quant),]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <-c(0, diff(timeamp$Time))
  
  # get time difference between remaining high level amplitudes
  endtimeamp$timediff <-c(0, diff(endtimeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is less than .1 sec
  # then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(sameflash = ifelse(timediff < .3, 0, 1))%>% 
    mutate(sameflash = replace(sameflash, is.na(sameflash), 1)) %>%
    mutate(grouping = cumsum(sameflash))
  
  # create groups based on the difference in time. If the difference in time is less than .1 sec
  # then it will be placed in the same sound group.
  endtimeamp <- endtimeamp %>% mutate(sameflash = ifelse(timediff < .3, 0, 1))%>% 
    mutate(sameflash = replace(sameflash, is.na(sameflash), 1)) %>%
    mutate(grouping = cumsum(sameflash))
  
  # get glow start times
  glowstart <- timeamp %>% group_by(grouping) %>% summarise(start=min(Time))
  
  # get glow end times
  glowend <- endtimeamp %>% group_by(grouping) %>% summarise(end=max(Time))
  
  # merge start and end times
  glowtimes <- merge(glowstart, glowend, by = "grouping")
  
  # get time diff
  glowtimes$length <- glowtimes$end - glowtimes$start 
  
  # get df of glow lengths
  glowlength <- timeamp %>% group_by(grouping) %>% 
    summarise(glowlength = Time[which.max(Time)] - Time[which.min(Time)])
  
  # get the end of the last flash onto the same line as the next glow
  glowtimes <- glowtimes %>% mutate(last_end = lag(end))
  
  # get pause length
  glowtimes$pause <- glowtimes$start-glowtimes$last_end
  
  
  
  flashcheck <- plot(x=timeArray, y=amp, type='l',
                     col='black', xlab='Seconds', ylab='Amplitude', xaxt="n")
  axis(1, at = seq(1, round(max(timeArray)), by = 2), las=2)
  segments(x0 = glowtimes$start, x1 = glowtimes$start, y0=min(wav@left), y1=0, 
           col="green")
  segments(x0 = glowtimes$end, x1 = glowtimes$end, y0=min(wav@left), y1=0, 
           col="red")
  
  
  
  return(flashcheck)
}


###### UI #####


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
 
  
  # tags$div(img(src='potomaca_pic.png', height='10%', width='10%', style="position:absolute; top:0px; right:0")),

  # Sidebar layout with input and output definitions ----
    sidebarPanel(
      tabsetPanel(
        tabPanel("Plot start and end times",
                  # the next two lines are inputs for the xlim of the audio plot. The end gets updated later
                  numericInput("start", "plot start time", value = 0, min = 0, max = 10000),
                  numericInput("end", "plot end time", value = 10, min = 1, max = 10000),

        actionButton("AUDIO2", "Play audio"),
        actionButton("clearaudio", "Remove audio player")),

      
        tabPanel("Flash calculations",
                 numericInput("tstart", "start time", value = 0, min = 1, max = 30), # start time of recording to use
                 numericInput("tend", "end time", value = 10, min = 1, max = 10000), # end time of recording to use
                 numericInput("quant", "amplitude quantile", value=0.999, min=0.85, max=1, step = 0.001), # quantile to use for amplitude cutoff
                 numericInput("pause", "Group flashes if less than x seconds", value=1, min=0, max=100, step=0.1),
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
      
      # Output: Histogram ----
      tabsetPanel(
        tabPanel( "Example",
                  br(),
                  h2("Flash Calculation Example"),
                  br(),
                  h4("This page is non-interactive"),
                  h4("After reviewing the examples on this page move to the 'Run Flash Calculations' tab to run calculations
                   on your audio file"),
                  br(),
                  p("One quick note, this page was put together before some updates were made to the code. That changed
                  the figure appearances. so don't worry if you're figures don't look quite like these ones."),
                  br(),br(),
                  p("First use the browse button to find and upload your audio file."),
                  p("On 'Run' tab a plot of your audio file will automatically render. It will look like the figure below."),
                  tags$div(img(src='singleflash_with_error_1.png', height='80%', width='80%')),
                  br(),
                  p("You can control the beginning and ending of this plot with the first tab on the left.
                    The inputs on this tab only control the x axis of the first plot, they do not affect
                    the flash calculations in any way."),
                  br(), br(),
                  p("To control how the flash statistics are calculated, swithc to the 'Flash
                    calculations' tab on the left. There you will see a number of inputs that you can modify to change
                 how the flash statistics are calculated. You can use the default settings and hit the 'run flash
                    calculations' button at the bottom of the tab. That will produce a graph similar to this one"),
                 img(src='singleflash_with_error_calc_plot.png', height='80%', width='80%'),
                 br(),
                 p("It will also produce a table like this one"),
                 img(src='singleflash_with_error_table.png', height='80%', width='80%'),
                 br(), br(),
                 p("The red lines show you where the function believes flashes occurred. We see that there is
                   an erroneous flash at around the 9 second mark in this example. We can use the 'remove noise'
                   button to supply a time range where we wish to remove all noise.
                   In this example we would use 8.7 and 8.9. We can now run
                   the function again and it will produce a plot like the one below. The table will also be updated."),
                  img(src='singleflash_witherror_fixed.png', height='80%', width='80%'),
                 br(), br(),
                 p("We can use the 'add flash' button to add a flash if the code is missing one."),
                 br(), p("The radio buttons will select which of three functions to use on the audio
                         based on what type of flash pattern you belive it to be. The amplitude quantile
                         controls how loud a noise is before it is considered a flash; slight changes make a big
                         differences, best results tend to be between 0.995 and 0.999. The 'Group flashes if less than x seconds' only 
                         comes into play when using the 'complex flash' function. The 'slowglow end quantile'
                         is only relevant if the 'slow glow' function is being used, and should usually be made equal
                         to the 'amplitude quantile'. Species, sample#, site name, and temperature are not necessary
                         for the code to run, and are only there to make it easier to copy and paste the table to a spreadsheet.
                         ")
                  ),

        tabPanel( "Run Flash Calculations",
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
      plotOutput(outputId = "resultsplot")),
      
      tabPanel("help info",
               p("This will eventually provide detailed help documentation"))
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
    # if (is.null(infile)) {
    #   return(NULL)
    # }
    audio <- if(grepl('.*\\.wav', ignore.case=TRUE, infile$datapath)) {readWave(infile$datapath)} else
      {readMP3(infile$datapath)}
   # audio <- readWave(infile$datapath)
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
    train_audio = FLASH()
    str(train_audio)
    s1 <- train_audio/2^(train_audio@bit -1)
    timeArray <- (0:(length(train_audio@left)-1)) / train_audio@samp.rate
    #Plot the wave
    plot(x=timeArray, y=train_audio@left, type='l',
         col='black', xlab='Seconds', ylab='Amplitude', xlim=c(input$start, input$end))
    
  })

  # single flash
  # create table of flash statistics
  output$flash_stats  <- renderTable({
    input$flash_calc
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
                                 start=input$tstart, end=input$tend, quant=input$quant
    )})

            
                                })

  # create plot checking where the code thinks the flashes are
  output$resultsplot <- renderPlot({
    input$flash_calc
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
    } else {glowcheck(df2, start=input$tstart, end=input$tend, quant=input$quant)})

  })
  

}


shinyApp(ui = ui, server = server)
#runApp("app.r")


# to deploy app
#library(rsconnect)
#deployApp()

#todo
# fix complex flash so that it allows user to specify interval to consider a grouping
# fix bug where audio continues playing even after ui is removed
# add detailed help
runApp()

