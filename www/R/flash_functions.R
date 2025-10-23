# Store functions

flash_plot <- function(timeArray, amp, xlims = NULL) {
  
  if (is.null(xlims)) xlims <- c(timeArray[1], timeArray[length(timeArray)])

  par(mar = c(3, 3, 1, 0), mgp = c(1.5, 0.5, 0))
  fun_plot <-  plot(x = timeArray, 
                    y = amp, 
                    type = 'l',
                    col = 'black', 
                    xlab = 'Seconds', 
                    ylab = 'Amplitude',
                    xlim = xlims)
}


mysegments <- function(peaktime, amp, mycol) {
  segments(x0 = peaktime, 
           x1 = peaktime, 
           y0 = max(amp),
           y1 = min(amp),
           col = mycol)
}


# Select the amplitudes from the left channel between the start and end
get_amps <- function(wav, start, end) {
  starting = (start + 0.01) * wav@samp.rate
  ending = end * wav@samp.rate
  wav@left[starting:ending]
}

create_timeamp <- function(amp, timeArray, quant) {
  # Create dataframe of time and amplitude
  timeamp <- data.frame(Time = timeArray, Amp = amp)
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp[abs(timeamp$Amp) > quantile(timeamp$Amp, quant),]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <- c(0, diff(timeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is less than .01 sec
  # then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(samepeak = ifelse(timediff < .05, 0, 1))%>% 
    mutate(samepeak = replace(samepeak, is.na(samepeak), 1)) %>%
    mutate(grouping = cumsum(samepeak))
}

create_peak <- function(timeamp) {
  # find median time of each sound grouping
  peak <- timeamp %>% group_by(grouping) %>% summarise(peakTime=median(Time))
  
  # find time difference in the peak times
  peak$timediff <- c(0, diff(peak$peakTime))
  
  peak
}

##### create a custom function needed later #####
signcheck <- function(x) {sum(sign(x) != lag(sign(x)), na.rm=T)}

# create functions for analyzing flash patterns
singleflash <- function(wav, 
                        start = 0, 
                        end = length(wav@left)/wav@samp.rate, 
                        quant = 0.998) {
  # Get only the amplitude values between the starting and ending times
  amp <- get_amps(wav, start ,end) 
  
  # Create time array 
  timeArray <- ((0:(length(amp)-1)) / wav@samp.rate) + start
  
  # Create dataframe of time and amplitude
  timeamp <- create_timeamp(amp, timeArray, quant)
  
  # Find median time of each sound grouping (peak) and the difference them
  peak <- create_peak(timeamp)
  
  list(timeArray = timeArray,
       amp = amp,
       peak = peak, 
       wav = wav)
}


singleflash_plot <- function(data) {
  flashcheck <- flash_plot(data$timeArray, data$amp)
  mysegments(data$peak$peakTime, data$wav@left, "red")
}

complexflash <- function(wav, 
                         start = 0, 
                         end = length(wav@left)/wav@samp.rate, 
                         quant = 0.998, 
                         pause = 1) {
  # Get only the amplitude values between the starting and ending times
  amp <- get_amps(wav, start ,end) 
  
  # Create time array by dividing the current frame by the sample rate and adding the initial start time
  timeArray <- ((0:(length(amp)-1)) / wav@samp.rate)+start
  
  #create dataframe of time and amplitude
  timeamp <- create_timeamp(amp, timeArray, quant)
  
  # Find median time of each sound grouping (peak) and the difference them
  peak <- create_peak(timeamp)
  
  # create breaktime
  if(hasArg(pause)) {breaktime = pause}  else{breaktime = mean(peak$timediff)*1.1}
  
  # create complex flash groupings
  peak <- peak %>% mutate(samegroup = ifelse(timediff < breaktime, 0, 1)) %>%
    mutate(samegroup = replace(samegroup, is.na(samegroup), 1)) %>%
    mutate(grouping = cumsum(samegroup))
  
  list(timeArray = timeArray,
       amp = amp,
       peak = peak, 
       wav = wav,
       breaktime = breaktime)
}

complexflash_df <- function(peak, breaktime) {
  # create df of just the interflash timings
  interflash <- peak[peak$timediff < breaktime,]
  
  # create df of just the between timings
  betweengroup <- peak[peak$timediff > breaktime,]
  
  # create df of number of flashes per group
  num_flashes <- peak %>% group_by(grouping) %>% summarise(n=n())
  
  data.frame(mean_interval=mean(interflash$timediff[-1]),
             mean_flash_num = mean(num_flashes$n),
             max_interval = max(interflash$timediff[-1]), 
             min = min(interflash$timediff[-1]), sd = sd(interflash$timediff[-1]),
             flash_num = nrow(interflash)-1,
             mean_between_group = mean(betweengroup$timediff),
             max_pause = max(betweengroup$timediff), 
             min_pause = min(betweengroup$timediff), 
             pause_sd = sd(betweengroup$timediff),
             pause_num = nrow(betweengroup))
}

complexflash_plot <- function(data) {
  flashcheck <- flash_plot(data$timeArray, data$amp)
  mycols <- rep(c("blue", "red"),
                length(unique(data$peak$grouping + 1)/2))[data$peak$grouping + 1]
  mysegments(data$peak$peakTime, data$wav@left, mycols)
}

slowglow <- function(wav, 
                     start = 0, 
                     end = length(wav@left)/wav@samp.rate, 
                     quant = 0.98, 
                     freq = 9) {
  # Get only the amplitude values between the starting and ending times
  amp <- get_amps(wav, start ,end) 
  
  #create time array
  timeArray <- ((0:(length(amp)-1)) / wav@samp.rate)+start
  
  #create dataframe of time and amplitude for starting time
  timeamp  <- data.frame(Time=timeArray, Amp=amp)
  
  # create frequency column
  timeamp$freq <- zoo::rollapply(timeamp$Amp, FUN = signcheck, 
                                 width = wav@samp.rate/100,
                                 by = 10, 
                                 fill = c("extend", "extend", "extend"))
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp[abs(timeamp$Amp) > quantile(timeamp$Amp, quant),]
  
  # remove variances in the bottom 2%
  timeamp <- timeamp[timeamp$freq > freq,]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <-c(0, diff(timeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is less than .3 sec
  # then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(sameflash = ifelse(timediff < 0.3, 0, 1))%>% 
    mutate(sameflash = replace(sameflash, is.na(sameflash), 1)) %>%
    mutate(grouping = cumsum(sameflash))
  
  # get glow start times
  glowstart <- timeamp %>% group_by(grouping) %>% summarise(start=min(Time))
  
  # get glow end times
  glowend <- timeamp %>% group_by(grouping) %>% summarise(end=max(Time))
  
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
  
  list(timeArray = timeArray,
       amp = amp,
       glowtimes = glowtimes, 
       wav = wav)
}

slowglow_df <- function(glowtimes) {
  data.frame(mean_glow = mean(glowtimes$length),
             max_glow = max(glowtimes$length), 
             min = min(glowtimes$length), 
             sd = sd(glowtimes$length),
             flash_num = nrow(glowtimes),
             dark_period = mean(glowtimes$pause[-1]),
             max_pause = max(glowtimes$pause[-1]), 
             min_pause = min(glowtimes$pause[-1]), 
             pause_sd = sd(glowtimes$pause[-1]),
             pause_num = nrow(glowtimes) - 1)
}

slowglow_plot <- function(data) {
  flashcheck <- flash_plot(data$timeArray, data$amp)
  flashtime <- c(data$glowtimes$start, data$glowtimes$end)
  mysegments(flashtime, 
             data$wav@left, 
             rep(c("green", "red"), length(flashtime)))
}
