
library(dplyr)
create_flash_array <- function(wav, # object of class wav, not a path
                               start = 0, # time in secons to start analysis
                               end = length(wav@left) / wav@samp.rate, # time in seconds
                               freq_min = 1000,
                               freq_max = 10000
                               ) {
  
  # remove frequency's outside of a range
  wav <-  fir(flash, from = freq_min, to = freq_max, bandpass = TRUE, output = "Wave")
  
  # multiply the starting input by the sample rate to get starting frame
  starting = start * wav@samp.rate
  
  # Make sure the start isn't 0
  if (starting == 0) {starting = 0.01}
  
  # get ending frame
  ending = end * wav@samp.rate 
  
  # creates a vector of amplitudes using provided start and end times
  amp <- wav@left[starting:ending]
  
  # create time array by dividing the current frame by the sample rate and 
  # adding the initial start time
  timeArray <- ((0:(length(amp) - 1)) / wav@samp.rate) + start
  
  #create data frame of time and amplitude
  timeamp <- data.frame(Time = timeArray, Amp = amp)
  
  return(timeamp)
}

singleflash <- function(wav, 
                        start = 0, 
                        end = length(wav@left) / wav@samp.rate, 
                        quant = 0.998,
                        species = "sample", 
                        sample = 1, 
                        site = "site", 
                        temp = NA) {
  
  # create time amp array
  timeamp <- create_flash_array(wav = wav, start = start, end = end)

  # keep only amplitudes in the top quantile specified by the quant argument
  timeamp <- timeamp[timeamp$Amp > quantile(timeamp$Amp, quant, na.rm = TRUE), ]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <- c(0, diff(timeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is 
  # less than .1 sec then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(samepeak = ifelse(timediff < .15, 0, 1)) %>% 
    mutate(samepeak = replace(samepeak, is.na(samepeak), 1)) %>%
    mutate(grouping = cumsum(samepeak))
  
  # find median time of each sound grouping
  peak <- timeamp %>% group_by(grouping) %>% summarise(peakTime = median(Time))
  
  # find time difference in the peak times
  peak$timediff <- c(0, diff(peak$peakTime))
  
  return(output <- data.frame(Species = species, 
                              sample = sample, 
                              site=site, 
                              temp = temp, 
                              mean_interval = mean(peak$timediff[-1]), 
                              max_interval = max(peak$timediff[-1]), 
                              min = min(peak$timediff[-1]), 
                              sd = sd(peak$timediff[2:length(peak$timediff)]),
                              flash_num = (nrow(peak) - 1)))

}

complexflash <- function(wav, 
                         start=0, 
                         end = length(wav@left) / wav@samp.rate, 
                         quant = 0.998,
                         species = "sample", 
                         sample = 1, 
                         site = "site",
                         temp = NA) {
  
  # create time amp array
  timeamp <- create_flash_array(wav = wav, start = start, end = end)
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp[timeamp$Amp > quantile(timeamp$Amp, quant, na.rm = TRUE), ]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <- c(0, diff(timeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is 
  # less than .15 sec then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(samepeak = ifelse(timediff < .15, 0, 1)) %>% 
    mutate(samepeak = replace(samepeak, is.na(samepeak), 1)) %>%
    mutate(grouping = cumsum(samepeak))
  
  # find median time of each sound grouping
  peak <- timeamp %>% group_by(grouping) %>% summarise(peakTime=median(Time))
  
  # find time difference in the peak times
  peak$timediff <- c(0, diff(peak$peakTime))
  
  # find the mean of time difference in flash timings
  breaktime <- mean(peak$timediff)
  
  # create complex flash groupings
  peak <- peak %>% mutate(samegroup = ifelse(timediff < breaktime, 0, 1)) %>%
    mutate(samegroup = replace(samegroup, is.na(samegroup), 1)) %>%
    mutate(grouping = cumsum(samegroup))
  
  # create df of just the interflash timings
  interflash <- peak[peak$timediff < breaktime, ]
  
  # create df of just the between timings
  betweengroup <- peak[peak$timediff > breaktime, ]
  
  # create df of number of flashes per group
  num_flashes <- peak %>% group_by(grouping) %>% summarise(n = n())
  
  flashdata <- data.frame(Species = species, 
                          sample = sample, 
                          site=site, 
                          temp = temp, 
                          mean_interval=mean(interflash$timediff[-1]),
                          mean_flash_num = mean(num_flashes$n),
                          max_interval = max(interflash$timediff[-1]), 
                          min = min(interflash$timediff[-1]), 
                          sd = sd(interflash$timediff[-1]),
                          flash_num = nrow(interflash))
  
  pausedata <- data.frame(Species = species, 
                          sample = sample, 
                          site=site, 
                          temp = temp,
                          mean_between_group = mean(betweengroup$timediff),
                          max_pause = max(betweengroup$timediff[-1]), 
                          min_pause = min(betweengroup$timediff[-1]), 
                          pause_sd = sd(betweengroup$timediff[-1]),
                          pause_num = nrow(betweengroup))
  
 return(list(interflash_data = flashdata, darkperiod_data = pausedata))
}


slowglow <- function(wav, start=0, 
                     end = length(wav@left) / wav@samp.rate, 
                     quant=0.998,
                     species="sample", 
                     sample=1, 
                     site="site", 
                     temp=NA) {

  # create time amp array
  timeamp <- create_flash_array(wav = wav, start = start, end = end)
  
  # create a df of time and amplitude for ending time
  endtimeamp <- data.frame(Time = timeamp$Time, Amp =timeamp$Amp)
  
  # keep only amplitudes in the top quantile specified by the quant argument
  timeamp <- timeamp[timeamp$Amp > quantile(timeamp$Amp, quant, na.rm = TRUE), ]
  
  # keep only amplitudes above the endquant
  endtimeamp <- endtimeamp[endtimeamp$Amp > quantile(endtimeamp$Amp, 
                                                     quant, na.rm = TRUE),]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <-c(0, diff(timeamp$Time))
  
  # get time difference between remaining high level amplitudes
  endtimeamp$timediff <-c(0, diff(endtimeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is 
  # less than .15 sec then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(sameflash = ifelse(timediff < .15, 0, 1))%>% 
    mutate(sameflash = replace(sameflash, is.na(sameflash), 1)) %>%
    mutate(grouping = cumsum(sameflash))
  
  # create groups based on the difference in time. If the difference in time is 
  # less than .1 sec then it will be placed in the same sound group.
  endtimeamp <- endtimeamp %>% 
    mutate(sameflash = ifelse(timediff < .3, 0, 1)) %>% 
    mutate(sameflash = replace(sameflash, is.na(sameflash), 1)) %>%
    mutate(grouping = cumsum(sameflash))
  
  # get glow start times
  glowstart <- timeamp %>% group_by(grouping) %>% summarise(start = min(Time))
  
  # get glow end times
  glowend <- endtimeamp %>% group_by(grouping) %>% summarise(end = max(Time))
  
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
  
  flashdata <- data.frame(Species = species, sample = sample, 
                          site=site, temp = temp, 
                          mean_glow=mean(glowtimes$length),
                          max_glow = max(glowtimes$length), 
                          min = min(glowtimes$length), 
                          sd = sd(glowtimes$length),
                          flash_num = nrow(glowtimes))
  
  pausedata <- data.frame(Species = species, 
                          sample = sample, site=site, temp = temp,
                          dark_period = mean(glowtimes$pause[-1]),
                          max_pause = max(glowtimes$pause[-1]), 
                          min_pause = min(glowtimes$pause[-1]), 
                          pause_sd = sd(glowtimes$pause[-1]),
                          pause_num = nrow(glowtimes) - 1)
  
  return(list(glow_data = flashdata, darkperiod_data = pausedata))
}




flashcheck <- function(wav, start = 0, 
                       end = length(wav@left) / wav@samp.rate, quant=0.998) {
  
  # create time amp array
  timeamp_df <- create_flash_array(wav = wav, start = start, end = end)
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp_df[timeamp_df$Amp > quantile(timeamp_df$Amp, quant, 
                                                  na.rm = TRUE),]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <-c(0, diff(timeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is 
  # less than .15 sec  then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(samepeak = ifelse(timediff < .15, 0, 1)) %>% 
    mutate(samepeak = replace(samepeak, is.na(samepeak), 1)) %>%
    mutate(grouping = cumsum(samepeak))
  
  # find median Time of each sound grouping
  peak <- timeamp %>% group_by(grouping) %>% summarise(peakTime = median(Time))
  
  flashcheck <- plot(x = timeamp_df$Time, 
                     y = timeamp_df$Amp, type = 'l', col = 'black', 
                     xlab = 'Seconds', ylab = 'Amplitude', xaxt = "n")
  axis(1, at = seq(1, round(max(timeamp_df$Time)), by = 2), las=2)
  abline(v=peak$peakTime, col = "red", lty = "dotted")
  abline(h=quantile(timeamp$Amp, quant, na.rm = TRUE), col = "blue")
  
  return(flashcheck)
}


complexcheck <- function(wav, start = 0, 
                         end = length(wav@left) / wav@samp.rate, 
                         quant=0.998) {
  
  # create time amp array
  timeamp_df <- create_flash_array(wav = wav, start = start, end = end)
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp_df[timeamp_df$Amp>quantile(timeamp_df$Amp, quant,
                                                na.rm = TRUE),]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <-c(0, diff(timeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is 
  # less than .15 sec then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(samepeak = ifelse(timediff < .15, 0, 1)) %>% 
    mutate(samepeak = replace(samepeak, is.na(samepeak), 1)) %>%
    mutate(grouping = cumsum(samepeak))
  
  # find median Time of each sound grouping
  peak <- timeamp %>% group_by(grouping) %>% summarise(peakTime = median(Time))
  
  # find time difference in the peak times
  peak$timediff <- c(0, diff(peak$peakTime))
  
  # find the mean of timedifference in flash timings
  breaktime <- mean(peak$timediff)
  
  # create complex flash groupings
  peak <- peak %>% mutate(samegroup = ifelse(timediff < breaktime, 0, 1)) %>%
    mutate(samegroup = replace(samegroup, is.na(samegroup), 1)) %>%
    mutate(grouping = cumsum(samegroup)+1)
  
  
  flashcheck <- plot(x = timeamp_df$Time, y = timeamp_df$Amp, type = 'l', 
                     col = 'black',
                     xlab = 'Seconds', ylab = 'Amplitude', xaxt="n")
  
  axis(1, at = seq(1, round(max(timeamp$Time)), by = 2), las = 2)
  
  segments(x0 = peak$peakTime, 
           x1 = peak$peakTime, 
           y0 = min(wav@left), 
           y1 = 0, 
           col = rep(c("blue", "red"), 
                     length(unique(peak$grouping)  /2))[peak$grouping])

  
  return(flashcheck)
}

glowcheck <- function(wav, start = 0, 
                      end = length(wav@left) / wav@samp.rate, 
                      quant = 0.998, endquant = quant) {
  
  # create time amp array
  timeamp_df <- create_flash_array(wav = wav, start = start, end = end)
  
  # create a df of time and amplitude for ending time
  endtimeamp_df <- create_flash_array(wav = wav, start = start, end = end)
  
  # keep only amplitudes in the top 1%
  timeamp <- timeamp_df[timeamp_df$Amp>quantile(timeamp_df$Amp, quant,
                                                na.rm = TRUE), ]
  
  # keep only amplitudes above the endquant
  endtimeamp <- endtimeamp_df[endtimeamp_df$Amp > 
                                quantile(endtimeamp_df$Amp, endquant,
                                         na.rm = TRUE),]
  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <-c(0, diff(timeamp$Time))
  
  # get time difference between remaining high level amplitudes
  endtimeamp$timediff <-c(0, diff(endtimeamp$Time))
  
  # create groups based on the difference in time. If the difference in time is 
  # less than .1 sec then it will be placed in the same sound group.
  timeamp <- timeamp %>% mutate(sameflash = ifelse(timediff < .15, 0, 1)) %>% 
    mutate(sameflash = replace(sameflash, is.na(sameflash), 1)) %>%
    mutate(grouping = cumsum(sameflash))
  
  # create groups based on the difference in time. If the difference in time is 
  # less than .1 sec then it will be placed in the same sound group.
  endtimeamp <- endtimeamp %>% 
    mutate(sameflash = ifelse(timediff < .3, 0, 1)) %>% 
    mutate(sameflash = replace(sameflash, is.na(sameflash), 1)) %>%
    mutate(grouping = cumsum(sameflash))
  
  # get glow start times
  glowstart <- timeamp %>% group_by(grouping) %>% summarise(start = min(Time))
  
  # get glow end times
  glowend <- endtimeamp %>% group_by(grouping) %>% summarise(end = max(Time))
  
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
  
  timeamp_df <- timeamp_df[seq(1, nrow(timeamp_df), 2), ]
  plot(x = timeamp_df$Time, y = timeamp_df$Amp, 
       type = 'l', col = 'black', 
       xlab = 'Seconds', ylab = 'Amplitude', xaxt = "n")
  axis(1, at = seq(1, round(max(timeamp_df$Time)), by = 2), las=2)
  segments(x0 = glowtimes$start, x1 = glowtimes$start, y0 = min(wav@left), 
           y1 = 0, 
           col="green")
  segments(x0 = glowtimes$end, x1 = glowtimes$end, y0 = min(wav@left), y1 = 0, 
           col="red")
}

