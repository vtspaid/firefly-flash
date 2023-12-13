
library(dplyr)

singleflash <- function(wav, start=0, end=length(wav@left)/wav@samp.rate, quant=0.998,
                        species="sample", sample=1, site="site", temp=NA){
  starting=start*wav@samp.rate+1 # multiply the starting input by the sample rate to get starting frame
  ending=end*wav@samp.rate # get ending frame
  amp<-wav@left[starting:ending] # creates a vector of amplitudes using provided start and end times
 
   #create time array by dividing the current frame by the sample rate and adding the initial start time
  timeArray <- ((0:(length(amp)-1)) / FLASH@samp.rate)+start
   
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
  timeArray <- ((0:(length(amp)-1)) / FLASH@samp.rate)+start
  
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
