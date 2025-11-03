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


mysegments <- function(peak, amp, mycol) {
  segments(x0 = peak, x1 = peak, y0 = max(amp), y1 = min(amp), col = mycol)
}


# Select the amplitudes from the left channel between the start and end
get_amps <- function(wav, start, end) {
  starting = (start + 0.01) * wav@samp.rate
  ending = end * wav@samp.rate
  wav@left[starting:ending]
}

create_timeamp <- function(amp, timeArray, quant, flashtype) {
  # Create dataframe of time and amplitude
  timeamp <- data.frame(Time = timeArray, Amp = amp)

  # keep only amplitudes above the specified quantile
  if (quant == 1) {
    timeamap <- timeamp[1, ]
  } else {
    timeamp <- timeamp[abs(timeamp$Amp) > quantile(timeamp$Amp, quant),]
  }

  
  # get time difference between remaining high level amplitudes
  timeamp$timediff <- c(0, diff(timeamp$Time))
  
  timer <- ifelse(flashtype == "glow", 0.3, 0.05)
  
  # create groups based on the difference in time. 
  timeamp <- timeamp %>% mutate(samepeak = ifelse(timediff < timer, 0, 1))%>% 
    mutate(samepeak = replace(samepeak, is.na(samepeak), 1)) %>%
    mutate(grouping = cumsum(samepeak))
}

create_peak <- function(timeamp, synth = NULL, rm_flash, dif, quant) {
  
  if (quant == 1 && is.null(synth)) {
    return(data.frame(grouping = 0, peakTime = NA))
  } else if (quant == 1) {
    peakTime <- synth
  } else {
    # find median time of each sound grouping
    peak <- timeamp %>% group_by(grouping) %>% summarise(peakTime=median(Time))
    peakTime <- peak$peakTime
  }
  
  if (length(synth > 0)) {
    synth2 <- synth[purrr::map_vec(synth, 
                                  function(x) !any(abs(x - peakTime) < dif))]
    peakTime <- sort(c(peakTime, synth2))
  }
  
  if (length(rm_flash) > 0) {
    for (i in rm_flash) {
      peakTime <- peakTime[abs(peakTime - i) > dif]
    }
  }
  
  group <- 1:length(peakTime) - 1
  peak <- data.frame(grouping = group, peakTime = peakTime)
  
  # find time difference in the peak times
  peak$timediff <- c(0, diff(peak$peakTime))
  
  peak
}

##### create a custom function needed later #####
signcheck <- function(x) {sum(sign(x) != lag(sign(x)), na.rm=T)}

# create functions for analyzing flash patterns
flashcalc <- function(wav, 
                      start = 0, 
                      end = length(wav@left)/wav@samp.rate, 
                      quant = 0.998,
                      pause = 1,
                      flashtype,
                      synth = NULL,
                      rm_flash = NULL) {
  # Get only the amplitude values between the starting and ending times
  amp <- get_amps(wav, start ,end) 
  
  dif <- (end - start) / 250
  synth <- synth[synth >= start & synth <= end]
  if (length(synth) == 0) synth <- NULL
  if (all(is.na(synth))) synth <- NULL
  rm_flash <- rm_flash[rm_flash > start & rm_flash < end]
  # Create time array 
  timeArray <- ((0:(length(amp)-1)) / wav@samp.rate) + start
  
  # Create dataframe of time and amplitude
  timeamp <- create_timeamp(amp, timeArray, quant, flashtype)
  
  # Find median time of each sound grouping (peak) and the difference them
  peak <- create_peak(timeamp, synth, rm_flash, dif, quant)
  
  if (flashtype == "complex flash") {
    
    # create complex flash groupings
    peak <- peak %>% mutate(samegroup = ifelse(timediff < pause, 0, 1)) %>%
      mutate(samegroup = replace(samegroup, is.na(samegroup), 1)) %>%
      mutate(grouping = cumsum(samegroup))
  } else {
    breaktime <- NULL
  }
  
  if (flashtype == "glow") {
    # get glow start times
    glowstart <- timeamp %>% group_by(grouping) %>% summarise(start=min(Time))
    
    # get glow end times
    glowend <- timeamp %>% group_by(grouping) %>% summarise(end=max(Time))
    
    st_ends <- sort(c(glowstart$start, glowend$end, synth))
    
    if (length(rm_flash) > 0) {
      for (i in rm_flash) {
        st_ends <- st_ends[abs(st_ends - i) > dif]
      }
    }
    gstarts = st_ends[c(TRUE, FALSE)]
    gends = st_ends[c(FALSE, TRUE)]
    if (length(gends) < length(gstarts)) {
      gends = c(gends, NA)
    }
    glowtimes <- data.frame(grouping = 1:ceiling(length(st_ends)/2),
                            start = gstarts,
                            end = gends)
    
    # get time diff
    glowtimes$length <- glowtimes$end - glowtimes$start 
    
    # get df of glow lengths
    glowlength <- timeamp %>% group_by(grouping) %>% 
      summarise(glowlength = Time[which.max(Time)] - Time[which.min(Time)])
    
    # get the end of the last flash onto the same line as the next glow
    glowtimes <- glowtimes %>% mutate(last_end = lag(end))
    
    # get pause length
    glowtimes$pause <- glowtimes$start-glowtimes$last_end
  } else {
    glowtimes = NULL
  }
  
  list(timeArray = timeArray,
       amp = amp,
       peak = peak, 
       wav = wav,
       breaktime = pause,
       glowtimes = glowtimes,
       flashtype = flashtype)
}

flashcalc_df <- function(peak) {
  if (peak$flashtype == "single flash") {
  data1 <- data.frame(mean_interval = mean(peak$peak$timediff[-1]),
                      max_interval = max(peak$peak$timediff[-1]), 
                      min = min(peak$peak$timediff[-1]),
                      sd = sd(peak$peak$timediff[2:length(peak$peak$timediff)]),
                      flash_num = (nrow(peak$peak) - 1))
  }
  
  if (peak$flashtype == "complex flash") {
    # create df of just the interflash timings
    interflash <- peak$peak[peak$peak$timediff < peak$breaktime, ]
    
    # create df of just the between timings
    betweengroup <- peak$peak[peak$peak$timediff > peak$breaktime, ]
    
    # create df of number of flashes per group
    num_flashes <- peak$peak %>% group_by(grouping) %>% summarise(n=n())
    
    data1 <- data.frame(mean_interval=mean(interflash$timediff[-1]),
                        mean_flash_num = mean(num_flashes$n),
                        max_interval = max(interflash$timediff[-1]), 
                        min = min(interflash$timediff[-1]), 
                        sd = sd(interflash$timediff[-1]),
                        flash_num = nrow(interflash) - 1,
                        mean_flash_num = mean(num_flashes$n),
                        mean_between_group = mean(betweengroup$timediff),
                        max_pause = max(betweengroup$timediff), 
                        min_pause = min(betweengroup$timediff), 
                        pause_sd = sd(betweengroup$timediff),
                        pause_num = nrow(betweengroup))
  }
  
  if (peak$flashtype == "glow") {
    data1 <- data.frame(mean_glow = mean(peak$glowtimes$length),
               max_glow = max(peak$glowtimes$length), 
               min = min(peak$glowtimes$length), 
               sd = sd(peak$glowtimes$length),
               flash_num = nrow(peak$glowtimes),
               dark_period = mean(peak$glowtimes$pause[-1]),
               max_pause = max(peak$glowtimes$pause[-1]), 
               min_pause = min(peak$glowtimes$pause[-1]), 
               pause_sd = sd(peak$glowtimes$pause[-1]),
               pause_num = nrow(peak$glowtimes) - 1)
  }
  data1
}

flashcalc_plot <- function(data) {
  flashcheck <- flash_plot(data$timeArray, data$amp)
  if (data$flashtype == "complex flash") {
    flashtime <- data$peak$peakTime
    mycols <- rep(c("blue", "red"),
                  length(unique(data$peak$grouping + 1)/2))[data$peak$grouping + 1]
  } else if (data$flashtype == "single flash") {
    flashtime <- data$peak$peakTime
    mycols <- "red"
  } else {
    flashtime <- c(data$glowtimes$start, data$glowtimes$end)
    mycols <- rep(c("green", "red"), each = length(flashtime)/2)
  }
  mysegments(flashtime, data$wav@left, mycols)
}
