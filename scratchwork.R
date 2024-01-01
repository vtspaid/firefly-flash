# Scratch pad for function ideas for fiRefly package that calculates flash statistices for you based on
# audio recordings

# install necessary packages 
#install.packages("av")
library(av)

# set working directory
setwd("C:/Users/vtspa/Documents/Professional/Firefly_research/firefly-flash")

# read in audio file
FLASH <- paste0(getwd(),"/Dunkard_potomaca.WAV")
av_media_info(FLASH)

# Read initial 5 sec as as frequency spectrum
fft_data <- read_audio_fft(FLASH)
dim(fft_data)
#> [1] 512 860

# Plot the spectrogram
plot(fft_data)

# read into binary PCM
pcm_data <- read_audio_bin(FLASH, channels = 1)
plot(pcm_data, type = 'l')


#install.packages("tuneR")
library(tuneR)
#library(readr)

FLASH <- paste0(getwd(),"/Dunkard_potomaca.WAV")
train_audio = readWave(FLASH)
str(train_audio)
s1 <- train_audio/2^(train_audio@bit -1)
timeArray <- (0:(length(train_audio@left)-1)) / train_audio@samp.rate
#Plot the wave
plot(x=timeArray, y=train_audio@left, type='l',
     col='black', xlab='Seconds', ylab='Amplitude', xlim=c(8,38))
abline(h=15000)

#create object of class Wspec
flashwspec <-periodogram(FLASH)

# estimate fundamental frequencies
FF(flashwspec, peakheight = 0.01, silence = 0.2, minpeak = 9, diapason = 440,
   notes = NULL, interest.frqs = seq(along = flashwspec@freq),
   search.par = c(0.8, 10, 1.3, 1.7))



######################################## TEST NEW STUFF
library(tuneR)
FLASH = readWave(paste0(getwd(),"/glowtest.WAV"))

timeArray <- (0:(length(FLASH@left)-1)) / FLASH@samp.rate

#Plot the wave
plot(x=timeArray, y=FLASH@left, type='l',
     col='black', xlab='Seconds', ylab='Amplitude', xaxt="n", xlim=c(12,40))
axis(1, at = seq(1, round(max(timeArray)), by = 2), las=2)


singleflash(FLASH, start=33, end=53, species="fairchildi", sample=1, site="tom's run", temp=65)

flashcheck(FLASH, start=33, end=53)



plot(x=timeArray[100000:110000], y=ampt[100000:110000], type='l',
     col='black', xlab='Seconds', ylab='Amplitude')
abline(h=quantile(timeamp$Amp, .99))

top99 <- timeamp[timeamp$Amp>quantile(timeamp$Amp, .99),]
top99$timediff <-c(0, diff(top99$Time))
quantile(top99$timediff, c(.9,0.91, 0.92, 0.93, 0.94, 0.95, .96, .97, .98, .99))

top99new <- top99[top99$timediff>0.1,]
top99new$timediff <-c(0, diff(top99new$Time))

top99new <- top99 %>% mutate(under_10 = ifelse(timediff < .1, 0, 1))%>% 
  mutate(under_10 = replace(under_10, is.na(under_10), 1)) %>%
  mutate(grouping = cumsum(under_10))
library(dplyr)

test <- top99new %>% group_by(grouping) %>% summarise(peakTime=median(Time))

FLASH <- readWave("Dunkard_potomaca.WAV")

rmnoise <- function(x, y, samprate) {x@left[c(eval(as.name(paste0("rmstart" , y)))*samprate):
                                              c(eval(as.name(paste0("rmend" , y)))*samprate)] <- 0
return(x@left)
}


testme <- lapply(1:counter, function(x) data.frame(start = eval(as.name(paste0("rmstart",x))),
                                                 end = eval(as.name(paste0("rmend",x)))))
f <- FLASH@left
temp <-list()

for (ii in 1:length(testme)){
  temp[[ii]] <- f
  temp[[ii]][c(testme[[ii]]$start*samprate):c(testme[[ii]]$end*samprate)] <- 0
  temp[[ii+1]] <- temp[[ii]]
  f <- temp[[ii+1]]
}
plot(f[c(0.1*samprate):c(18*samprate)])

f <- FLASH
for (ii in 1:length(testme)){
  f@left[c(testme[[ii]]$start*samprate):c(testme[[ii]]$end*samprate)] <- 0
}
plot(f@left[c(0.1*samprate):c(18*samprate)])

test  <- lapply( function)

 test <- rmnoise(x=FLASH, y=1, samprate=samprate)
plot(test@left[c(rmstart1*samprate):c(rmend1*samprate)])

s <- lapply(1:counter,function(x) rmnoise(x=FLASH, y=x, samprate=samprate))

plot(s[[2]])

FLASH2 <- FLASH@left-s
