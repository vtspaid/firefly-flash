details_text <- function() {
  list(
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
                                   you will need to use the remove noise, and add flash options.")
  )
}

flash_example <- "<b>1.</b> Select the example flash type to explore.
<br><b>2.</b> Set The start and end times of the calculations on the left hand
side bar and hit \"Run flash caluclations\".
<br><b>3.</b> Play with the different inputs like 'amplitude', 'remove noise', and
'add flash' to see how the results change. You can hover over these buttons to 
see an explanation of what they do."

amplitude_text <- "This is the quantile of amplitude that is used to detect a
flash. Increase it to be more selective and decreaese it to be more inclusive."

rm_noise_text <- "Press this and select a time range to remove all noise from.
Multiple time ranges can be selected by pressing this button multiple times.
Useful for removing erroneuous flashes that can't be removed by changing the
amplitude quantile"

add_flash_text <- "If a flash is not being detected you can add a flash to a 
specific time with this button. There is no limit to how many can be added."

group_flash_text <- "This is the time difference used to dilineate groups. 
Flashes that occur within this time are considered apart of the same group.
Otherwise they will be considered part of separate groups"