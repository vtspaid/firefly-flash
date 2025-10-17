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

flash_stats_text <- "Red lines represent single flashes. If the complex flash 
method was chosen there will be red and blue lines representing groups of
flashes. If glow method was chosen the green line represents the start of a 
flash and the red line represents the end."

single_flash_example <- "<b>1.</b> Set The start and end times of the calculations to
0 and 32 and amplitude to 0.999 and hit \"Run flash caluclations\"
<br><b>2.</b> Set the amplitude to 0.998 and observe the result. Note the extra flash 
before 9 seconds. Hit the \"Remove noise\" button and set the values to 8.5 and
 8.9 and run the calculations again.
<br><b>3.</b> Now set the amplitude to 0.9999 and observe the results. Hit the 
\"Add Flash\" button twice and enter 16.4 and 29 and run the calculations again.
<br><b>4.</b> Hit the \"Restore noise\" and \"Remove added flash\" button to reset 
everything"

complex_flash_example <- "<b>1.</b> Set The start and end times of the 
calculations to 0 and 45 and amplitude to 0.999 and hit \"Run flash caluclations\"
<br><b>2.</b> Change the \"Flash pattern\" on the side bar to complex flash and 
try again.
<br><b>3.</b> Note the extra flashes from 9 to 10 seconds. 
 Hit the \"Remove noise\" button and set the values from 9.1 to 10 and run the 
 calculations again.
<br><b>4.</b> Enter different values into the \"Group flashes\" box
and explore the results. Try 0.2, 1, 1.5, and 2."

glow_flash_example <- "<b>1.</b> Set The start and end times of the 
calculations to 20 and 55 and amplitude to 0.999 and hit \"Run flash caluclations\"
<br><b>2.</b> Change the \"Flash pattern\" on the side bar to glow and 
try again.
<br><b>3.</b> Lower the amplitude to 0.998 and run again."
