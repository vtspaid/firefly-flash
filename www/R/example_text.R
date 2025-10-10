example_text <- function() {
  list(
br(),
h2("Flash Calculation Example"),
br(),
h4("This page is non-interactive"),
h4("After reviewing the examples on this page move to the 'Run Flash Calculations' tab to run calculations
                   on your audio file"),
br(),
br(),br(),
p("First use the browse button at the top left of the page to find and upload your audio file. 
                    File size is currently limited to 15 MB"),
p("A plot of your audio file will automatically render. It will look similar the figure below."),
tags$div(img(src='singleflash_with_error_1.png', height='80%', width='80%')),
br(),
p("You can control the x axis of this plot with the first tab on the left. This plot is only for visualization
                    purposes. The start and end times that control this plot, do not set the start and end time of the flash 
                    calculation functions"),
br(), br(),
p("To control how the flash statistics are calculated, switch to the 'Flash
                    calculations' tab on the left. There you will see a number of inputs that you can modify to change
                 how the flash statistics are calculated. The flash statistics will not be calculated until you hit the 
                    'Run Flash Calculations' button. Once you do a plot similar to the one shown below will render"),
img(src='singleflash_with_error_calc_plot2.png', height='80%', width='80%'),
br(), br(),
p("It will also produce a table containing results"),
img(src='table_with_error.png', height='50%', width='50%'),
br(), br(),
p("The red lines are where the function detected a flash. We see that there is
                   an erroneous flash at around the 9 second mark in this example. We can use the 'remove noise'
                   button to supply a time range where we wish to remove all noise.
                   In this example we would use 8.7 and 8.9. Running
                   the function again will produce a plot like the one below."),
img(src='singleflash_witherror_fixed2.png', height='80%', width='80%'),
br(), br(),
p("The table will also be updated. Compare the minimum flash interval with the previous table."),
  img(src='table_with_error_fixed.png', height='50%', width='50%'),
  br(), br(),
  p("We can also use the 'add flash' button to add a flash into the plot if the function is not identifying 
                   all flashes"),
  br(), p("The radio buttons will select which of three functions to use
                         based on what type of flash pattern you recorded. The amplitude quantile
                         controls how loud a noise is before it is considered a flash; slight changes make a big
                         differences, best results tend to be between 0.995 and 0.999. The 'Group flashes if less than x seconds' only 
                         comes into play when using the 'complex flash' function, and sets the cut off point for when flashes will be 
                         considered part of the same group.
                         "),
  br(), br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
  )
}


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
                                   you will need to use the remove noise, and add flash options."),
    br(),br(),
    h4("remove noise/restore noise:"),
    p("Hitting the remove noise button allows you to select a time range where you can set the amplitude to zero.
         You can remove multiple sections at once. The 'restore noise' button will restore ALL sections of removed amplitude. 
        If you wish to only remove one section, you will have to manually change the times. Note that if the remove noise button
        is clicked more than 15 times in a session it will cause the shiny app to crash. Best practice is to refresh the page 
        after completing analysis on an audio file, before importing another audio file."),
    br(),br(),
    h4("add flash/noise:"),
    p("This will add a flash at a specific location, if the function fails to detect. Currently this only works for quick 
        flashes. In the future I may add functionality for adding long, glow type flashes."),
    br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
  )
}

flash_stats_text <- "Red lines represent single flashes. If the complex flash 
method was chosen there will be green and blue lines representing groups of
flashes. If glow method was chosen the green line represents the start of a 
flash and the red line represents the end."