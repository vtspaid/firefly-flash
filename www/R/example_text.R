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
p("The table will also be updated. Compare the minimum flash interval with the previous table.")
)
}