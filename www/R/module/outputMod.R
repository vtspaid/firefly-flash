
OutputUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    flash_stats_text,
    tableOutput(ns("flash_stats")),
    
    br(), br(),
    p("This plot is of the audio used in the flash calculations, the red lines 
    are where the r function believes a flash occured"),
    plotOutput(ns("resultsplot")),
    br(), br(), br(),br(),br(),br(),br(),br(),br(),br()
  )
}

OutputServer <- function(id, input2, FLASH, counter, counterflash, flashtype,
                         controls) {
  moduleServer(
    id,
    function(input, output, session) {
      output$flash_stats  <- renderTable({
        req(FLASH())
        req(input2[["controls-flash_calc"]])
        dfflash <- FLASH()$audio
        samprate <- FLASH()$audio@samp.rate
        print(flashtype$flashtype)
        # Remove flash
        isolate(
          if(counter$countervalue > 0) {
            rm_times <- lapply(1:counter$countervalue, function(x) 
              data.frame(start = input2[[paste0("controls-rmstart", x)]],
                         end = input2[[paste0("controls-rmend", x)]])
            )
            
            for (ii in 1:length(rm_times)){
              dfflash$left[c(rm_times[[ii]]$start*samprate):c(rm_times[[ii]]$end*samprate)] <- 0
            }
          }
        )
      
        # Add flash
        isolate(if(counterflash$countervalue > 0) 
        {add_times <- lapply(1:counterflash$countervalue, function(x) 
          data.frame(newflash = input2[[paste0("controls-added", x)]])
        )
        for (ii in 1:length(add_times)){
          dfflash$left[c(add_times[[ii]]$newflash*samprate)] <- quantile(dfflash$left, input$quant) + 1
        }
        })
print("did we get here")
        # Render table
        isolate(if (flashtype$flashtype == 'single flash') {
          singleflash(dfflash,
                      start=controls$tstart, end=controls$tend, quant=controls$quant
          )
        } else if (flashtype$flashtype == 'complex flash'){
          complexflash(dfflash,
                       start=controls$tstart, end=controls$tend, pause=controls$pause, 
                       quant=controls$quant
          )
        } else {slowglow(dfflash,
                         start=controls$tstart, end=controls$tend, 
                         quant=controls$quant, freq = controls$freq
        )})
      })
    }
  )
}