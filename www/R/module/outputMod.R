
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
      
      flash_data <- reactiveValues(flash_table = list(),
                                   data = NA)
      
      observeEvent(input2[["controls-flash_calc"]], {
        req(FLASH())
        dfflash <- FLASH()$audio
        samprate <- FLASH()$audio@samp.rate
        print(flashtype$flashtype)
        # Remove flash
        
        if(counter$countervalue > 0) {
          rm_times <- lapply(1:counter$countervalue, function(x) 
            data.frame(start = input2[[paste0("controls-rmstart", x)]],
                       end = input2[[paste0("controls-rmend", x)]])
          )
          
          for (ii in 1:length(rm_times)){
            dfflash@left[(rm_times[[ii]]$start*samprate):(rm_times[[ii]]$end*samprate)] <- 0
          }
        }
        
        # Add flash
        if(counterflash$countervalue > 0) {
          add_times <- lapply(1:counterflash$countervalue, function(x) 
          data.frame(newflash = input2[[paste0("controls-added", x)]])
        )
        for (ii in 1:length(add_times)) {
            dfflash@left[add_times[[ii]]$newflash*samprate] <- quantile(dfflash@left, controls$quant) + 1
        }
        }
        print("did we get here")
        # Render table
        flash_data$data <- dfflash
        if (flashtype$flashtype == 'single flash') {
          flash_data$flash_table <- singleflash(dfflash,
                      start=controls$tstart, end=controls$tend, quant=controls$quant)
        } else if (flashtype$flashtype == 'complex flash') {
          flash_data$flash_table <- complexflash(dfflash,
                       start=controls$tstart, end=controls$tend, pause=controls$pause, 
                       quant=controls$quant)
        } else {
          flash_data$flash_table <- slowglow(dfflash,
                         start = controls$tstart, end = controls$tend, 
                         quant = controls$quant, freq = controls$freq)
        }
    
      })
      
      output$flash_stats <- renderTable({
        req(flash_data$flash_table)
        req(input2[["controls-flash_calc"]])
        flash_data$flash_table
      })
      
      output$resultsplot <- renderPlot({
        print("does this ever run")
        req(flash_data$data)
        req(input2[["controls-flash_calc"]])
        if (flashtype$flashtype == 'single flash') {
              flashcheck(flash_data$data, start=controls$tstart, end=controls$tend, quant=controls$quant)
            } else if (flashtype$flashtype == 'complex flash') {
              complexflashcheck(flash_data$data, start=controls$tstart, end=controls$tend, quant=controls$quant, pause=controls$pause)
            } else {
              glowcheck(flash_data$data, 
                        start=controls$tstart, 
                        end = controls$tend, 
                        quant = controls$quant, freq = controls$freq)
              }
      })
    }
  )
}