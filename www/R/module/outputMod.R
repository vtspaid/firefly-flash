
OutputUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    h3("Results"),
    tableOutput(ns("flash_stats")),
    
    br(), br(),
    h3("Results Visualization"),
    flash_stats_text,
    plotOutput(ns("resultsplot")),
    br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
  )
}

OutputServer <- function(id, input2, flash, app_values) {
  moduleServer(
    id,
    function(input, output, session) {
      
      flash_data <- reactiveValues(flash_table = list(),
                                   data = NA)
      
      observeEvent(input2[["controls-flash_calc"]], {
        req(flash())
        dfflash <- flash()$audio
        samprate <- flash()$audio@samp.rate
        print(app_values$flashtype)
        # Remove flash
        
        if (app_values$countervalue > 0) {
          rm_times <- lapply(1:app_values$countervalue, function(x) {
            data.frame(start = input2[[paste0("controls-rmstart", x)]],
                       end = input2[[paste0("controls-rmend", x)]])
          }
          )
          
          for (ii in seq_along(rm_times)){
            dfflash@left[(rm_times[[ii]]$start * samprate):
                           (rm_times[[ii]]$end * samprate)] <- 0
          }
        }
        
        # Add flash
        if (app_values$addcounter > 0) {
          add_times <- lapply(1:app_values$addcounter, function(x) 
            data.frame(newflash = input2[[paste0("controls-added", x)]])
          )
          for (ii in seq_along(add_times)) {
            dfflash@left[add_times[[ii]]$newflash * samprate] <- 
              quantile(dfflash@left, app_values$quant) + 1
          }
        }
        print("did we get here")
        # Render table
        flash_data$data <- dfflash
        if (app_values$flashtype == "single flash") {
          flash_data$flash_table <- singleflash(dfflash,
                                                start = app_values$tstart, 
                                                end = app_values$tend, 
                                                quant = app_values$quant)
        } else if (app_values$flashtype == "complex flash") {
          flash_data$flash_table <- complexflash(dfflash,
                                                 start = app_values$tstart, 
                                                 end = app_values$tend, 
                                                 pause = app_values$pause, 
                                                 quant = app_values$quant)
        } else {
          flash_data$flash_table <- slowglow(dfflash,
                                             start = app_values$tstart, end = app_values$tend, 
                                             quant = app_values$quant, freq = app_values$freq)
        }
        
      })
      
      output$flash_stats <- renderTable({
        req(flash_data$flash_table)
        input2[["controls-flash_calc"]]
        flash_data$flash_table
      })
      
      output$resultsplot <- renderPlot({
        print("render calc plot")
        req(flash_data$data)
        input2[["controls-flash_calc"]]
        isolate(
        if (app_values$flashtype == "single flash") {
          flashcheck(flash_data$data, 
                     start = app_values$tstart, 
                     end = app_values$tend, 
                     quant = app_values$quant)
        } else if (app_values$flashtype == "complex flash") {
          complexflashcheck(flash_data$data, 
                            start = app_values$tstart, 
                            end = app_values$tend,
                            quant = app_values$quant,
                            pause = app_values$pause)
        } else {
          glowcheck(flash_data$data, 
                    start = app_values$tstart, 
                    end = app_values$tend, 
                    quant = app_values$quant, 
                    freq = app_values$freq)
        }
        )
      })
    }
  )
}
