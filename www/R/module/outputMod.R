
OutputUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Results"),
    tableOutput(ns("flash_stats")),
    flash_stats_text,
    plotOutput(ns("resultsplot"), height = "300px")
  )
}

OutputServer <- function(id, input2, flash, app_values) {
  moduleServer(
    id,
    function(input, output, session) {
      
      flash_data <- reactiveValues(flash_table = list(),
                                   data = NA)
      
      observeEvent(input2[["controls-flash_calc"]], {
        print("trying")
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
          flash_data$flash <- singleflash(dfflash,
                                          start = app_values$tstart, 
                                          end = app_values$tend, 
                                          quant = app_values$quant)
          flash_data$flash_table <- singleflash_df(flash_data$flash$peak)
        } else if (app_values$flashtype == "complex flash") {
          flash_data$flash <- complexflash(dfflash,
                                           start = app_values$tstart, 
                                           end = app_values$tend, 
                                           pause = app_values$pause, 
                                           quant = app_values$quant)
          flash_data$flash_table <- complexflash_df(flash_data$flash$peak,
                                                    flash_data$flash$breaktime)
        } else {
          flash_data$flash <- slowglow(dfflash,
                                       start = app_values$tstart, 
                                       end = app_values$tend, 
                                       quant = app_values$quant, 
                                       freq = app_values$freq)
          flash_data$flash_table <- slowglow_df(flash_data$flash$glowtimes)
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
          singleflash_plot(flash_data$flash)
        } else if (app_values$flashtype == "complex flash") {
          complexflash_plot(flash_data$flash)
        } else {
          slowglow_plot(flash_data$flash)
        }
        )
      })
    }
  )
}
