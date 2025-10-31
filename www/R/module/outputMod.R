
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
        req(flash())
        flash <- flash()$audio
        samprate <- flash()$audio@samp.rate
        
        # Remove noise
        if (app_values$countervalue > 0) {
          rm_times <- lapply(app_values$newvec, function(x) {
            data.frame(start = input2[[paste0("controls-rmstart", x)]],
                       end = input2[[paste0("controls-rmend", x)]])
          }
          )
          
          for (ii in seq_along(rm_times)) {
            flash@left[(rm_times[[ii]]$start * samprate):
                           (rm_times[[ii]]$end * samprate)] <- 0
          }
        }
        
        # Add flash
        if (app_values$addcounter > 0) {
          add_times <- lapply(app_values$flist, function(x) 
            data.frame(newflash = input2[[paste0("controls-added", x)]])
          )
          for (ii in seq_along(add_times)) {
            flash@left[add_times[[ii]]$newflash * samprate] <- 
              quantile(flash@left, app_values$quant) + 1
          }
        }

        # Render table
        flash_data$data <- flashcalc(flash,
                                     start = app_values$tstart, 
                                     end = app_values$tend, 
                                     pause = app_values$pause, 
                                     quant = app_values$quant,
                                     flashtype = app_values$flashtype)
      })
      
      output$flash_stats <- renderTable({
        req(flash_data$data)
        input2[["controls-flash_calc"]]
        isolate(flashcalc_df(flash_data$data))
      })
      
      output$resultsplot <- renderPlot({
        req(flash_data$data)
        input2[["controls-flash_calc"]]
        isolate(flashcalc_plot(flash_data$data))
      })
    }
  )
}
