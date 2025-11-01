
OutputUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Results"),
    tableOutput(ns("flash_stats")),
    flash_stats_text,
    plotOutput(ns("resultsplot"), 
               click = ns("eg_click"),
               dblclick = ns("dblclick"),
               height = "300px")
  )
}

OutputServer <- function(id, input2, flash, app_values) {
  moduleServer(
    id,
    function(input, output, session) {
      
      flash_data <- reactiveValues(new_flashes = c(),
                                   click_flashes = c(),
                                   rm_flashes = c(),
                                   data = NA)
      
      observeEvent(input2[["controls-flash_calc"]], {
        req(flash())
        flash <- flash()$audio
        samprate <- flash()$audio@samp.rate
        flash_data$click_flashes <- c()
        flash_data$rm_flashes <- c()
        
        
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
        flash_data$new_flashes <- c()
       if (app_values$addcounter > 0) {
         flash_data$new_flashes <- sapply(app_values$flist, function(x)
           input2[[paste0("controls-added", x)]]
         )
       }
        
        flash_data$flash <- flash
      })
      
      observeEvent(input$eg_click, {
        dist <- (app_values$tend - app_values$tstart) / 250
        flash_data$click_flashes <-
          flash_data$click_flashes[abs(flash_data$click_flashes -
                                         input$eg_click$x) > dist]
        
        flash_data$click_flashes <- c(flash_data$click_flashes,
                                      input$eg_click$x)
        
        close <- abs(flash_data$rm_flashes - input$eg_click$x) < dist
        flash_data$rm_flashes <- flash_data$rm_flashes[!close]
      })
      
      observeEvent(input$dblclick, {
        dist <- (app_values$tend - app_values$tstart) / 250
        close <- abs(flash_data$click_flashes - input$dblclick$x) < dist
        flash_data$click_flashes <- flash_data$click_flashes[!close]
        if(!any(close)) {
          flash_data$rm_flashes <- c(flash_data$rm_flashes, input$dblclick$x)
        }
      })
      
      observe({
        req(flash_data$flash)
        flash_data$data <- flashcalc(flash_data$flash,
                                     start = app_values$tstart,
                                     end = app_values$tend,
                                     pause = app_values$pause,
                                     quant = app_values$quant,
                                     flashtype = app_values$flashtype,
                                     synth = c(flash_data$new_flashes, 
                                               flash_data$click_flashes),
                                     rm_flash = flash_data$rm_flashes
        )
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
