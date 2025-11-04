
OutputUI <- function(id) {
  ns <- NS(id)

  tagList(
    # tags$script(HTML(sprintf("
    #   $(document).on('mousemove', function(event) {
    #     var mx = event.pageX;
    #     var my = event.pageY;
    #     Shiny.setInputValue('%s', {mx: mx, my: my}, {priority: 'event'});
    #   });
    # ", ns('mouse_loc')))),
    h3("Results"),
    tableOutput(ns("flash_stats")),
    HTML(flash_stats_text),
    uiOutput(ns("popup")),
    # tags$div(style = "position:relative;",
    #          # plot has namespaced id so we can bind JS to it
    #          plotOutput(ns("resultsplot"),
    #                     hover = hoverOpts(id = ns("hover"),
    #                                       delay = 10, delayType = "throttle"),
    #                     height = "300px")
    # ),
    
    plotOutput(ns("resultsplot"),
               click = ns("eg_click"),
               dblclick = ns("dblclick"),
               hover = ns("hover"),
               height = "300px"),
    tags$script(HTML(sprintf("
      (function() {
        var plotId = '%s';
        // plot is rendered as a canvas inside the container with id = plotId
        // use mousemove on the container so we can compute page coords easily
        $(document).on('mousemove', '#' + plotId, function(e) {
          Shiny.setInputValue('%s', { pageX: e.pageX, pageY: e.pageY }, {priority: 'event'});
        });
        // hide popup on mouseleave of the plot container
        $(document).on('mouseleave', '#' + plotId, function(e) {
          Shiny.setInputValue('%s', null, {priority: 'event'});
        });
      })();
    ", ns("resultsplot"), ns("mouse_loc"), ns("mouse_loc"))))
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
        # input$hover contains the data coordinates (x, y) and coords_css
       # h <- input$hover
        #m <- input$mouse_loc  # namespaced mouse coords {pageX, pageY} or NULL
        if (is.null(input$hover) || is.null(input$mouse_loc)) {
          output$popup <- renderUI(NULL)
          return()
        }
        
        # h$x is the data x value; m$pageX/m$pageY are page coordinates
        px <- input$mouse_loc$pageX
        py <- input$mouse_loc$pageY
        
        # create inline style with 'px' and pointer-events:none so it won't steal mouse events
        style <- paste0("position:absolute; left:", px + 10, "px; top:", py + 10,
                        "px; background:#fff; border:1px solid #999; padding:6px 8px; border-radius:4px; z-index:9999; pointer-events:none;")

        output$popup <- renderUI({
          div(style = style, paste0("x = ", round(input$hover$x, 4)))
        })
      })
      
      # observeEvent(input$hover, {
      #    print("hover")
      #   print(names(input$hover))
      #   print(input$hover$x)
      #     mx <- input$mouse_loc$mx # The mouse x position on the page
      #     my <- input$mouse_loc$my # The mouse y position on the page
      #     print(input$hover$coords_css$x)
      #     print(input$hover$coords_css)
      #     print(input$hover$coords_img)
      #     print(my)
      #     print(mx)
      #     output$popup <- renderUI({
      #       div(id = "popup", 
      #           style = paste0("position:absolute; 
      #                          top:", my - 75, "px;
      #                          left: ", mx - 325, "px;"), 
      #           round(input$hover$x, 2))
      #     })
      # })
      
      
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
