
OutputUI <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Results"),
    dataTableOutput(ns("flash_stats")),
    HTML(flash_stats_text),
    uiOutput(ns("popup")),

    tags$div(id = ns("plot_wrap"), style = "position:relative;",
             plotOutput(ns("resultsplot"), 
                        hover = hoverOpts(id = ns("plot_hover"), 
                                          delay = 10, delayType = "throttle"),
                        click = ns("eg_click"),
                        dblclick = ns("dblclick"),
                        height = "300px")
    ),
    fluidRow(column(1),
             column(11,  
                    div(class = "duration_slider",
                        sliderInput(NS(id, "duration"), label = NULL,
                                    min = 0, max = 100, value = c(0, 100),
                                    step = 0.1,
                                    width = "100%")))),
    
  tags$script(HTML(sprintf("
(function(){
  var plotId = '%s';        // container id (div that holds the plotOutput)
  var popupId = '%s';       // id for popup element

  // create popup element if missing
  if (!document.getElementById(popupId)) {
    var p = document.createElement('div');
    p.id = popupId;
    p.style.position = 'absolute';
    p.style.display = 'none';
    p.style.background = '#fff';
    p.style.border = '1px solid #999';
    p.style.padding = '6px 8px';
    p.style.borderRadius = '4px';
    p.style.pointerEvents = 'none';
    p.style.zIndex = 99999;
    p.style.whiteSpace = 'nowrap';
    document.body.appendChild(p);
  }

  // store last mouse page coords for smooth placement
  var lastMouse = {pageX: 0, pageY: 0};

  // mousemove on the plot container: remember mouse position
  $(document).on('mousemove', '#' + plotId, function(e) {
    lastMouse.pageX = e.pageX;
    lastMouse.pageY = e.pageY;
  });

  // hide popup when leaving the plot container
  $(document).on('mouseleave', '#' + plotId, function() {
    var p = document.getElementById(popupId);
    if (p) p.style.display = 'none';
  });

  // custom message handler to update text and show popup.
  // msg should contain { text: '...', relX: <px>, relY: <px> }
  Shiny.addCustomMessageHandler('hoverPopup_update', function(msg) {
    var p = document.getElementById(popupId);
    if (!p) return;
    p.textContent = msg.text || '';
    // compute page coordinates using plot container offset + rel coords (pixels inside plot)
    var $plot = $('#' + plotId);
    if ($plot.length) {
      var off = $plot.offset();
      var left = off.left + (msg.relX || 0) + 12;   // 12px gap from cursor
      var top  = off.top  + (msg.relY || 0) + 12;
      // clamp to viewport
      var maxLeft = window.pageXOffset + document.documentElement.clientWidth - p.offsetWidth - 8;
      var maxTop  = window.pageYOffset + document.documentElement.clientHeight - p.offsetHeight - 8;
      left = Math.max(window.pageXOffset + 8, Math.min(left, maxLeft));
      top  = Math.max(window.pageYOffset + 8, Math.min(top, maxTop));
      p.style.left = left + 'px';
      p.style.top  = top + 'px';
      p.style.display = 'block';
    } else {
      // fallback: use last mouse page coords
      p.style.left = (lastMouse.pageX + 12) + 'px';
      p.style.top  = (lastMouse.pageY + 12) + 'px';
      p.style.display = 'block';
    }
  });
})();
", ns("plot_wrap"), ns("hover_popup"))))
  )
  
}

OutputServer <- function(id, input2, flash, app_values) {
  moduleServer(
    id,
    function(input, output, session) {
      
      flash_data <- reactiveValues(data = NA)
      flash_test <- reactiveValues(test = c())

      
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
      
        flash_test$flash <- flash
      })
      
      
      # observe hover; when present, send text + rel pixel coords to JS
      observe({
        h <- input$plot_hover
        if (is.null(h)) {
          # hide by sending empty text (JS hides on mouseleave anyway)
          session$sendCustomMessage("hoverPopup_update", list(text = "", 
                                                              relX = NULL,
                                                              relY = NULL))
          return()
        }
        # h$x is data x value, h$coords_css$x/y are pixels relative to the plot
        txt <- paste0("x = ", signif(h$x, 4))
        session$sendCustomMessage("hoverPopup_update",
                                  list(text = txt, 
                                       relX = h$coords_css$x, 
                                       relY = h$coords_css$y))
      })
      
      # Run flashcalc when button is pressed
      observeEvent(input2[["controls-flash_calc"]], {
        req(flash_test$flash)
        tryCatch(
        flash_data$data <- flashcalc(flash_test$flash,
                                     start = app_values$tstart,
                                     end = app_values$tend,
                                     pause = app_values$pause,
                                     quant = app_values$quant,
                                     flashtype = app_values$flashtype)
        , error = function(e) e)
      })
      
      # Add flash
      observeEvent(input$eg_click, {
        dif = (input$duration[2] - input$duration[1]) / 250
        flash_data$data <- add_flash(flash_data$data,
                                     add = input$eg_click$x,
                                     dif  = dif)
      })
      
      # Remove flash
      observeEvent(input$dblclick, {
        dif = (input$duration[2] - input$duration[1]) / 250
        flash_data$data <- rm_flash(flash_data$data,
                                    rm = input$dblclick$x,
                                    dif  = dif)
      }) 
      
      observe({
        updateNumericInput(session, "duration", 
                           value = c(app_values$tstart, app_values$tend), 
                           min = app_values$tstart, max = app_values$tend)
      })

      
      output$flash_stats <- renderDataTable({
        req(flash_data$data)
        input2[["controls-flash_calc"]]
        mydt(flashcalc_df(flash_data$data), app_values$flashtype)
      }, server = TRUE)
      
      output$resultsplot <- renderPlot({
        req(flash_data$data)
        input2[["controls-flash_calc"]]
        flashcalc_plot(flash_data$data,
                       xlims = c(input$duration[1], input$duration[2]))
      })
    }
  )
}
