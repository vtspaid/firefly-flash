
OutputUI <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Results"),
    tableOutput(ns("flash_stats")),
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
