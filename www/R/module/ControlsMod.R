
ControlsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("inputfile"), "Choose a .wav or .mp3 file",
              accept = c(".wav", ".mp3")),
    uiOutput(ns("audioplayer")),
    div(class = "inline",
        strong("Start and End Time of Calculations (seconds)"),
        fluidRow(column(6, 
                        numericInput(ns("start"), 
                                     "start:", 
                                     value = 0, 
                                     min = 0,
                                     max = 10000),
        ),
        column(6, 
               numericInput(ns("end"), "end:", value = 1, min = 0, max = 10000),
        )
        ),
        tags$div(title = amplitude_text, 
                 numericInput(ns("quant"),
                              "amplitude quantile:",
                              value = 0.999,
                              min = 0.85,
                              max = 1,
                              step = 0.001)),
    ),
    radioButtons(ns("flashtype"),
                 "Flash Pattern", 
                 choices = c("single flash", "complex flash", "glow")),
    uiOutput(ns("complex_option")),
    tags$div(title = rm_noise_text,
             actionButton(ns("cancelnoise"), "remove noise")),
    br(),
    tags$div(title = add_flash_text,
             actionButton(ns("addflash"), "add flash/noise")),
    br(),
    actionButton(ns("flash_calc"), "Run flash calculations",
                 style = "font-weight: bold; font-size:120%"))
  
}

ControlsServer <- function(id, input2, app_values) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
      flash <- reactive({
        req(input$inputfile)
        print("starting infile server")
        infile <- input$inputfile
        print(infile)
        if (grepl(".*\\.wav", ignore.case = TRUE, infile$datapath)) {
          audio <- readWave(infile$datapath)
        } else {
          audio <- readMP3(infile$datapath)
        }
        audio <- Wave(left = audio@left[seq(1, length(audio@left), by = 4)],
                      samp.rate = audio@samp.rate / 4, bit = 16)
        data <- list(file = input$inputfile$datapath,
                     audio = audio)
        print("ending infile server")
        data
      })
      
      # Update max length of file
      observeEvent(input$inputfile, {
        base64 <- dataURI(file = flash()$file, mime = "audio/wav")
        output$audioplayer <- renderUI({
          tags$audio(src = base64, type = "audio/wav", controls = NA)
        })
      })
      
      observeEvent(input$flashtype, app_values$flashtype <- input$flashtype)
      observeEvent(input$start, app_values$tstart <- input$start)
      observeEvent(input$end, app_values$tend <- input$end)
      observeEvent(input$quant, app_values$quant <- input$quant)
      observeEvent(input$pause, app_values$pause <- input$pause)
      observeEvent(input$frequency, app_values$freq <- input$freq)
      
      observeEvent(input$flashtype, {
        print("flashtype changed")
        print(input$flashtype)
        if (input$flashtype == "complex flash") {
          output$complex_option <- renderUI({
            tags$div(title = group_flash_text,
                     numericInput(ns("pause"), 
                         "Group flashes if less than x seconds:", 
                         value = 1,
                         min = 0,
                         max = 100,
                         step = 0.1))
          })
        } else {
          output$complex_option <- renderUI({})
        }
      })
      # insert numeric inputs to remove background noise
      observeEvent(input$cancelnoise, {
        req(input$cancelnoise)
        app_values$countervalue <- app_values$countervalue + 1 
        num <- app_values$countervalue
        app_values$newvec <- c(app_values$newvec, app_values$countervalue)
        insertUI(selector = paste0("#", id, "-cancelnoise"), 
                 where = "afterEnd",
                 ui =  tags$div(id = paste0("removerow_", num),
                                class = "inline2",
                                fluidRow(column(5,
                                                numericInput(
                                                  paste0(ns("rmstart"), num),
                                                  "From:",
                                                  value = 0, 
                                                  min = 0,
                                                  max = 1000)),
                                         column(5, 
                                                numericInput(
                                                  paste0(ns("rmend"), num),
                                                  "To:",
                                                  value = 0, 
                                                  min = 0,
                                                  max = 1000)),
                                         column(1, 
                                                tags$div(
                                                  title = "remove row",
                                               actionButton(paste0(ns("rmn_"), num),
                                                            NULL,
                                                            icon = icon("times"),
                                                            class = "rm_btn"))
                                               )
                                )))
      })

      # Remove the additional UI
      observe({
        purrr::walk(app_values$newvec, function(i) {
          observeEvent(input[[paste0("rmn_", i)]], {
            removeUI(selector = paste0("#removerow_", i), multiple = TRUE)
            app_values$newvec <- app_values$newvec[app_values$newvec != i]
          }, ignoreInit = TRUE)
        })
      })
      
      # insert numeric inputs to add noise
      observeEvent(input$addflash, {
        req(input$addflash)
        app_values$addcounter <- app_values$addcounter + 1
        app_values$flist <- c(app_values$flist, app_values$addcounter)
        num1 <- app_values$addcounter
        insertUI(selector = paste0("#", id, "-addflash"), 
                 where = "afterEnd",
                 ui = tags$div(id = paste0("-flashadd", num1), 
                               class = "inline",
                               column(10, 
                                      numericInput(ns(paste0("added", num1)), 
                                            "Time:", 
                                            value = NA, 
                                            min = 0, 
                                            max = 1000)),
                               column(1, 
                                      tags$div(
                                        title = "remove row",
                                      actionButton(paste0(ns("rma_"), num1),
                                                   NULL,
                                                   icon = icon("times"),
                                                   class = "rm_btn")),
                               )))
      })
      
      # Remove the additional UI
      observe({
        purrr::walk(app_values$flist, function(i) {
          observeEvent(input[[paste0("rma_", i)]], {
            removeUI(selector = paste0("#-flashadd", i), multiple = TRUE)
            app_values$flist <- app_values$flist[app_values$flist != i]
          }, ignoreInit = TRUE)
        })
      })
      
      reactive(list(file = flash()$file,
                    audio = flash()$audio,
                    start = input$start_m, 
                    end = input$end_m))
    }
  )
}
