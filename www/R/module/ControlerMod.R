
ControllerUI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("start"), 
                 "start time",
                 value = 0,
                 min = 1,
                 max = 30), 
    numericInput(ns("end"), "end time", value = 10, min = 1, max = 10000),
    numericInput(ns("quant"), "amplitude quantile", value=0.999, 
                 min=0.85, max=1, step = 0.001),
    numericInput(ns("freq"), "frequency (slow glow only)", value = 9, 
                 min = 0, max = 25, step = 1), 
    numericInput(ns("pause"), 
                 "Group flashes if less than x seconds (complex flash only)", 
                 value=1, min=0, max=100, step=0.1),
    radioButtons(ns("flashtype"), "flash pattern", 
                 choices = c("single flash", "complex flash", "glow")),
    fluidRow( column(12, actionButton(ns("cancelnoise"), "remove noise"))),
    fluidRow(column(8, actionButton(ns("rmv_cancelnoise"), "restore noise"))),
    br(),
    fluidRow(column(8, actionButton(ns("addflash"), "add flash/noise")),
             column(6)),
    fluidRow(column(1, actionButton(ns("rmv_addflash"), 
                                    "remove added flash/noise"))),
    br(),
    fluidRow(column(1, 
                    actionButton(ns("flash_calc"), "Run flash calculations",
                                    style='font-weight: bold; font-size:120%')))
  )
}

ControllerServer <- function(id, counter, counterflash, flashtype,
                             controls) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
      observeEvent(input$flashtype, {
        flashtype$flashtype <- input$flashtype
      })
      
        
      observeEvent(input$start, {
        controls$tstart <- input$start
      })
      
      observeEvent(input$end, {
        controls$tend <- input$end
      })
      
      observeEvent(input$quant, {
        controls$quant <- input$quant
      })
      
      observeEvent(input$pause, {
        controls$pause <- input$pause
      })
      
      observeEvent(input$frequency, {
        controls$freq <- input$freq
      })
      
      # insert numeric inputs to remove background noise
      observeEvent(input$cancelnoise, {
        req(input$cancelnoise)
        counter$countervalue <- counter$countervalue + 1 
        num <- counter$countervalue
        
        insertUI( selector = paste0("#", id, "-cancelnoise"), 
                  where = 'afterEnd',
                  ui =  tags$div(id="removeall", 
                                 fluidRow( column(6,
                                                  numericInput(
                                                    paste0(ns("rmstart"), num), 
                                                    "Remove noise from",
                                                    value=0.01, min=0.1,
                                                    max=1000)),
                                           column(6, 
                                                  numericInput(
                                                    paste0(ns("rmend"), num),
                                                    "Remove noise to",
                                                    value=0.01, min=0.1,
                                                    max=1000))
                                 )))
      })
      
      # remove UI inputs for background noise
      observeEvent(input$rmv_cancelnoise, {
        updateNumericInput(session, "rmstart1", value =0)
        updateNumericInput(session, "rmstart2", value =0)
        updateNumericInput(session, "rmstart3", value =0)
        updateNumericInput(session, "rmstart4", value =0)
        updateNumericInput(session, "rmstart5", value =0)
        updateNumericInput(session, "rmstart6", value =0)
        updateNumericInput(session, "rmstart7", value =0)
        updateNumericInput(session, "rmstart8", value =0)
        updateNumericInput(session, "rmstart9", value =0)
        updateNumericInput(session, "rmstart10", value =0)
        updateNumericInput(session, "rmstart11", value =0)
        updateNumericInput(session, "rmstart12", value =0)
        updateNumericInput(session, "rmstart13", value =0)
        updateNumericInput(session, "rmstart14", value =0)
        updateNumericInput(session, "rmstart15", value =0)
        updateNumericInput(session, "rmend1", value =0)
        updateNumericInput(session, "rmend2", value =0)
        updateNumericInput(session, "rmend3", value =0)
        updateNumericInput(session, "rmend4", value =0)
        updateNumericInput(session, "rmend5", value =0)
        updateNumericInput(session, "rmend6", value =0)
        updateNumericInput(session, "rmend7", value =0)
        updateNumericInput(session, "rmend8", value =0)
        updateNumericInput(session, "rmend9", value =0)
        updateNumericInput(session, "rmend10", value =0)
        updateNumericInput(session, "rmend11", value =0)
        updateNumericInput(session, "rmend12", value =0)
        updateNumericInput(session, "rmend13", value =0)
        updateNumericInput(session, "rmend14", value =0)
        updateNumericInput(session, "rmend15", value =0)
        removeUI( selector = paste0("#", id, "-removeall"), immediate = F)
      })
      
      # insert numeric inputs to add noise
      observeEvent(input$addflash, {
        req(input$addflash)
        counterflash$countervalue <- counterflash$countervalue + 1
        num1 <- counterflash$countervalue
        insertUI(selector = paste0("#", id, "-addflash"), where = 'afterEnd',
                 ui = tags$div(id= paste0("#", id, "-flashadd"), 
                               numericInput(ns(paste0("added", num1)), 
                                            "add a flash at x time", 
                                            value=NA, 
                                            min=0, 
                                            max=1000)))
      })
      
      # Remove UI inputs for adding flash
      observeEvent(input$rmv_addflash, {
        updateNumericInput(session, "added1", value = NA)
        updateNumericInput(session, "added2", value = NA)
        updateNumericInput(session, "added3", value = NA)
        updateNumericInput(session, "added4", value = NA)
        updateNumericInput(session, "added5", value = NA)
        updateNumericInput(session, "added6", value = NA)
        updateNumericInput(session, "added7", value = NA)
        updateNumericInput(session, "added8", value = NA)
        updateNumericInput(session, "added9", value = NA)
        updateNumericInput(session, "added10", value = NA)
        updateNumericInput(session, "added11", value = NA)
        updateNumericInput(session, "added12", value = NA)
        updateNumericInput(session, "added13", value = NA)
        updateNumericInput(session, "added14", value = NA)
        updateNumericInput(session, "added15", value = NA)
        removeUI(selector= paste0("#", id, "-flashadd"), immediate=F)
        
      })
    }
  )
}
