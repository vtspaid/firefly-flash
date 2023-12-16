library(shiny)

ui <- fluidPage(
  tags$head(tags$script(src = "message-handler.js")),
  actionButton("dobeep", "Play sound")
)

server <- function(input, output, session) {
  observeEvent(input$dobeep, {
    insertUI(selector = "#dobeep",
             where = "afterEnd",
             # beep.wav should be in /www of the shiny app
             ui = tags$audio(src = "Dunkard_potomaca.wav", type = "audio/wav", autoplay = T, controls = NA, style="display:none;")
    )
  })
}

shinyApp(ui, server)


library(shiny)

ui <- fluidPage(
  tags$head(tags$script(src = "message-handler.js")),
  actionButton("dobeep", "Play sound"),
  div(id = "audioDiv")  # Placeholder div for the audio element
)

server <- function(input, output, session) {
  observeEvent(input$dobeep, {
    insertUI(
      selector = "#audioDiv",
      where = "afterEnd",
      # 'Dunkard_potomaca.wav' should be in the '/www' directory of the Shiny app
      ui = tags$audio(src = "Dunkard_potomaca.wav", type = "audio/wav", autoplay = NA, controls = TRUE)
    )
  })
}

shinyApp(ui, server)
