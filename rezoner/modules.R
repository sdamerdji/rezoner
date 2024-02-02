library(shiny)

requirementUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("is_in"), NULL, choices = c("in", "not in"), selected = "in"),
    selectInput(ns("parcel_filter"), NULL, choices = c("Transit", "Commercial Corridor", "Economic Opportunity", "PEG", "Already Rezoned")),
    uiOutput(ns("dynamic_slider")) # Placeholder for dynamic slider UI
  )
}

# Define server logic for a single requirement
requirementServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$dynamic_slider <- renderUI({
      if (input$parcel_filter %in% c("Transit", "Commercial Corridor", "Economic Opportunity")) {
        if (input$parcel_filter == "Transit") {
          sliderInput(session$ns("distance"), "Distance from Transit", min = 0, max = 1, value = 0.25)
        } else if (input$parcel_filter == "Commercial Corridor") {
          sliderInput(session$ns("distance"), "Distance from Commercial Corridor", min = 0, max = 1, value = 0.25)
        } else if (input$parcel_filter == "Economic Opportunity") {
          sliderInput(session$ns("economic_score"), "Tracts with Economic Score of at least", min = 0, max = 100, value = 50)
        }
      } else {
        return(NULL)
      }
    })
  })
}