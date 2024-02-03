library(shiny)

requirementUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("is_in"), NULL, choices = c("in", "not in"), selected = "in"),
    selectInput(ns("parcel_filter"), NULL, choices = c("Transit",
                                                       'Rapid Bus Line',
                                                       "Commercial Corridor", 
                                                       "Economic Opportunity", 
                                                       "PEG",
                                                       "Already Rezoned",
                                                       "Neighborhood",
                                                       'Lot Size')),
    uiOutput(ns("dynamic_slider")) # Placeholder for dynamic slider UI
  )
}

# Define server logic for a single requirement
requirementServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$dynamic_slider <- renderUI({
      if (input$parcel_filter %in% c("Transit", 'Rapid Bus Line', "Commercial Corridor", "Economic Opportunity", 'Neighborhood', 'Lot Size')) {
        if (input$parcel_filter == "Transit") {
          sliderInput(session$ns("distance"), "Distance (miles) from High-Frequency (<10 min) MUNI Line", min = 0, max = 1, value = 0.25)
        } else if (input$parcel_filter == "Rapid Bus Line") {
          sliderInput(session$ns("distance"), "Distance (miles) from Rapid MUNI Line", min = 0, max = 1, value = 0.25)
        } else if (input$parcel_filter == "Commercial Corridor") {
          sliderInput(session$ns("distance"), "Distance (miles) from Commercial Corridor", min = 0, max = 1, value = 0.25)
        } else if (input$parcel_filter == "Economic Opportunity") {
          sliderInput(session$ns("economic_score"), "Tracts with Economic Score of At Least", min = 0, max = 100, value = 50)
        } else if (input$parcel_filter == "Lot Size") {
          sliderInput(session$ns("lot_size"), "Lot Area (Sq Ft) Larger Than", min = 0, max = 50000, value = 5000)
        } else if (input$parcel_filter == "Neighborhood") {
          selectInput(session$ns("hood"), "Pick a neighborhood", choices=readRDS('./hoods.RDS'))
        }
      } else {
        return(NULL)
      }
    })
  })
}