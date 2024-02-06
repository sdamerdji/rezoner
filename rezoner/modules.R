library(shiny)

requirementUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("is_in"), NULL, choices = c("in", "not in"), selected = "in"),
    selectInput(ns("parcel_filter"), NULL, choices = c("Transit",
                                                       "Commercial Corridor", 
                                                       "Economic Opportunity", 
                                                       "PEG",
                                                       'TCAC',
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
      if (input$parcel_filter %in% c("Transit", "Commercial Corridor", "Economic Opportunity", 'TCAC', 'Neighborhood', 'Lot Size')) {
        if (input$parcel_filter == "Transit") {
          tagList(
            sliderInput(session$ns("distance"), "Distance (miles)", min = 0, max = 1, value = 0.25),
            selectInput(session$ns("transit_options"), 
                        "From", 
                        choices = c("BART Stops", "Caltrain Stops", 
                                    "Rapid Bus Line", 'Bus Lines (<10 min headways)'),
                        selected = 'BART Stops',
                        multiple=T)
          )
        } else if (input$parcel_filter == "Commercial Corridor") {
          sliderInput(session$ns("distance"), "Distance (miles) from Commercial Corridor", min = 0, max = 1, value = 0.25)
        } else if (input$parcel_filter == "Economic Opportunity") {
          sliderInput(session$ns("economic_score"), "Tracts with Economic Score of At Least", min = 0, max = 100, value = 50)
        } else if (input$parcel_filter == "TCAC") {
          selectInput(session$ns("affh_score"), "Tracts with TCAC Opportunity Map Score of At Least", choices = c('Low', 'Medium', 'High'))
        } else if (input$parcel_filter == "Lot Size") {
          sliderInput(session$ns("lot_size"), "Lot Area (Sq Ft) Larger Than", min = 0, max = 50000, value = 5000)
        } else if (input$parcel_filter == "Neighborhood") {
          selectInput(session$ns("hood"), "Pick a neighborhood", choices=readRDS('./hoods.RDS'), multiple=T)
        }
      } else {
        return(NULL)
      }
    })
  })
}