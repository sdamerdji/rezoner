library(shiny)
library(shinyWidgets)

requirementUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$style(HTML("
      .bootstrap-switch-container {
        height: 36.99px; /* Adjust the height as needed */
        line-height: 36.99px; /* Align text vertically */
        width: 101px;
      }
    ")),
      tags$style(HTML("
  #requirement > div.row > div.col-sm-4 > div > div > div {
    height: 35px; /* Adjust the height as needed */
    line-height: 35px; /* Align text vertically */
    width: 1070px;
  }
")),
      
      column(4, switchInput(ns("is_in"),
                               NULL, 
                               onLabel = "in", offLabel = "not in", 
                               value = TRUE, size='mini')),
    column(8, selectInput(ns("parcel_filter"), NULL, choices = c("Transit",
                                                       "Commercial Corridor", 
                                                       "Economic Opportunity", 
                                                       "PEG",
                                                       'TCAC',
                                                       "Already Rezoned",
                                                       "Neighborhood",
                                                       'Lot Size', 
                                                       'Parks', 
                                                       'Colleges')))
    ),
    uiOutput(ns("dynamic_slider")) # Placeholder for dynamic slider UI
    
  )
}

# Define server logic for a single requirement
requirementServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$dynamic_slider <- renderUI({
      if (input$parcel_filter %in% c("Transit", "Commercial Corridor", "Economic Opportunity", 'TCAC', 'Neighborhood', 'Lot Size', 'Parks', 'Colleges')) {
        if (input$parcel_filter == "Transit") {
          tagList(
            sliderInput(session$ns("distance"), "Distance (miles)", min = 0, max = 1, value = 0.1),
            selectInput(session$ns("transit_options"), 
                        "From", 
                        choices = c("Caltrain Stops", "BART Stops",
                                    "Muni Rapid Network", 'All Muni Lines'),
                        selected = 'BART Stops',
                        multiple=T)
          )
        } else if (input$parcel_filter == "Commercial Corridor") {
          sliderInput(session$ns("distance"), "Distance (miles) from Commercial Corridor", min = 0, max = 1, value = 0.1)
        } else if (input$parcel_filter == "Economic Opportunity") {
          sliderInput(session$ns("economic_score"), "Tracts with Economic Score of At Least", min = 0, max = 100, value = 50)
        } else if (input$parcel_filter == "TCAC") {
          selectInput(session$ns("affh_score"), "Tracts with TCAC Opportunity Map Score of At Least", choices = c('Low', 'Medium', 'High'))
        } else if (input$parcel_filter == "Lot Size") {
          sliderInput(session$ns("lot_size"), "Lot Area (Sq Ft) Larger Than", min = 0, max = 50000, value = 5000)
        } else if (input$parcel_filter == "Neighborhood") {
          selectInput(session$ns("hood"), "Pick a neighborhood", choices=readRDS('./hoods.RDS'), multiple=T)
        } else if (input$parcel_filter == "Parks") {
          sliderInput(session$ns("distance"), "Distance (miles) from Parks (1+ Acres)", min = 0, max = 1, value = 0.1)
        } else if (input$parcel_filter == "Colleges") {
          sliderInput(session$ns("distance"), "Distance (miles) from Colleges", min = 0, max = 1, value = 0.1)
        } 
      } else {
        return(NULL)
      }
    })
  })
}