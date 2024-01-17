library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(waiter)
library(shinyjs)
library(sfarrow)

model <- readRDS(file='./light_model.rds')
pal <- colorBin("viridis",  bins = c(1, 5, 7, 20, Inf), right = F)

#pal <- colorBin("viridis",  bins = c(0, 1, 5, 10, 100, Inf), right = F)

sf_sites_inventory <- st_read_feather('./four_rezonings.feather')


# Years after rezoning
n_years <- 5
fourplex <- "Increased density up to four units (six units on corner lots)"
decontrol <- "No height change, density decontrol"
skyscrapers <-  "300' Height Allowed"


parisian <- function(df) {
  # Find all M3_ZONING with fourplex or decontrol and set to 55'
  parisian <- "55' Height Allowed"
  df[(!is.na(df$M3_ZONING)) & df$M3_ZONING == fourplex, 'M3_ZONING'] <- parisian
  df[(!is.na(df$M3_ZONING)) & df$M3_ZONING == decontrol, 'M3_ZONING'] <- parisian
  df[is.na(df$M3_ZONING) & (!is.na(df$M1_ZONING)| 
                             !is.na(df$M2_ZONING)|
                              !is.na(df$M4_ZONING)), 'M3_ZONING'] <- parisian
  df
}

legalize_it <- function(df) {
  df[, 'M3_ZONING'] <- skyscrapers
  df
}

union_of_maxdens <- function(df) {
  # Set M3 Zoning to be maximum upzoning of M1 and M2. Surely this is covered by EIR
  df %>% 
    mutate(M3_ZONING =
             case_when(
               as.numeric(stringr::str_extract(M1_ZONING, "\\d+")) > as.numeric(stringr::str_extract(M3_ZONING,  "\\d+"))  ~ M1_ZONING,
               !is.na(as.numeric(stringr::str_extract(M1_ZONING, "\\d+"))) & is.na(as.numeric(stringr::str_extract(M3_ZONING,  "\\d+"))) ~ M1_ZONING,
               TRUE ~ M3_ZONING
             )) %>%
    mutate(M3_ZONING = 
             case_when(
               as.numeric(stringr::str_extract(M2_ZONING, "\\d+")) > as.numeric(stringr::str_extract(M3_ZONING,  "\\d+")) ~ M2_ZONING,
               !is.na(as.numeric(stringr::str_extract(M2_ZONING, "\\d+"))) & is.na(as.numeric(stringr::str_extract(M3_ZONING,  "\\d+"))) ~ M2_ZONING,
               TRUE ~ M3_ZONING
             )) %>%
     mutate(M3_ZONING = 
                      case_when(
                        as.numeric(stringr::str_extract(M4_ZONING, "\\d+")) > as.numeric(stringr::str_extract(M3_ZONING,  "\\d+")) ~ M4_ZONING,
                        !is.na(as.numeric(stringr::str_extract(M4_ZONING, "\\d+"))) & is.na(as.numeric(stringr::str_extract(M3_ZONING,  "\\d+"))) ~ M4_ZONING,
                        TRUE ~ M3_ZONING
                        )
    )
}

height_setter <- function(M3_ZONING, height) {
  dplyr::case_when(
    !is.na(M3_ZONING) & is.na(height) ~ 40,
    .default = height
  )
}


upzone <- function(df) {
  print('upzoning... kinda a fake name')
  print('sum(df$expected_units, na.rm=F)')
  print(sum(df$expected_units, na.rm=F))
  print('sum(df$expected_units, na.rm=T)')
  print(sum(df$expected_units, na.rm=T))
  (total_expected_units <- sum(df$expected_units, na.rm=T))
}

update_df <- function(scenario) {
  # Given site inventory df inner joined with history 
  # Control upzoning by changing M3_ZONING before passing df in
  # Return df with fields "Expected" and "Pdev"
  print(scenario)
  df <- sf_sites_inventory
  
  if (scenario == 'A') {
    df[, 'M3_ZONING'] <- df$M1_ZONING
    df[, 'M3_MAXDENS'] <- df$M1_MAXDENS
  }
  if (scenario == 'B') {
    df[, 'M3_ZONING'] <- df$M2_ZONING
    df[, 'M3_MAXDENS'] <- df$M2_MAXDENS
  }
  if (scenario == 'C') {
    df[, 'M3_ZONING'] <- df$M3_ZONING
    df[, 'M3_MAXDENS'] <- df$M3_MAXDENS
  }
  if (scenario == 'D') {
    df[, 'M3_ZONING'] <- df$M4_ZONING
  }
  if (scenario == 'Union') {
    df = union_of_maxdens(df)
  }
  if (scenario == 'Parisian') {
    df = union_of_maxdens(df)
    df <- parisian(df)
  }
  if (scenario == 'Legalize It') {
    df <- legalize_it(df)
  }
  # Erase existing zoning indicators for rezoned parcels
  df[!is.na(df$M3_ZONING), 
     grep("^zp", names(sf_sites_inventory), value = TRUE)
     ] <- 0
  
  # So I don't have to handle missing values
  df[is.na(df$M3_ZONING), 'M3_ZONING'] <- 'No Change'
  
  # See page 30 of Appendix B in Scenario A for reasoning
  df <- df %>%
    mutate(zp_FormBasedMulti = if_else(!(M3_ZONING == 'No Change' | M3_ZONING == fourplex),
                                       1,
                                       zp_FormBasedMulti))
  
  df <- df %>%
    mutate(zp_DensRestMulti = if_else(M3_ZONING == fourplex | M3_ZONING == 'No Change', 1, zp_DensRestMulti)) %>%
    mutate(height = as.numeric(stringr::str_extract(M3_ZONING, "\\d+"))) %>%
    mutate(height = height_setter(M3_ZONING, height)) %>%
    mutate(
      # Calculate envelope_1000_new based on ACRES and height
      Envelope_1000_new = case_when(
        height >= 85 ~ ((ACRES * 43560) * 0.8 * height / 10.5) / 1000,
        ACRES >= 1 & height < 85 ~ ((ACRES * 43560) * 0.55 * height / 10.5) / 1000,
        ACRES < 1 ~ ((ACRES * 43560) * 0.75 * height / 10.5) / 1000, # Swap lines from Rmd bc this logic matches STATA code
        TRUE ~ NA_real_
      ),
      existing_sqft = Envelope_1000 / Upzone_Ratio,
      Upzone_Ratio_new = Envelope_1000_new / existing_sqft
    ) %>%
    mutate(
      Envelope_1000 = if_else(!(M3_ZONING == 'No Change'), Envelope_1000_new, Envelope_1000),
      Upzone_Ratio = if_else(!(M3_ZONING == 'No Change'), Upzone_Ratio_new, Upzone_Ratio)
    ) %>%
    mutate(expected_units_if_dev = ifelse(M3_ZONING != 'No Change', 
                                          Envelope_1000 * 1000 * 0.8 / 850, 0))
  
  if (scenario == 'A' | scenario == 'B' | scenario == 'C') { # This is another change from Rmd
    print("dont allow E[U|D] to exceed sf planninig's claim")
    df <- df %>%
      mutate(M3_MAXDENS = abs(M3_MAXDENS)) %>%
      mutate(du_allowed = ACRES * M3_MAXDENS) %>%
      mutate(expected_units_if_dev = ifelse((!is.na(du_allowed)) & (expected_units_if_dev > du_allowed),
                                                du_allowed, expected_units_if_dev))
  }
  
  predictions.16 <- predict(model, newdata = df, type = "response")
  df <- df %>%
    mutate(pdev = 1 - (1-predictions.16)^n_years) %>% 
    mutate(expected_units = pdev * expected_units_if_dev)
  
  return(df)
}

generate_plot <- function(df=F, plot_pdev=F) {
  print('render leaflet')
  
  leaflet(sf_sites_inventory) %>%
    addProviderTiles(providers$CartoDB.Positron, 
                     options = providerTileOptions(minZoom = 12, maxZoom = 16)) %>%
    setMaxBounds(lng1 = -122.5149, lat1 = 37.7081, lng2 = -122.3564, lat2 = 37.8324)  %>%
      addLegend(
        "bottomright",
        pal = pal,
        labels = c("1 - 4", "5 - 7", "8 - 19", "20+"),
        values = c(1, 5, 7, 20, Inf),
        title = "Stories"
      )
  #pal <- colorBin("viridis",  bins = c(0, 1, 5, 10, 100, Inf), right = F)

  #  addLegend(
  #    "bottomright",
   #   pal = pal,
  #    labels = c("0", "1 - 4", "5 - 9", "10 - 99", "100+"),
  #    values = ~ pal(c(0, 10, 100, 500)),
  #    title = "Expected Units"
  #  )
}

calculate_shortfall <- function(df) {
  print('calculating shortfall')
  df2 <- select(df, -geometry)
  return(upzone(df2))
}

# UI definition
ui <- fluidPage(
  useWaiter(), # include dependencies
  shinyjs::useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$script(src = "./js-confetti.browser.js"),
    tags$script(HTML("
      var jsConfettiInstance; // Global confetti instance
      document.addEventListener('DOMContentLoaded', function() {
        jsConfettiInstance = new JSConfetti(); // Instantiate when the document is ready
      });
    "))
  ),
  titlePanel("Upzone the City"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("scenario", "Upzoning Strategies:",
                         choices = c("Housing Element Rezoning Scenario A" = "A",
                                     "Housing Element Rezoning Scenario B" = "B",
                                     "Housing Element Rezoning Scenario C" = "C",
                                     "SF's Fall 2023 Rezoning Map" = "D",
                                     "Take the most ambitious version of everything SF Planning has proposed so far" = "Union", 
                                     "Parisian (5 story buildings in R1 neighborhoods)" = "Parisian",
                                     "Skyscrapers Everywhere" = "Legalize It"),
                         selected = 'A'),
      position = "bottom-left"
    ),
    mainPanel(
      leafletOutput("mainPlot", height = "600px"),
      span(verbatimTextOutput("helpText"), style = "color:red; font-size:20px"),
      position = "top-right",
      height = "600px"
    )
  )
)




# Server logic
server <- function(input, output) {
  updatedData <- reactiveVal(NA)
  
  # Update the reactive value whenever input features change
  observeEvent(input$scenario, {
    updatedData(update_df(input$scenario))
  })
  
  output$mainPlot <- renderLeaflet({
    generate_plot()
  })
  
  observeEvent(input$scenario, {
    df <- updatedData()  # Get the updated data
    waiter::transparent(0)
    waiter_show(# show the waiter
      id = "mainPlot",
      html = div(
        spin_1(),
        h3("Upzoning San Francisco - the EIR will take a second!"),
        color=  waiter::transparent(0)

      ))
    
    #df <- filter(df, (M3_ZONING != 'No Change') & !is.na(M3_ZONING))
    
    print(filter(df, (M3_ZONING == 'No Change') | is.na(M3_ZONING)) %>% nrow())
    print(table(filter(df, (M3_ZONING == 'No Change') | is.na(M3_ZONING))$expected_units))
    to_plot <- filter(df, !is.na(expected_units) & expected_units > 0)
    print('start group by')
    start <- Sys.time()
    to_plot['block'] <- stringr::str_sub(to_plot$mapblklot, 1, 4)
    to_plot <- to_plot %>%
      group_by(block, M3_ZONING) %>%
      summarise(expected_units = sum(expected_units),
                pdev = mean(pdev),
                expected_units_if_dev = sum(expected_units_if_dev))
    print(Sys.time() - start)
    to_plot['n_stories'] <- as.numeric(stringr::str_extract(to_plot$M3_ZONING, "\\d+")) %/% 10
    to_plot[to_plot$M3_ZONING == fourplex, 'n_stories'] <- 4
    to_plot[to_plot$M3_ZONING == decontrol, 'n_stories'] <- 4
    to_plot <- st_sf(to_plot)
    
    print('finish group by')
    start <- Sys.time()
    leafletProxy("mainPlot", 
                 data = to_plot) %>%
      clearGroup('parcels') %>%
      addPolygons(
        fillColor = ~ pal(n_stories),
        color = ~ pal(n_stories),
        fillOpacity = 1,
        weight = 1,
        group = 'parcels',
        popup = ~ paste(
          "New Zoning:",
          M3_ZONING,
          "<br>Expected Units:",
          formatC(round(expected_units, 1), format='f', big.mark=',', digits=1),
          '<br>P(Dev):',
          formatC(round(pdev * 100, 2), format='f', big.mark=',', digits=2),
          '%',
          '<br>E(Units | Dev):',
          formatC(round(expected_units_if_dev, 1), format='f', big.mark=',', digits=1)
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      )
    print(Sys.time() - start)
    waiter_hide() # hide the waiter
    
  })
  
  
  output$helpText <- renderText({
    added_capacity <- round(calculate_shortfall(df = updatedData()))
    print(added_capacity)
    updated_help <- paste(
      "Initial shortfall: 36,282 units. \nYou helped build: ",
      formatC(added_capacity, format="f", big.mark=",", digits=0),
      " new units by 2031."
    )
    congrats <- paste("\nYOU MET THE GOAL AND ADDED AN EXTRA", 
                      formatC(added_capacity - 36282, 
                              format="f", big.mark=",", digits=0),
                      "HOMES GO TEAM")
    sad <-paste("\nThe remaining shortfall is: ",
                         formatC(36282 - added_capacity, 
                                 format="f", big.mark=",", digits=0),
                ' units.')

    if (added_capacity > 36282) {
      result <- paste(updated_help, congrats)
      
      for (i in 1:5) {
        Sys.sleep(1)
        shinyjs::runjs("jsConfettiInstance.addConfetti();") # Use the instance to add confetti
      }
    } else {
      result <- paste(updated_help, sad)
    }
    return(result)
  })
  
}


shinyApp(ui = ui, server = server)


