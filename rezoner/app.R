library(shiny)
library(dplyr)
library(sf)
library(leaflet)
#library(waiter)
library(shinyjs)
library(sfarrow)
library(stringr)

model <- readRDS(file='./light_model.rds')
pal <- colorBin("viridis",  bins = c(1, 5, 8, 10, 20, Inf), right = F)

#pal <- colorBin("viridis",  bins = c(0, 1, 5, 10, 100, Inf), right = F)

df <- st_read_feather('./four_rezonings_v4.feather')
df['ZONING'] <- NA
geometries <- st_read_feather('./simple_geometries.feather')


# Years after rezoning
n_years <- 5
fourplex <- "Increased density up to four units (six units on corner lots)"
decontrol <- "No height change, density decontrol"
skyscrapers <-  "300' Height Allowed"
parisian_height <- 75
parisian <- paste0(parisian_height, "' Height Allowed")

paris <- function(df) {
  # Find all M3_ZONING with fourplex or decontrol and set to 55'

  df[(!is.na(df$ZONING)) & df$ZONING == fourplex, 'ZONING'] <- parisian
  df[(!is.na(df$ZONING)) & df$ZONING == decontrol, 'ZONING'] <- parisian
  df[(!is.na(df$ZONING))
     & as.numeric(str_extract(df$ZONING, "\\d+")) < 75, 'ZONING'] <- parisian
  
  # This just ensures it's one of the parcels in an existing SF rezoning map
  df[is.na(df$ZONING) & (!is.na(df$M1_ZONING)| 
                          !is.na(df$M2_ZONING)|
                           !is.na(df$M3_ZONING)|
                              !is.na(df$M4_ZONING)), 'ZONING'] <- parisian
  df
}

legalize_it <- function(df) {
  df[, 'ZONING'] <- skyscrapers
  df
}

union_of_maxdens <- function(df) {
  # Set M3 Zoning to be maximum upzoning of M1 and M2. Surely this is covered by EIR
  df %>% 
    mutate(ZONING =
             case_when(
               as.numeric(str_extract(M1_ZONING, "\\d+")) > as.numeric(str_extract(ZONING,  "\\d+")) ~ M1_ZONING,
               !is.na(as.numeric(str_extract(M1_ZONING, "\\d+"))) & is.na(as.numeric(str_extract(M3_ZONING,  "\\d+"))) ~ M1_ZONING,
               TRUE ~ M1_ZONING
             )) %>%
    mutate(ZONING = 
             case_when(
               as.numeric(str_extract(M2_ZONING, "\\d+")) > as.numeric(str_extract(ZONING,  "\\d+")) ~ M2_ZONING,
               !is.na(as.numeric(str_extract(M2_ZONING, "\\d+"))) & is.na(as.numeric(str_extract(ZONING,  "\\d+"))) ~ M2_ZONING,
               TRUE ~ ZONING
             )) %>%
    mutate(ZONING = 
             case_when(
               as.numeric(str_extract(M3_ZONING, "\\d+")) > as.numeric(str_extract(ZONING,  "\\d+")) ~ M3_ZONING,
               !is.na(as.numeric(str_extract(M3_ZONING, "\\d+"))) & is.na(as.numeric(str_extract(ZONING,  "\\d+"))) ~ M3_ZONING,
               TRUE ~ ZONING
             )
    ) %>%
     mutate(ZONING = 
              case_when(
                as.numeric(str_extract(M4_ZONING, "\\d+")) > as.numeric(str_extract(ZONING,  "\\d+")) ~ M4_ZONING,
                !is.na(as.numeric(str_extract(M4_ZONING, "\\d+"))) & is.na(as.numeric(str_extract(ZONING,  "\\d+"))) ~ M4_ZONING,
                TRUE ~ ZONING
                )
    )
}

height_setter <- function(ZONING, height) {
  dplyr::case_when(
    !is.na(ZONING) & is.na(height) ~ 40,
    .default = height
  )
}


upzone <- function(df) {
  # print('upzoning... kinda a fake name')
  # print('sum(df$expected_units, na.rm=F)')
  # print(sum(df$expected_units, na.rm=F))
  # print('sum(df$expected_units, na.rm=T)')
  # print(sum(df$expected_units, na.rm=T))
  (total_expected_units <- sum(df$expected_units, na.rm=T))
}

update_df <- function(scenario, extend_affh, extend_econ) {
  # Given site inventory df inner joined with history 
  # Control upzoning by changing M3_ZONING before passing df in
  # Return df with fields "Expected" and "Pdev"
  # if extend affh, then where high opp but not already upzoned, upzone to either floor (fourplex) or height + 20
  print(scenario)
  start <- Sys.time()

  if (scenario == 'A') {
    df['ZONING'] <- df$M1_ZONING
    df['MAXDENS'] <- df$M1_MAXDENS
  }
  if (scenario == 'B') {
    df['ZONING'] <- df$M2_ZONING
    df['MAXDENS'] <- df$M2_MAXDENS
  }
  if (scenario == 'C') {
    df['ZONING'] <- df$M3_ZONING
    df['MAXDENS'] <- df$M3_MAXDENS
  }
  if (scenario == 'D') {
    df['ZONING'] <- df$M4_ZONING
  }
  if (scenario == 'Union') {
    df <- union_of_maxdens(df)
  }
  if (scenario == 'Parisian') {
    df <- union_of_maxdens(df)
    df <- paris(df)
  }
  if (scenario == 'Legalize It') {
    df <- legalize_it(df)
  }
  else {
    if (extend_affh == TRUE) {
      #browser()
      # If fourplex zoning is the floor, then rezone low density parcels to fourplex zoning
      is_fourplex_floor <- fourplex %in% df$ZONING
      boost <- 20
      df <- df %>%
        mutate(ZONING = ifelse(is.na(ZONING) & high_opportunity & is_fourplex_floor & ex_height2024 <= 40,
                               fourplex,
                               ZONING)) %>%
        mutate(ZONING = ifelse(is.na(ZONING) & high_opportunity 
                               & is_fourplex_floor & (ex_height2024 > 40) & (ex_height2024 < 300 - boost),
                               paste0(ex_height2024 + 20, "' Height Allowed"),
                               ZONING))
      
      # If parisian zoning is the floor, then rezone low density parcels to parisian zoning
      df <- df %>%
        mutate(ZONING = ifelse(is.na(ZONING) & high_opportunity &
                                 !(is_fourplex_floor) & (ex_height2024 <= parisian_height),
                               parisian,
                               ZONING)) %>%
        mutate(ZONING = ifelse(is.na(ZONING) & high_opportunity &
                                 !(is_fourplex_floor) & (ex_height2024 > parisian_height) & (ex_height2024 < 300 - boost),
                               paste0(ex_height2024 + 20, "' Height Allowed"),
                               ZONING))
    }
    if (extend_econ == TRUE) {
      is_fourplex_floor <- fourplex %in% df$ZONING
      df <- df %>%
        mutate(ZONING = ifelse(is.na(ZONING) & !is.na(econ_affh) & (econ_affh > .9 ) 
                               & is_fourplex_floor & ex_height2024 <= 40,
                               fourplex,
                               ZONING)) %>%
        mutate(ZONING = ifelse(is.na(ZONING) & !is.na(econ_affh) & (econ_affh > .9 )  
                               & is_fourplex_floor & (ex_height2024 > 40) & (ex_height2024 < 300 - boost),
                               paste0(ex_height2024 + 20, "' Height Allowed"),
                               ZONING))
      
      # If parisian zoning is the floor, then rezone low density parcels to parisian zoning
      df <- df %>%
        mutate(ZONING = ifelse(is.na(ZONING) & !is.na(econ_affh) & (econ_affh > .9) &
                                 !(is_fourplex_floor) & ex_height2024 <= parisian_height,
                               parisian,
                               ZONING)) %>%
        mutate(ZONING = ifelse(is.na(ZONING) & !is.na(econ_affh) & (econ_affh > .9) & 
                                 !(is_fourplex_floor) & (ex_height2024 > parisian_height) & (ex_height2024 < 300 - boost),
                               paste0(ex_height2024 + 20, "' Height Allowed"),
                               ZONING))
    }
  }
  # Erase existing zoning indicators for rezoned parcels
  df[!is.na(df$ZONING), 
     grep("^zp", names(df), value = TRUE)
     ] <- 0
  
  # So I don't have to handle missing values
  df[is.na(df$ZONING), 'ZONING'] <- 'No Change'
  
  # See page 30 of Appendix B in Scenario A for reasoning
  df <- df %>%
    mutate(zp_FormBasedMulti = if_else(!(ZONING == 'No Change' | ZONING == fourplex),
                                       1,
                                       zp_FormBasedMulti))
  
  df <- df %>%
    mutate(zp_DensRestMulti = if_else(ZONING == fourplex | ZONING == 'No Change', 1, zp_DensRestMulti)) %>%
    mutate(height = as.numeric(str_extract(ZONING, "\\d+"))) %>%
    mutate(height = height_setter(ZONING, height)) %>%
    mutate(
      # See page 44 of Appendix B
      Envelope_1000_new = case_when(
        height >= 85 ~ ((ACRES * 43560) * 0.8 * height / 10.5) / 1000,
        ACRES >= 1 & height < 85 ~ ((ACRES * 43560) * 0.55 * height / 10.5) / 1000,
        ACRES < 1 ~ ((ACRES * 43560) * 0.75 * height / 10.5) / 1000, # Swap lines from Rmd bc this logic matches STATA code
        TRUE ~ NA_real_
      ),
      # no downzoning allowed
      Envelope_1000_new = pmax(Envelope_1000_new, Envelope_1000, na.rm = TRUE),
      existing_sqft = Envelope_1000 / Upzone_Ratio,
      Upzone_Ratio_new = Envelope_1000_new / existing_sqft
    ) %>%
    mutate(
      Envelope_1000 = if_else(!(ZONING == 'No Change'), Envelope_1000_new, Envelope_1000),
      Upzone_Ratio = if_else(!(ZONING == 'No Change'), Upzone_Ratio_new, Upzone_Ratio)
    ) %>%
    mutate(expected_units_if_dev = ifelse(ZONING != 'No Change', 
                                          Envelope_1000 * 1000 * 0.8 / 850, 0))
  
  if (scenario == 'A' | scenario == 'B' | scenario == 'C') { # This is another change from Rmd
    print("dont allow E[U|D] to exceed sf planninig's claim")
    df <- df %>%
      mutate(MAXDENS = abs(MAXDENS)) %>%
      mutate(du_allowed = ACRES * MAXDENS) %>%
      mutate(expected_units_if_dev = ifelse((!is.na(du_allowed)) & (expected_units_if_dev > du_allowed),
                                                du_allowed, expected_units_if_dev))
  }
  
  predictions.16 <- predict(model, newdata = df, type = "response")
  df <- df %>%
    mutate(pdev = 1 - (1-predictions.16)^n_years) %>% 
    mutate(expected_units = pdev * expected_units_if_dev)
  
  print(paste0('Dataframe update took: ', round(Sys.time() - start, 1)))
  return(df)
}

generate_plot <- function() {
  leaflet(df) %>%
    addProviderTiles(providers$CartoDB.Positron, 
                     options = providerTileOptions(minZoom = 12, maxZoom = 16)) %>%
    setMaxBounds(lng1 = -122.5149, lat1 = 37.7081, lng2 = -122.3564, lat2 = 37.8324)  %>%
    addLegend(
      "bottomright",
      title = "Stories",
      colors = pal(c(1, 5, 8, 10, 20)),
      labels = c("1 - 4", "5 - 7", "8 - 9", "10 - 19", "20+"),
      opacity = 0.5
    )
}

calculate_shortfall <- function(df) {
  df2 <- select(df, -geometry)
  return(upzone(df2))
}

# UI definition
ui <- fluidPage(
  #useWaiter(), # include dependencies
  shinyjs::useShinyjs(),  # Initialize shinyjs
  id = "main_content",
  tags$head(
    tags$script(src = "./js-confetti.browser.js"),
    tags$script(HTML("
      var jsConfettiInstance; // Global confetti instance
      document.addEventListener('DOMContentLoaded', function() {
        jsConfettiInstance = new JSConfetti(); // Instantiate when the document is ready
      });
      
      // Define the sprayConfetti function
      function sprayConfetti() {
        for (var i = 0; i < 2; i++) {
            setTimeout(function() {
                jsConfettiInstance.addConfetti();
            }, 1000 * i); // Delay of 1 second (1000 milliseconds) between each spray
        }
      }
    "))
  ),
  tags$head(
    tags$link(rel = "shortcut icon", type = "image/png", href = "sfy.png")
  ),
  titlePanel(
    "Upzone the City"
  ),
  sidebarLayout(
    sidebarPanel(
      radioButtons("scenario", "Upzoning Strategies:",
                         choices = c("Current SF Planning Proposal for Housing Element Rezoning" = "D",
                                     "Housing Element Rezoning Scenario A" = "A",
                                     "Housing Element Rezoning Scenario B" = "B",
                                     "Housing Element Rezoning Scenario C" = "C",
                                     "Take the boldest elements of scenarios A, B, C, and the current proposal" = "Union", 
                                     "Parisian zoning in low density neighborhoods" = "Parisian",
                                     "Skyscrapers everywhere" = "Legalize It"),
                         selected = 'D'),
      radioButtons("customize_map", "Customize this rezoning:",
                   choices = c("No" = "no", "Yes" = "yes"),
                   selected = "no"),
      conditionalPanel(
        condition = "input.customize_map == 'yes'",
        radioButtons("stories", "Select number of stories:",
                     choices = c("5 stories", "8 stories", "12 stories", "20 stories"),
                     selected = NULL),
        actionButton("reset_map", "Reset", icon = icon("sync")),
      ),
      HTML("<b>Overlay options:</b>"),
      checkboxInput("lldl", "Overlay large, low density lots (outside low opportunity tract)"),
      checkboxInput("affh", "Overlay high opportunity tracts per Draft 2024 TCAC Map"),
      HTML("<b>Extend rezoning:</b>"),
      checkboxInput("extend_affh", "Extend rezoning to high opportunity areas per Draft 2024 TCAC Map"),
      checkboxInput("extend_econ", "Extend rezoning to areas with high economic opportunity"),
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
  observeEvent(c(input$scenario, input$extend_affh, input$extend_econ), {
    updatedData(update_df(input$scenario, input$extend_affh, input$extend_econ))
  })
  
  output$mainPlot <- renderLeaflet({
    generate_plot()
  })
  
  observe({
    overlay <- input$lldl
    multipolygon <- readRDS('./growth.RDS')
    if (overlay) {
      # Add the multipolygon layer when overlay is TRUE
      leafletProxy("mainPlot") %>%
        addPolygons(data = multipolygon, fill = NA, group="lldl", weight=.5)

    } else {
      leafletProxy("mainPlot") %>%
        clearGroup("lldl")
    }
  })
  
  observe({
    overlay <- input$affh
    multipolygon <- readRDS('./high_opp.RDS')
    if (overlay) {
      # Add the multipolygon layer when overlay is TRUE
      leafletProxy("mainPlot") %>%
        addPolygons(data = multipolygon, fill = NA, group="affh", color='black', weight=1)

    } else {
      leafletProxy("mainPlot") %>%
        clearGroup("affh")
    }
  })
  
  observe({
    df <- updatedData()  # Get the updated data
    # waiter::transparent(0)
    # waiter_show(# show the waiter
    #   id = "mainPlot",
    #   html = div(
    #     spin_1(),
    #     h3("Upzoning San Francisco - the EIR will take a second!"),
    #     color=  waiter::transparent(0)
    # 
    #   ))
    
    #df <- filter(df, (M3_ZONING != 'No Change') & !is.na(M3_ZONING))
    
    #print(filter(df, (ZONING == 'No Change') | is.na(ZONING)) %>% nrow())
    #print(table(filter(df, (ZONING == 'No Change') | is.na(ZONING))$expected_units))
    to_plot <- filter(df, !is.na(expected_units) & expected_units > 0)
    
    # Group by
    start <- Sys.time()
    to_plot['block'] <- str_sub(to_plot$mapblklot, 1, 4)
    to_plot <- st_drop_geometry(to_plot)
    to_plot <- to_plot %>%
      group_by(block, M1_ZONING, M2_ZONING, M3_ZONING, M4_ZONING, ZONING) %>%
      summarise(expected_units = sum(expected_units),
                pdev = mean(pdev),
                expected_units_if_dev = sum(expected_units_if_dev),
                .groups='keep')
    to_plot <- st_sf(left_join(to_plot, geometries, 
                               by=c('block', 'M1_ZONING', 'M2_ZONING', 'M3_ZONING', 'M4_ZONING')))
    to_plot$n_stories <- as.numeric(str_extract(to_plot$ZONING, "\\d+")) %/% 10
    to_plot[to_plot$ZONING == fourplex, 'n_stories'] <- 4
    to_plot[to_plot$ZONING == decontrol, 'n_stories'] <- 4
    to_plot <- st_sf(to_plot)
    to_plot <- st_cast(to_plot, "MULTIPOLYGON")
    print(paste0('Group by took: ', round(Sys.time() - start, 1)))
    
    # Render
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
          ZONING,
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
    print(paste0('Rendering took: ', round(Sys.time() - start, 1)))
    #waiter_hide() # hide the waiter
    
  })
  
  
  output$helpText <- renderText({
    added_capacity <- round(calculate_shortfall(df = updatedData()))
    print(added_capacity)
    updated_help <- paste(
      "Initial shortfall: 36,282 units. \nYou helped build: ",
      formatC(added_capacity, format="f", big.mark=",", digits=0),
      "new units by 2031."
    )
    congrats <- paste("\nYOU MET THE GOAL AND ADDED AN EXTRA", 
                      formatC(added_capacity - 36282, 
                              format="f", big.mark=",", digits=0),
                      "HOMES GO TEAM")
    sad <-paste("\nThe remaining shortfall is:",
                         formatC(36282 - added_capacity, 
                                 format="f", big.mark=",", digits=0),
                'units.')

    if (added_capacity > 36282) {
      result <- paste(updated_help, congrats)
      
      shinyjs::runjs("sprayConfetti();")
      
    } else {
      result <- paste(updated_help, sad)
    }
    return(result)
  })
  
  observe({
    if(input$customize_map == "yes") {
      # Change cursor to paint roller
      shinyjs::runjs('document.getElementById("mainPlot").style.cursor = "url(/paint-brush/brownpntbrush.cur), auto";')
    } else {
      # Revert cursor to default
      shinyjs::runjs('document.getElementById("mainPlot").style.cursor = "default";')
    }
  })
  
}


shinyApp(ui = ui, server = server)


