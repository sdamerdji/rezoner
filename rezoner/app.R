library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(shinyjs)
library(shinyBS)
library(sfarrow)
library(stringr)

source('./ui.R', local=T)
model <- readRDS(file='./light_model.rds')
pal <- colorBin("viridis",  bins = c(1, 5, 8, 10, 20, Inf), right = F)

max_user_rezoning_height <- 240
df <- st_read_feather('./four_rezonings_v4.feather')
df['ZONING'] <- NA
geometries <- st_read_feather('./simple_geometries.feather')
info_on_lldl <- paste0("Large is defined as >= 2500 sq ft&#013;", 
                       "Low opportunity tracts are defined by the Draft 2024 TCAC Map&#013;",
                       "Lots with existing multi-family residential uses or multi-family zoning are excluded.")

# Years after rezoning
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

get_denser_zone <- function(current_zoning, new_zoning) {
  # Set current zoning to new zoning if new zoning is bigger, on a per-parcel level
  new_zoning_numeric <- as.numeric(str_extract(new_zoning, "\\d+"))
  current_zoning_numeric <- as.numeric(str_extract(current_zoning, "\\d+"))
  
  case_when(
    new_zoning_numeric > current_zoning_numeric ~ new_zoning,
    !is.na(new_zoning_numeric) & is.na(current_zoning_numeric) ~ new_zoning,
    !is.na(current_zoning) & is.na(new_zoning) ~ current_zoning,
    is.na(current_zoning) & !is.na(new_zoning) ~ new_zoning,
    TRUE ~ current_zoning
  )
}

union_of_maxdens <- function(df) {
  df %>% 
    mutate(ZONING = get_denser_zone(ZONING, M1_ZONING)) %>%
    mutate(ZONING = get_denser_zone(ZONING, M2_ZONING)) %>%
    mutate(ZONING = get_denser_zone(ZONING, M3_ZONING)) %>%
    mutate(ZONING = get_denser_zone(ZONING, M4_ZONING))
}


height_setter <- function(ZONING, height) {
  dplyr::case_when(
    !is.na(ZONING) & is.na(height) ~ 40,
    .default = height
  )
}


upzone <- function(df) {
  sum(pmax(df$expected_units - df$expected_units_baseline, 0, na.rm = T))
}

update_df <- function(scenario, extend, n_years) {
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
  boost <- 20
    if (extend == 'extend_affh') {
      # If fourplex zoning is the floor, then rezone low density parcels to fourplex zoning
      is_fourplex_floor <- fourplex %in% df$ZONING
      df <- df %>%
        mutate(ZONING = ifelse(is.na(ZONING) & high_opportunity & (!is.na(peg)) & (!peg) 
                               & is_fourplex_floor & (ex_height2024 <= 40),
                               fourplex,
                               ZONING)) %>%
        mutate(ZONING = ifelse(is.na(ZONING) & high_opportunity & (!is.na(peg)) & (!peg)
                               & is_fourplex_floor & (ex_height2024 > 40) & (ex_height2024 < max_user_rezoning_height - boost),
                               paste0(ex_height2024 + boost, "' Height Allowed"),
                               ZONING))
      
      # If parisian zoning is the floor, then rezone low density parcels to parisian zoning
      df <- df %>%
        mutate(ZONING = ifelse(is.na(ZONING) & high_opportunity & (!is.na(peg)) & (!peg) &
                                 !(is_fourplex_floor) & (ex_height2024 <= parisian_height),
                               parisian,
                               ZONING)) %>%
        mutate(ZONING = ifelse(is.na(ZONING) & high_opportunity & (!is.na(peg)) & (!peg) &
                                 !(is_fourplex_floor) & (ex_height2024 > parisian_height) & (ex_height2024 < max_user_rezoning_height - boost),
                               paste0(ex_height2024 + boost, "' Height Allowed"),
                               ZONING))
    }
    if (extend == 'extend_econ') {
      #TODO: I feel like !is.na(econ_affh) should not work
      is_fourplex_floor <- fourplex %in% df$ZONING
      df <- df %>%
        mutate(ZONING = ifelse(is.na(ZONING) & !is.na(econ_affh) & (!is.na(peg)) & (!peg) & (econ_affh > .9 ) 
                               & is_fourplex_floor & (ex_height2024 <= 40),
                               fourplex,
                               ZONING)) %>%
        mutate(ZONING = ifelse(is.na(ZONING) & !is.na(econ_affh) & (!is.na(peg)) & (!peg) & (econ_affh > .9 )  
                               & is_fourplex_floor & (ex_height2024 > 40) & (ex_height2024 < max_user_rezoning_height - boost),
                               paste0(ex_height2024 + boost, "' Height Allowed"),
                               ZONING))
      
      # If parisian zoning is the floor, then rezone low density parcels to parisian zoning
      df <- df %>%
        mutate(ZONING = ifelse(is.na(ZONING) & !is.na(econ_affh) & (!is.na(peg)) & (!peg) & (econ_affh > .9) &
                                 !(is_fourplex_floor) & ex_height2024 <= parisian_height,
                               parisian,
                               ZONING)) %>%
        mutate(ZONING = ifelse(is.na(ZONING) & !is.na(econ_affh) & (!is.na(peg)) & (!peg) & (econ_affh > .9) & 
                                 !(is_fourplex_floor) & (ex_height2024 > parisian_height) & (ex_height2024 < max_user_rezoning_height - boost),
                               paste0(ex_height2024 + boost, "' Height Allowed"),
                               ZONING))
    }
    if (extend ==  'extend_except_peg') {
      is_fourplex_floor <- fourplex %in% df$ZONING
      df <- df %>%
        mutate(ZONING = ifelse(is.na(ZONING)  & (!is.na(peg)) & (!peg)
                               & is_fourplex_floor & (ex_height2024 <= 40),
                               fourplex,
                               ZONING)) %>%
       mutate(ZONING = ifelse(is.na(ZONING) & (!peg)
                              & is_fourplex_floor & (ex_height2024 > 40) & (ex_height2024 < max_user_rezoning_height - boost),
                              paste0(ex_height2024 + boost, "' Height Allowed"),
                              ZONING))
      
      # If parisian zoning is the floor, then rezone low density parcels to parisian zoning
      df <- df %>%
        mutate(ZONING = ifelse(is.na(ZONING) & (!is.na(peg)) & (!peg) & 
                                 !(is_fourplex_floor) & ex_height2024 <= parisian_height,
                               parisian,
                               ZONING)) %>%
        mutate(ZONING = ifelse(is.na(ZONING) & (!peg) &
                                !(is_fourplex_floor) & (ex_height2024 > parisian_height) & (ex_height2024 < max_user_rezoning_height - boost),
                              paste0(ex_height2024 + boost, "' Height Allowed"),
                              ZONING))
    }
    if (extend == 'extend_errwhere') {
      is_fourplex_floor <- fourplex %in% df$ZONING
      df <- df %>%
        mutate(ZONING = ifelse(is.na(ZONING)  
                               & is_fourplex_floor & (ex_height2024 <= 40),
                               fourplex,
                               ZONING)) %>%
      mutate(ZONING = ifelse(is.na(ZONING) & (!peg)
                             & is_fourplex_floor & (ex_height2024 > 40) & (ex_height2024 < max_user_rezoning_height - boost),
                             paste0(ex_height2024 + boost, "' Height Allowed"),
                             ZONING))

      # If parisian zoning is the floor, then rezone low density parcels to parisian zoning
      df <- df %>%
        mutate(ZONING = ifelse(is.na(ZONING) &
                                 !(is_fourplex_floor) & ex_height2024 <= parisian_height,
                               parisian,
                               ZONING)) %>%
      mutate(ZONING = ifelse(is.na(ZONING) & (!peg) &
                              !(is_fourplex_floor) & (ex_height2024 > parisian_height) & (ex_height2024 < max_user_rezoning_height - boost),
                            paste0(ex_height2024 + boost, "' Height Allowed"),
                            ZONING))
    }
  df <- df[!is.na(df$ZONING),]
  
  # Erase existing zoning indicators for rezoned parcels
  df[, grep("^zp", names(df), value = TRUE)] <- 0
  
  # See page 30 of Appendix B in Scenario A for reasoning
  df <- df %>%
    mutate(zp_FormBasedMulti = if_else(ZONING != fourplex,
                                       1,
                                       zp_FormBasedMulti))
  
  df <- df %>%
    mutate(zp_DensRestMulti = if_else(ZONING == fourplex, 1, zp_DensRestMulti)) %>%
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
      existing_sqft = Envelope_1000 / Upzone_Ratio,
      Envelope_1000 = pmax(Envelope_1000_new, Envelope_1000, na.rm = TRUE),
      Upzone_Ratio = Envelope_1000 / existing_sqft
    ) %>%
    mutate(expected_units_if_dev = Envelope_1000 * 1000 * 0.8 / 850)
  
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
    mutate(pdev_baseline = 1 - (1-pdev_baseline_1yr)^n_years) %>%
    mutate(pdev_skyscraper = 1 - (1-pdev_skyscraper_1yr)^n_years) %>%
    mutate(expected_units = pdev * expected_units_if_dev) %>%
    mutate(expected_units_baseline = pdev_baseline * expected_units_baseline_if_dev) %>%
    mutate(expected_units_skyscraper = pdev_skyscraper * expected_units_skyscraper_if_dev)
  
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
      title = "Base Zoning",
      colors = pal(c(1, 5, 8, 10, 20)),
      labels = c("< 4 Stories", "5 - 7 Stories", "8 - 9 Stories", "10 - 19 Stories", "20+ Stories"),
      opacity = 0.5
    )
}

calculate_shortfall <- function(df) {
  df2 <- select(df, -geometry)
  return(upzone(df2))
}



# Server logic
server <- function(input, output) {
  updatedData <- reactiveVal(NA)
  
  # Update the reactive value whenever input features change
  observeEvent(c(input$scenario, input$extend, input$years_slider), {
    updatedData(update_df(input$scenario, input$extend, input$years_slider))
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
    overlay <- input$peg
    multipolygon <- readRDS('./peg.RDS')
    if (overlay) {
      # Add the multipolygon layer when overlay is TRUE
      leafletProxy("mainPlot") %>%
        addPolygons(data = multipolygon, fill = .1, group="peg", c='red', weight=.5)
      
    } else {
      leafletProxy("mainPlot") %>%
        clearGroup("peg")
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
    to_plot$n_stories <- (as.numeric(str_extract(to_plot$ZONING, "\\d+")) - 5) %/% 10
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
          '<br>Block:', block,
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
  
  output$supervisors <- renderText({
    values <- st_drop_geometry(updatedData()) %>% 
      mutate(net_units = pmax(expected_units - expected_units_baseline, 0, na.rm = T)) %>%
      group_by(sup_name) %>%
      summarise(units = sum(net_units, na.rm=T)) %>%
      arrange(desc(units))
    return(paste0(format(values$sup_name, width=15), "\t", round(values$units), ' units\n'))
  })
  
  output$helpText <- renderText({
    added_capacity <- round(calculate_shortfall(df = updatedData()))
    print(added_capacity)
    updated_help <- paste(
      "Initial shortfall: 36,282 units. \nYou helped build: ",
      formatC(added_capacity, format="f", big.mark=",", digits=0),
      "new units in",
      input$years_slider,
      'years.'
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
  
  # observe({
  #   if(input$customize_map == "yes") {
  #     # Change cursor to paint roller
  #     shinyjs::runjs('document.getElementById("mainPlot").style.cursor = "url(/paint-brush/brownpntbrush.cur), auto";')
  #   } else {
  #     # Revert cursor to default
  #     shinyjs::runjs('document.getElementById("mainPlot").style.cursor = "default";')
  #   }
  # })
  
}


shinyApp(ui = ui, server = server)


