library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(shinyjs)
library(shinyBS)
library(sfarrow)
library(stringr)
library(RColorBrewer)
library(compiler)
library(profvis)

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
  df[(!is.na(df$ZONING)) 
     & ((df$ZONING == fourplex) | 
       (df$ZONING == decontrol) | 
       (as.numeric(str_extract(df$ZONING, "\\d+")) < parisian_height)), 'ZONING'] <- parisian

  df[is.na(df$ZONING) & (!is.na(df$M1_ZONING) | 
                           !is.na(df$M2_ZONING) |
                           !is.na(df$M3_ZONING) |
                           !is.na(df$M4_ZONING) |
                           !is.na(df$M5_ZONING)), 'ZONING'] <- parisian
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
    mutate(ZONING = get_denser_zone(ZONING, M1_ZONING),
           ZONING = get_denser_zone(ZONING, M2_ZONING),
           ZONING = get_denser_zone(ZONING, M3_ZONING),
           ZONING = get_denser_zone(ZONING, M4_ZONING),
           ZONING = get_denser_zone(ZONING, M5_ZONING))
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

update_df_ <- function(scenario, extend, n_years) {
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
  if (scenario == 'E') {
    df['ZONING'] <- df$M5_ZONING
  }
  if (scenario == 'Union') {
    df <- union_of_maxdens(df)
  }
  if (scenario == 'Parisian') {
    df['ZONING'] <- df$M5_ZONING
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
  #squo_zoning <- df[is.na(df$ZONING),]
  #df <- df[!is.na(df$ZONING),]

  # See page 30 of Appendix B in Scenario A for reasoning
  df <- df %>%
    mutate(zp_OfficeComm = if_else(!is.na(ZONING), 0, zp_OfficeComm),
           zp_PDRInd = if_else(!is.na(ZONING), 0, zp_PDRInd),
           zp_Public = if_else(!is.na(ZONING), 0, zp_Public),
           zp_Redev = if_else(!is.na(ZONING), 0, zp_Redev),
           zp_RH2 = if_else(!is.na(ZONING), 0, zp_RH2),
           zp_RH3_RM1 = if_else(!is.na(ZONING), 0, zp_RH3_RM1),
           zp_FormBasedMulti = if_else(ZONING != fourplex & !is.na(ZONING),
                                       1,
                                       0),
           zp_DensRestMulti = if_else(ZONING == fourplex & !is.na(ZONING), 1, 0),
           height = as.numeric(str_extract(ZONING, "\\d+")),
           height = height_setter(ZONING, height),
           Envelope_1000_new = case_when(
              height >= 85 ~ ((ACRES * 43560) * 0.8 * height / 10.5) / 1000,
              ACRES >= 1 & height < 85 ~ ((ACRES * 43560) * 0.55 * height / 10.5) / 1000,
              ACRES < 1 ~ ((ACRES * 43560) * 0.75 * height / 10.5) / 1000, # Swap lines from Rmd bc this logic matches STATA code
              TRUE ~ NA_real_
            ),
            # no downzoning allowed
            existing_sqft = Envelope_1000 / Upzone_Ratio,
            Envelope_1000 = pmax(Envelope_1000_new, Envelope_1000, na.rm = TRUE),
            Upzone_Ratio = Envelope_1000 / existing_sqft,
            expected_units_if_dev = Envelope_1000 * 1000 * 0.8 / 850
      )
  
  if (scenario == 'A' | scenario == 'B' | scenario == 'C') { # This is another change from Rmd
    print("dont allow E[U|D] to exceed sf planninig's claim")
    df <- df %>%
      mutate(MAXDENS = abs(MAXDENS),
             du_allowed = ACRES * MAXDENS,
             expected_units_if_dev = ifelse((!is.na(du_allowed)) & (expected_units_if_dev > du_allowed),
                                                du_allowed, expected_units_if_dev))
  }
  
  predictions.16 <- predict(model, newdata = df, type = "response")
  
  df <- df %>%
    mutate(
      pdev = if_else(is.na(ZONING), 
                     1 - (1 - pdev_baseline_1yr)^n_years,
                     1 - (1-predictions.16)^n_years),
      pdev_baseline = 1 - (1-pdev_baseline_1yr)^n_years,
      pdev_skyscraper = 1 - (1-pdev_skyscraper_1yr)^n_years,
      expected_units_if_dev = if_else(is.na(ZONING), 
                                      expected_units_baseline_if_dev, 
                                      expected_units_if_dev),
      expected_units = pdev * expected_units_if_dev,
      expected_units_baseline = pdev_baseline * expected_units_baseline_if_dev,
      expected_units_skyscraper = pdev_skyscraper * expected_units_skyscraper_if_dev
    ) %>%
    select(-Envelope_1000_new, -existing_sqft)
  print(paste0('Dataframe update took: ', round(Sys.time() - start, 1)))
  return(df)
}

update_df <- cmpfun(update_df_)

generate_plot <- function() {
  leaflet(df) %>%
    addProviderTiles(providers$CartoDB.Positron, 
                     options = providerTileOptions(minZoom = 12, maxZoom = 16)) %>%
    setMaxBounds(lng1 = -122.5149, lat1 = 37.7081, lng2 = -122.3564, lat2 = 37.8324)  
}

calculate_shortfall <- function(df) {
  df2 <- select(df, -geometry)
  return(upzone(df2))
}

simulate_buildout <- function(to_plot) {
  sf_use_s2(T)
  to_plot <- to_plot[runif(nrow(to_plot)) < to_plot$pdev, ]
  to_plot[,'expected_units_if_dev'] <- to_plot$expected_units_if_dev / max(to_plot$expected_units_if_dev) * 1000

  leafletProxy("mainPlot", data = to_plot) %>%
    clearGroup('parcels') %>%
    clearGroup('dynamicMarkers') %>%
    addCircles(lng = to_plot$lng,
               lat = to_plot$lat,
               radius = ~expected_units_if_dev,
               group = 'dynamicMarkers')
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
  observeEvent(c(input$resimulateBtn, input$map, input$scenario), {
    if (input$map == 'sim') {
      df <- updatedData()
      df['block'] <- str_sub(df$mapblklot, 1, 4)
      df$n_stories <- (df$height - 5) %/% 10
      df[!is.na(df$ZONING) & df$ZONING == fourplex, 'n_stories'] <- 4
      df[!is.na(df$ZONING) & df$ZONING == decontrol, 'n_stories'] <- 4
      to_plot <- filter(df, !is.na(df$ZONING) & !is.na(expected_units) & expected_units > 0)
      simulate_buildout(to_plot)
    }
  })
  
  observeEvent({
    list(updatedData(), input$map)
  },
  {
    print('updating map')
    print(input$map)
    df <- updatedData()  # Get the updated data

    
    # Group by
    df['block'] <- str_sub(df$mapblklot, 1, 4)
    df$n_stories <- (as.numeric(str_extract(df$ZONING, "\\d+")) - 5) %/% 10
    df[!is.na(df$ZONING) & df$ZONING == fourplex, 'n_stories'] <- 4
    df[!is.na(df$ZONING) & df$ZONING == decontrol, 'n_stories'] <- 4

    
    # Render
    start <- Sys.time()
    
    if (input$map == 'heights') {
      print('plot height')
      to_plot <- filter(df, !is.na(ZONING) & !is.na(expected_units) & expected_units > 0)
      to_plot <- st_drop_geometry(to_plot)
      print(paste0('pre group by took: ', round(Sys.time() - start, 1)))
      
      to_plot <- to_plot %>%
        group_by(block, M1_ZONING, M2_ZONING, M3_ZONING, M4_ZONING, ZONING) %>%
        summarise(
                  pdev = mean(pdev),
                  expected_units_if_dev = sum(expected_units_if_dev),
                  n_stories = first(n_stories),
                  expected_units = sum(expected_units),
                  expected_units_baseline = sum(expected_units_baseline),
                  net_units = pmax(expected_units - expected_units_baseline, 0, na.rm=T),
                  .groups='keep')
      
      to_plot <- st_sf(left_join(to_plot, geometries, 
                                 by=c('block', 'M1_ZONING', 'M2_ZONING', 'M3_ZONING', 'M4_ZONING')))
      #to_plot <- st_cast(to_plot, "MULTIPOLYGON")
      print(paste0('Group by took: ', round(Sys.time() - start, 1)))
      start <- Sys.time() 
      leafletProxy("mainPlot", 
                   data = to_plot) %>%
        clearGroup('parcels') %>%
        clearGroup('dynamicMarkers') %>%
        clearControls() %>%
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
            formatC(round(net_units, 1),
                    format='f', big.mark=',', digits=1),
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
          )) %>%
            addLegend(
              "bottomright",
              title = "Base Zoning",
              colors = pal(c(1, 5, 8, 10, 20)),
              labels = c("< 4 Stories", "5 - 7 Stories", "8 - 9 Stories", "10 - 19 Stories", "20+ Stories"),
              opacity = 0.5
            )
      print(paste0('Render took: ', round(Sys.time() - start, 1)))
      
        
    } 
    else if (input$map == "potential") {
      print('wya')
      sf_use_s2(T)
      to_plot <- df
      to_plot$missing_potential <- to_plot$expected_units_skyscraper - to_plot$expected_units
      to_plot$missing_potential <- pmax(to_plot$missing_potential, 0, na.rm=T) / to_plot$ACRES
      to_plot$missing_potential <- log10(1 + to_plot$missing_potential)
      pal_pot <- colorNumeric(
        palette = "Blues",
        domain = c(0, 5))
      
      #browser()
      leafletProxy("mainPlot", data = to_plot) %>%
        clearGroup('parcels') %>%
        clearGroup('dynamicMarkers') %>%
        clearControls() %>%
        addCircles(lng = to_plot$lng,
                         lat = to_plot$lat,
                         radius = ~pmin(to_plot$ACRES, .5, na.rm=T) * sqrt(4046.86),
                         color = ~pal_pot(missing_potential),
                         group = 'dynamicMarkers',
                         fillOpacity = 1, 
                         weight = 0,
                   popup = ~ paste(
                     "New Zoning:",
                     ZONING,
                     '<br>APN:', mapblklot,
                     '<br>Missing Potential per Acre:', round(10**(missing_potential - 1), 1),
                     "<br>Expected Units:",
                     formatC(round(expected_units, 1),
                             format='f', big.mark=',', digits=1),
                     '<br>P(Dev):',
                     formatC(round(pdev * 100, 2), format='f', big.mark=',', digits=2),
                     '%',
                     '<br>E(Units | Dev):',
                     formatC(round(expected_units_if_dev, 1), format='f', big.mark=',', digits=1)
                   )) %>%
        
        addLegend(
          "bottomright",
          title = "Potential",
          colors = pal_pot(c(1, 2, 3, 4)),
          labels = c('<10 du/ac', '10-100 du/ac', '100-1000 du/ac', '1000+ du/ac')
        )
      print('done')
    } 
    else if (input$map == 'E[u]') {
      to_plot <- filter(df, !is.na(ZONING) & !is.na(expected_units) & expected_units > 0)
      
      print('eu')
      sf_use_s2(T)
      to_plot$expected_units <- log(to_plot$expected_units)
      pal_eu <- colorNumeric(
        palette = "Blues",
        domain = to_plot$expected_units)
      leafletProxy("mainPlot", data = to_plot) %>%
        clearGroup('parcels') %>%
        clearGroup('dynamicMarkers') %>%
        clearControls() %>%
        addCircles(lng = to_plot$lng,
                   lat = to_plot$lat,
                         color = ~pal_eu(expected_units),
                   radius = 10,
                         group = 'dynamicMarkers',
                         fillOpacity = .1, 
                         weight = .1) %>%
        addLegend(
          "bottomright",
          title = "E[Units]",
          colors = pal_eu(quantile(to_plot$expected_units, na.rm=T)),
          labels = as.character(seq(0, 1, 0.25))
        )
    } else if (input$map == 'sim') {
      to_plot <- filter(df, !is.na(expected_units) & expected_units > 0)
      simulate_buildout(to_plot)
    }
  })
  
  output$supervisors <- renderText({
    values <- st_drop_geometry(updatedData()) %>% 
      mutate(net_units = pmax(expected_units - expected_units_baseline, 0, na.rm = T)) %>%
      group_by(sup_name) %>%
      summarise(units = sum(net_units, na.rm=T)) %>%
      arrange(desc(units))
    allocations <- paste0(format(values$sup_name, width=15), "\t", round(values$units), " units")
    allocations_string <- paste(allocations, collapse="\n")
    final_output <- paste0('Allocation by Supervisor\n', allocations_string)
    
    return(final_output)
  })
  
  output$most_units <- renderText({
    values <- st_drop_geometry(updatedData()) %>% 
      mutate(net_units = pmax(expected_units - expected_units_baseline, 0, na.rm = T)) %>%
      arrange(desc(net_units)) %>%
      head(5) %>%
      select(mapblklot, ZONING, ACRES, pdev, net_units, expected_units)
    yields <- paste0('Lot ', format(values$mapblklot), 
                  " yields ", round(values$net_units), 
                  ' units with P(dev) = ', round(100*values$pdev, 1), '% ',
                  'with ', values$ZONING, ' and ', round(values$ACRES, 1), 
                  ' acres\n', collapse='')
    return(paste0('Top Lots\n', yields))
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
