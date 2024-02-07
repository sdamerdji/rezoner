library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(shinyjs)
library(sfarrow)
library(stringr)
library(RColorBrewer)
library(compiler)
source('./modules.R', local=T)
source('./ui.R', local=T)
model <- readRDS(file='./light_model.rds') 
pal <- colorBin("viridis",  bins = c(1, 5, 8, 10, 20, Inf), right = F)
options(shiny.fullstacktrace=TRUE)

#df <- st_read_feather('./four_rezonings_v5.feather')
df <- readRDS('./four_rezonings_v5.RDS')

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


height_setter <- function(ZONING, new_height, old_height) {
  dplyr::case_when(
    ZONING == fourplex ~ pmax(old_height, 40),
    ZONING == decontrol ~ pmax(old_height, 40),
    !is.na(ZONING) & !is.na(new_height) ~ new_height,
    .default = old_height
  )
}


upzone <- function(df) {
  sum(pmax(df$expected_units - df$expected_units_baseline, 0))
}

update_df_ <- function(scenario, n_years, user_rezonings) {
  # Given site inventory df inner joined with history 
  # Control upzoning by changing M3_ZONING before passing df in
  # Return df with fields "Expected" and "Pdev"
  # if extend affh, then where high opp but not already upzoned, upzone to either floor (fourplex) or height + 20
  print(scenario)
  # TODO: Check that df$zp_ vars are the same each iteration even tho overwritten
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
  
  #squo_zoning <- df[is.na(df$ZONING),]
  #df <- df[!is.na(df$ZONING),]
  #browser()
  
  # USER REZONING
  for (list_item in user_rezonings) {
    df <- mutate(df, 
                 ZONING = ifelse(eval(parse(text=list_item$new_expr)),
                                 list_item$new_height_description, 
                                 ZONING))
  }
  # See page 30 of Appendix B in Scenario A for reasoning
  building_efficiency_discount <- .8
  typical_unit_size <- 850

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
           height = height_setter(ZONING, height, ex_height2024),
           height_deduction = if_else(height <= 50, 10, 15), #TODO: unselect
           n_floors_residential = (height - height_deduction) %/% 10, #TODO: unselect
           #n_floors_residential = height / 10.5, # David's code
           lot_coverage_discount = if_else(ACRES > 1, .55, .75), #TODO: unselect
           lot_coverage_discount = if_else((height > 85) & (ACRES > 1.5), .75, lot_coverage_discount), # To me, this is illogical but it's what the HE says
           #lot_coverage_discount = if_else(height > 85, .8, lot_coverage_discount), # David's code
           ground_floor = (ACRES * 43560) * lot_coverage_discount,
           Envelope_1000_new = ground_floor * (n_floors_residential + 1) / 1000,
           n_floors_residential = if_else((ACRES * 43560 <= 12000 & (height > 85)), # Cap at 12 for towers on small lots
                                          pmin(n_floors_residential, 12), 
                                          n_floors_residential),
           expected_built_envelope = case_when(
             height <= 85 ~ ground_floor * n_floors_residential,
             height > 85 & (ACRES * 43560 < 12000) ~ ground_floor * n_floors_residential,
             height > 85 & (ACRES * 43560 < 45000) ~ ground_floor * 7 + 12000 * pmax(n_floors_residential - 7, 0),
             TRUE ~ ground_floor * 7 + round(ACRES) * 12000 * pmax(n_floors_residential - 7, 0) # Repl 7 with n_floors to recreate STATA
           ),
           expected_units_if_dev = expected_built_envelope * building_efficiency_discount / 850,
           # no downzoning allowed
           existing_sqft = if_else(Upzone_Ratio != 0, Envelope_1000 / Upzone_Ratio, 0),
           Envelope_1000 = pmax(Envelope_1000_new, Envelope_1000),
           Upzone_Ratio = if_else(existing_sqft > 0, Envelope_1000 / existing_sqft, 0), # This, to me, is wrong, but it's how Blue Sky data is coded
           expected_units_if_dev = if_else(ZONING == fourplex, pmin(expected_units_if_dev, 6), expected_units_if_dev)
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
      expected_units_skyscraper = pdev_skyscraper * expected_units_skyscraper_if_dev,
      net_units = pmax(expected_units - expected_units_baseline, 0)
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
server <- function(input, output, session) {
  updatedData <- reactiveVal(NA)
  user_rezoning <- reactiveValues(lists=list())
  
  # Update the reactive value whenever input features change
  observeEvent(c(input$scenario, input$years_slider, user_rezoning$lists), {
    updatedData(update_df(input$scenario, input$years_slider, user_rezoning$lists))
  })
  
  output$mainPlot <- renderLeaflet({
    generate_plot()
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
      df[!is.na(df$ZONING) & df$ZONING == fourplex & is.na(df$n_stories), 'n_stories'] <- 4
      df[!is.na(df$ZONING) & df$ZONING == decontrol & is.na(df$n_stories), 'n_stories'] <- 4
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
    df$n_stories <- (df$height - 5) %/% 10
    df[!is.na(df$ZONING) & df$ZONING == fourplex & is.na(df$n_stories), 'n_stories'] <- 4
    df[!is.na(df$ZONING) & df$ZONING == decontrol & is.na(df$n_stories), 'n_stories'] <- 4
    
    
    # Render
    start <- Sys.time()
    
    if (input$map == 'heights') {
      print('plot height')
      to_plot <- filter(df, !is.na(ZONING) & !is.na(expected_units) & expected_units > 0)
      to_plot <- st_drop_geometry(to_plot)
      print(paste0('pre group by took: ', round(Sys.time() - start, 1)))
      # TODO: there is a fundamental problem I think in this code that I use
      # a group by that includes ZONING
      # I need to filter out parcels before simplify_geometries.R
      to_plot <- to_plot %>%
        group_by(block, M1_ZONING, M2_ZONING, M3_ZONING, M4_ZONING, ZONING) %>%
        summarise(
          pdev = mean(pdev),
          expected_units_if_dev = sum(expected_units_if_dev),
          n_stories = first(n_stories),
          expected_units = sum(expected_units),
          expected_units_baseline = sum(expected_units_baseline),
          net_units = sum(net_units),
          affh2023 = first(affh2023),
          ex_height2024 = first(ex_height2024),
          sb330_applies = first(sb330_applies),
          .groups='keep')

      to_plot <- st_sf(left_join(to_plot, geometries,
                                 by=c('block', 'M1_ZONING', 'M2_ZONING',
                                      'M3_ZONING', 'M4_ZONING')))
      # TODO: Fix this in simplify_geometries.R
      # unmatched_geos <- geometries[!(geometries$geometry %in% to_plot$geometry),]
      # unmatched_blocks <- st_drop_geometry(to_plot[st_is_empty(to_plot),])
      # if (nrow(unmatched_blocks) > 1) {
      #   extra <- st_sf(left_join(unmatched_blocks,  
      #                      select(unmatched_geos, 'block'),
      #                      by=c('block')))
      #   to_plot <- rbind(extra, to_plot)
      # }
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
            '<br>Existing height:', ex_height2024, 
            '<br>Block:', block,
            '<br>TCAC:', affh2023,
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
                     '<br>Missing Potential per Acre:', round((10**missing_potential) - 1, 1),
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
      to_plot <- filter(df, !is.na(ZONING) & !is.na(net_units) & net_units > 0)
      sf_use_s2(T)
      to_plot$net_units <- log10(1 + (to_plot$net_units/to_plot$ACRES))
      pal_eu <- colorNumeric(
        palette = "Blues",
        domain = c(0, 3))
      leafletProxy("mainPlot", data = to_plot) %>%
        clearGroup('parcels') %>%
        clearGroup('dynamicMarkers') %>%
        clearControls() %>%
        addCircles(lng = to_plot$lng,
                   lat = to_plot$lat,
                   color = ~pal_eu(pmin(net_units, 3)),
                   radius =  ~pmin(ACRES, .5, na.rm=T) * sqrt(4046.86),
                   group = 'dynamicMarkers',
                   fillOpacity = ~(ifelse(net_units < 1, .1, 1)), 
                   weight = 0,
                   popup = ~ paste(
                     "New Zoning:",
                     ZONING,
                     '<br>APN:', mapblklot,
                     "<br>Expected Units:",
                     formatC(round(net_units, 1),
                             format='f', big.mark=',', digits=1),
                     '<br>P(Dev):',
                     formatC(round(pdev * 100, 2), format='f', big.mark=',', digits=2),
                     '%',
                     '<br>E(Units | Dev):',
                     formatC(round(expected_units_if_dev, 1), format='f', big.mark=',', digits=1)
                   )) %>%
        addLegend(
          "bottomright",
          title = "Expected Units Added per Acre",
          colors = pal_eu(c(1, 2, 3)),
          labels = c('<10 units', '10-100 units', '100+ units')
        )
    } else if (input$map == 'sim') {
      to_plot <- filter(df, !is.na(expected_units) & expected_units > 0)
      simulate_buildout(to_plot)
    }
  })
  
  output$supervisors <- renderText({
    values <- st_drop_geometry(updatedData()) %>% 
      group_by(sup_name) %>%
      summarise(units = sum(net_units)) %>%
      arrange(desc(units))
    allocations <- paste0(format(values$sup_name, width=15), "\t", round(values$units), " units")
    allocations_string <- paste(allocations, collapse="\n")
    final_output <- paste0('Allocation by Supervisor\n', allocations_string)
    
    return(final_output)
  })
  
  output$most_units <- renderText({
    values <- st_drop_geometry(updatedData()) %>% 
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
  
  requirements <- reactiveValues(count = 0, ids = list())
  
  
  observeEvent(input$add_requirement, {
    req_id <- paste0("requirement", requirements$count + 1)
    requirements$count <- requirements$count + 1
    requirements$ids <- c(requirements$ids, req_id)
    # Ensure a div wrapper or similar container for easy removal
    insertUI(
      selector = "#add_requirement", 
      where = "beforeBegin", 
      ui = div(id = req_id, requirementUI(req_id))
    )
    requirementServer(req_id)
  })
  
  observeEvent(input$delete_requirement, {
    if (requirements$count > 0) {
      last_id <- requirements$ids[[requirements$count]]
      # Correctly target the div wrapper for removal
      removeUI(selector = paste0("#", last_id))
      requirements$count <- requirements$count - 1
      requirements$ids <- head(requirements$ids, -1)
    }
  })
  
  observeEvent(input$rezone, {
    new_height <- 5 + 10*input$stories
    new_height_description <- paste0(new_height, "' Height Allowed")
    new_expr <- paste0('TRUE & !((ex_height2024 > ', new_height, ') & sb330_applies)')
    for (prefix in requirements$ids) {
      to_add <- NULL
      parcel_filter <- input[[paste0(prefix, '-parcel_filter')]]
      if (parcel_filter == 'PEG') {
        to_add <- 'peg'
      }
      else if (parcel_filter == 'Already Rezoned') {
        to_add <- '!is.na(ZONING)'
      }
      else if (parcel_filter == 'Economic Opportunity') {
        econ_threshold <- input[[paste0(prefix, '-economic_score')]]
        to_add <- paste0('(econ_affh > ', as.numeric(econ_threshold) / 100, ')')
      }
      else if (parcel_filter == 'TCAC') {
        threshold <- input[[paste0(prefix, '-affh_score')]]
        in_expr <- ''
        if (threshold == 'Low') {
          in_expr = "c('Low Resource', 'Moderate Resource', 'High Resource', 'Highest Resource')"
        } else if (threshold == 'Medium') {
          in_expr = "c('Moderate Resource', 'High Resource', 'Highest Resource')"
        } else if (threshold == 'High') {
          in_expr = "c('High Resource', 'Highest Resource')"
        }
        to_add <- paste0('(!is.na(affh2023) & affh2023 %in% ', in_expr, ')')
      }
      else if (parcel_filter == 'Transit') {
        distance <- as.numeric(input[[paste0(prefix, '-distance')]])
        relevant_transit <- input[[paste0(prefix, '-transit_options')]]
        conditions <- c()
        if('Caltrain Stops' %in% relevant_transit) {
          conditions <- c(conditions, paste0('(transit_dist_caltrain < ', distance, ')'))
        }
        
        if('BART Stops' %in% relevant_transit) {
          conditions <- c(conditions, paste0('(transit_dist_bart < ', distance, ')'))
        }
        
        if('Rapid Bus Line' %in% relevant_transit) {
          conditions <- c(conditions, paste0('(transit_dist_rapid < ', distance, ')'))
        }
        if('Bus Lines (<10 min headways)' %in% relevant_transit) {
          conditions <- c(conditions, paste0('(transit_dist < ', distance, ')'))
        }
        to_add <- paste(conditions, collapse = ' | ')
      }
      else if (parcel_filter == 'Rapid Bus Line') {
        distance <- input[[paste0(prefix, '-distance')]]
        to_add <- paste0('(transit_dist_rapid < ', distance, ')')
      }
      else if (parcel_filter == 'Commercial Corridor') {
        distance <- input[[paste0(prefix, '-distance')]]
        to_add <- paste0('(commercial_dist < ', as.numeric(distance), ')')
      }
      else if (parcel_filter == 'Lot Size') {
        sqft_lot_size <- input[[paste0(prefix, '-lot_size')]]
        to_add <- paste0('(ACRES > ', as.numeric(sqft_lot_size) / 43560, ')')
      }
      else if (parcel_filter == 'Neighborhood') {
        nhood <- input[[paste0(prefix, '-hood')]]
        to_add <- paste0('(nhood == "', nhood, '")')
      }
      if (input[[paste0(prefix, '-is_in')]] == 'not in') {
        to_add <- paste0('(!', to_add, ')')
      }
      new_expr <- paste(new_expr, to_add, sep=' & ')
    }
    user_rezoning[['lists']][[paste0('list', 
                          length(user_rezoning$lists))]] <- list(new_height_description = new_height_description, 
                                                           new_expr = new_expr)
    
    # Correctly remove each dynamically added component
    lapply(requirements$ids, function(id) removeUI(selector = paste0("#", id)))
    requirements$count <- 0
    requirements$ids <- list()
  })
}


# observe({
#   if(input$customize_map == "yes") {
#     # Change cursor to paint roller
#     shinyjs::runjs('document.getElementById("mainPlot").style.cursor = "url(/paint-brush/brownpntbrush.cur), auto";')
#   } else {
#     # Revert cursor to default
#     shinyjs::runjs('document.getElementById("mainPlot").style.cursor = "default";')
#   }
# })




shinyApp(ui = ui, server = server)
