library(shiny)
library(dplyr)
library(sf)
library(shinyjs)
library(shinyWidgets)
library(stringr)
library(viridis)
library(RColorBrewer)
library(compiler)
source('./modules.R', local=T)
source('./ui.R', local=T)
source('./utils.R', local=T)
model <- readRDS(file='./light_model.rds') 

pal <- function(x, bins = c(1, 6, 8, 14, 24, Inf), colors = c("#D2EBEB", "#F4AD3E", "#EC7520", "#E44520", "#AE2922", "#6C2F18"), right = FALSE) {
  binIndex <- findInterval(x, vec = bins, rightmost.closed = right, all.inside = TRUE)
  return(colors[binIndex])
}

options(shiny.fullstacktrace=TRUE)
LAYER_ID = 'parcels_id'

df <- readRDS('./five_rezonings_nongeo.RDS')
df_mapbox <- readRDS('./sf_map.RDS')


# Years after rezoning
max_envelope <- max(df$Envelope_1000)
fourplex <- 'Increased density up to four units'
sixplex <- 'Increased density up to six units'
density_restricted <- c(fourplex, sixplex)
decontrol <- "No height change, density decontrol"
skyscrapers <-  "300' Height Allowed"
parisian_height <- 75
parisian <- paste0(parisian_height, "' Height Allowed")
fill_style =
  list(id = "parcels_id", type = "fill", source = "parcels_source",
       fill_antialias = F,
       filter = list("in", "mapblklot",  "fake"),
       paint = list("fill-color" = "blue"))

paris <- function(df) {
  # Find all M3_ZONING with fourplex or decontrol and set to 55'
  df[(!is.na(df$ZONING)) 
     & ((df$ZONING %in% density_restricted) | 
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
    ZONING %in% density_restricted ~ pmax(old_height, 40),
    ZONING == decontrol ~ pmax(old_height, 40),
    !is.na(ZONING) & !is.na(new_height) ~ new_height,
    .default = old_height
  )
}


upzone <- function(df) {
  sum(pmax(df$expected_units - df$expected_units_baseline, 0))
}

yimbytown <- function(df) {
  df %>%
    union_of_maxdens() %>%
    # General transit upzoning in high opp areas with step down
    mutate(ZONING = ifelse((transit_dist_bart < .115 | transit_dist_caltrain < .115 | transit_dist_rapid < .115) &
                             !peg & !((ex_height2024 > 85) & sb330_applies) &
                             (affh2023 %in% c('High Resource', 'Highest Resource')),
                           get_denser_zone("85' Height Allowed", ZONING),
                           ZONING)) %>%
    mutate(ZONING = ifelse((transit_dist_bart < .25 | transit_dist_caltrain < .25 | transit_dist_rapid < .25) &
                             !peg & !((ex_height2024 > 55) & sb330_applies) &
                             (affh2023 %in% c('High Resource', 'Highest Resource')),
                           get_denser_zone("55' Height Allowed", ZONING),
                           ZONING)) %>%
    # Transit-oriented uzponing with step down for southern part of the city
    mutate(ZONING = ifelse((transit_dist_bart < .1) &
                             !peg & !((ex_height2024 > 120) & sb330_applies) &
                             nhood %in% c('Outer Mission', 'Glen Park', 'Excelsior',
                                          'Bernal Heights', 'Noe Valley', 'Oceanview/Merced/Ingleside') &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')),
                           get_denser_zone("120' Height Allowed", ZONING),
                           ZONING))  %>%
    mutate(ZONING = ifelse((transit_dist_bart < .115 | transit_dist_caltrain < .115 | transit_dist_rapid < .115) &
                             !peg & !((ex_height2024 > 85) & sb330_applies) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             nhood %in% c('Outer Mission', 'Glen Park', 'Excelsior',
                                          'Bernal Heights', 'Noe Valley', 'Oceanview/Merced/Ingleside'),
                           get_denser_zone("85' Height Allowed", ZONING),
                           ZONING)) %>%
    mutate(ZONING = ifelse((transit_dist_bart < .25 | transit_dist_caltrain < .25 | transit_dist_rapid < .25) &
                             !peg & !((ex_height2024 > 55) & sb330_applies) &
                             nhood %in% c('Outer Mission', 'Glen Park', 'Excelsior', 
                                          'Bernal Heights', 'Noe Valley', 'Oceanview/Merced/Ingleside') &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')),
                           get_denser_zone("55' Height Allowed", ZONING),
                           ZONING)) %>%
    # Transit-oriented upzoning with step down for northern part of the city
    mutate(ZONING = ifelse((transit_dist_bart < .115 | transit_dist_caltrain < .115 | transit_dist_rapid < .115) &
                             !peg & !((ex_height2024 > 120) & sb330_applies) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             nhood %in% c('Presidio Heights', 'Pacific Heights', 'Nob Hill', 'Japantown', 'Lone Mountain/USF'),
                           get_denser_zone("120' Height Allowed", ZONING),
                           ZONING)) %>%
    mutate(ZONING = ifelse((transit_dist_bart < .25 | transit_dist_caltrain < .25 | transit_dist_rapid < .25) &
                             !peg & !((ex_height2024 > 85) & sb330_applies) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             nhood %in% c('Presidio Heights', 'Pacific Heights', 'Nob Hill', 'Japantown', 'Lone Mountain/USF'),
                           get_denser_zone("85' Height Allowed", ZONING),
                           ZONING)) %>%
    # Set a higher zoning floor for Marina, Russian Hill, Pac Heights, Nob Hill, North Beach 
    mutate(ZONING = ifelse(!peg & !((ex_height2024 > 55) & sb330_applies) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             (nhood %in% c('Marina', 'Russian Hill', 'Pacific Heights', 'Nob Hill', 'North Beach')),
                           get_denser_zone("55' Height Allowed", ZONING),
                           ZONING)) %>%
    # Transit-oriented upzoning for Portrero and Mission Bay
    mutate(ZONING = ifelse((transit_dist_bart < .25 | transit_dist_caltrain < .25 | transit_dist_rapid < .25) &
                             !peg & !((ex_height2024 > 85) & sb330_applies) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             is.na(ZONING) &
                             nhood %in% c('Potrero Hill', 'Mission Bay'),
                           get_denser_zone("85' Height Allowed", ZONING),
                           ZONING)) %>%
    mutate(ZONING = ifelse((transit_dist_bart < .5 | transit_dist_caltrain < .5 | transit_dist_rapid < .5) &
                             !peg & !((ex_height2024 > 65) & sb330_applies) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             is.na(ZONING) &
                             nhood %in% c('Potrero Hill', 'Mission Bay'),
                           get_denser_zone("65' Height Allowed", ZONING),
                           ZONING)) %>%
    # Geary and Masonic intersection
    mutate(ZONING = ifelse((stringr::str_sub(df$mapblklot, 1, 3) %in% c('107', '108', '109')) &
                             !((ex_height2024 > 240) & sb330_applies),
                           get_denser_zone("240' Height Allowed", ZONING),
                           ZONING))     
}

yimbycity <- function(df) {
  # Even better than a town is a city
  df %>%
    union_of_maxdens() %>%
    # General transit upzoning in high opp areas with step down
    mutate(ZONING = ifelse((transit_dist_bart < .25 | transit_dist_caltrain < .25 | transit_dist_rapid_stops < .25) &
                             !peg & !((ex_height2024 > 85) & sb330_applies) &
                             (affh2023 %in% c('High Resource', 'Highest Resource')),
                           get_denser_zone("85' Height Allowed", ZONING),
                           ZONING)) %>%
    mutate(ZONING = ifelse((transit_dist_bart < .33 | transit_dist_caltrain < .33 | transit_dist_rapid_stops < .33) &
                             !peg & !((ex_height2024 > 55) & sb330_applies) &
                             (affh2023 %in% c('High Resource', 'Highest Resource')),
                           get_denser_zone("55' Height Allowed", ZONING),
                           ZONING)) %>%
    # Transit-oriented uzponing with step down for southern part of the city
    mutate(ZONING = ifelse((transit_dist_bart < .1) &
                             !peg & !((ex_height2024 > 120) & sb330_applies) &
                             nhood %in% c('Outer Mission', 'Glen Park', 'Excelsior',
                                          'Bernal Heights', 'Noe Valley', 'Oceanview/Merced/Ingleside') &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')),
                           get_denser_zone("120' Height Allowed", ZONING),
                           ZONING))  %>%
    mutate(ZONING = ifelse((transit_dist_bart < .25 | transit_dist_caltrain < .25 | transit_dist_rapid_stops < .25) &
                             !peg & !((ex_height2024 > 85) & sb330_applies) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             nhood %in% c('Outer Mission', 'Glen Park', 'Excelsior',
                                          'Bernal Heights', 'Noe Valley', 'Oceanview/Merced/Ingleside'),
                           get_denser_zone("85' Height Allowed", ZONING),
                           ZONING)) %>%
    mutate(ZONING = ifelse((transit_dist_bart < .33 | transit_dist_caltrain < .33 | transit_dist_rapid_stops < .33) &
                             !peg & !((ex_height2024 > 55) & sb330_applies) &
                             nhood %in% c('Outer Mission', 'Glen Park', 'Excelsior', 
                                          'Bernal Heights', 'Noe Valley', 'Oceanview/Merced/Ingleside') &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')),
                           get_denser_zone("55' Height Allowed", ZONING),
                           ZONING)) %>%
    # Transit-oriented upzoning with step down for northern part of the city
    mutate(ZONING = ifelse((transit_dist_bart < .25 | transit_dist_caltrain < .25 | transit_dist_rapid_stops < .25) &
                             !peg & !((ex_height2024 > 120) & sb330_applies) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             nhood %in% c('Presidio Heights', 'Pacific Heights', 'Nob Hill', 'Japantown', 'Lone Mountain/USF'),
                           get_denser_zone("120' Height Allowed", ZONING),
                           ZONING)) %>%
    mutate(ZONING = ifelse((transit_dist_bart < .33 | transit_dist_caltrain < .33 | transit_dist_rapid_stops < .33) &
                             !peg & !((ex_height2024 > 85) & sb330_applies) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             nhood %in% c('Presidio Heights', 'Pacific Heights', 'Nob Hill', 'Japantown', 'Lone Mountain/USF'),
                           get_denser_zone("85' Height Allowed", ZONING),
                           ZONING)) %>%
    # Set a higher zoning floor for Marina, Russian Hill, Pac Heights, Nob Hill, North Beach 
    mutate(ZONING = ifelse(!peg & !((ex_height2024 > 55) & sb330_applies) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             (nhood %in% c('Marina', 'Russian Hill', 'Pacific Heights', 'Nob Hill', 'North Beach')),
                           get_denser_zone("55' Height Allowed", ZONING),
                           ZONING)) %>%
    # Transit-oriented upzoning for Portrero and Mission Bay
    mutate(ZONING = ifelse((transit_dist_bart < .25 | transit_dist_caltrain < .25 | transit_dist_rapid_stops < .25) &
                             !peg & !((ex_height2024 > 85) & sb330_applies) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             is.na(ZONING) &
                             nhood %in% c('Potrero Hill', 'Mission Bay'),
                           get_denser_zone("85' Height Allowed", ZONING),
                           ZONING)) %>%
    mutate(ZONING = ifelse((transit_dist_bart < .33 | transit_dist_caltrain < .33 | transit_dist_rapid_stops < .33) &
                             !peg & !((ex_height2024 > 55) & sb330_applies) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             is.na(ZONING) &
                             nhood %in% c('Potrero Hill', 'Mission Bay'),
                           get_denser_zone("55' Height Allowed", ZONING),
                           ZONING)) %>%
    # Geary and Masonic intersection
    mutate(ZONING = ifelse((stringr::str_sub(df$mapblklot, 1, 3) %in% c('107', '108', '109')) &
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             !((ex_height2024 > 240) & sb330_applies),
                           get_denser_zone("240' Height Allowed", ZONING),
                           ZONING)) %>%
    # Within .1 miles of parks to 55'
    mutate(ZONING = ifelse((park_dist < .1) &
                             !peg & 
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             !((ex_height2024 > 55) & sb330_applies),
                           get_denser_zone("55' Height Allowed", ZONING),
                           ZONING)) %>%
    # Within .1 miles of a college, to 55
    mutate(ZONING = ifelse((college_dist < .1) &
                             !peg & 
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             !((ex_height2024 > 55) & sb330_applies),
                           get_denser_zone("55' Height Allowed", ZONING),
                           ZONING)) %>%
    mutate(ZONING = ifelse(((stringr::str_sub(df$mapblklot, 1, 4) %in% c('3536', '2540')) | # Castro Safeway, Arden Wood
                              (df$mapblklot %in% c('1095005', '1098050', '1079025', '1539003')) |  # Inner RIchmond Kaiser Permanente
                              (df$mapblklot %in% c('1691019', '0446002', '0446003'))) & # La Playa Safeway and Marina Safeway
                             !peg & 
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             
                             !((ex_height2024 > 140) & sb330_applies),
                           get_denser_zone("140' Height Allowed", ZONING),
                           ZONING)) %>%
    mutate(ZONING = ifelse(((df$mapblklot %in% c('1094001', '1214017', '1214008', '1214001'))) & # City center, dmv , 
                             !peg & 
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             
                             !((ex_height2024 > 300) & sb330_applies),
                           get_denser_zone("300' Height Allowed", ZONING),
                           ZONING)) %>%
    mutate(ZONING = ifelse(((stringr::str_sub(df$mapblklot, 1, 5) %in% c('14380', '72555'))) & # smart final, lakeshore plaza
                             !peg & 
                             (affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')) & 
                             
                             !((ex_height2024 > 85) & sb330_applies),
                           get_denser_zone("85' Height Allowed", ZONING),
                           ZONING))
}
# 1094001 - city center

update_df_ <- function(scenario, n_years, user_rezonings, stack_sdbl) {
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
  if (scenario == 'BR') {
    df['ZONING'] <- df$BR_ZONING
  }
  if (scenario == 'F') {
    df['M6_ZONING'] <- df$M5_ZONING
    # density decontrol in HRN
    df[
      (df$affh2023 %in% c('High Resource', 'Highest Resource')) & 
      !is.na(df$M6_ZONING) & (df$M5_ZONING %in% density_restricted), 'M6_ZONING'] <-  "45' Height Allowed"
    df[
      (df$affh2023 %in% c('High Resource', 'Highest Resource')) & 
        is.na(df$M6_ZONING) & !((df$ex_height2024 >= 45) & df$sb330_applies), 'M6_ZONING'] <-  "45' Height Allowed"
    
    df[
      (df$affh2023 %in% c('High Resource', 'Highest Resource')) & 
        !is.na(df$M6_ZONING) & (df$M6_ZONING == 'No height change, density decontrol')
      & !((df$ex_height2024 >= 45) & df$sb330_applies), 'M6_ZONING'] <- "45' Height Allowed"
    df['ZONING'] <- df$M6_ZONING
  }
  if (scenario == 'Union') {
    df <- union_of_maxdens(df)
    
  }
  if (scenario == 'Parisian') {
    df['ZONING'] <- df$M5_ZONING
    df <- paris(df)
  }
  if (scenario == 'yimby1') {
    df <- yimbytown(df)
    
    # .1 miles from rapid transit, bart, caltrain, 
    # Upzone 
  }
  if (scenario == 'yimby2') {
    df <- yimbytown(df)
    df[!is.na(df$ZONING) & (df$ZONING %in% density_restricted), 'ZONING'] <- decontrol
    # .1 miles from rapid transit, bart, caltrain, 
    # Upzone 
  }
  if (scenario == 'yimby3') {
    df <- yimbycity(df)
    df[!is.na(df$ZONING) & (df$ZONING %in% density_restricted), 'ZONING'] <- decontrol
  }
  
  #squo_zoning <- df[is.na(df$ZONING),]
  #df <- df[!is.na(df$ZONING),]

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
    mutate(
      # Update zoning indicators for blue sky regression
      zp_OfficeComm = if_else(!is.na(ZONING), 0, zp_OfficeComm),
      zp_PDRInd = if_else(!is.na(ZONING), 0, zp_PDRInd),
      zp_Public = if_else(!is.na(ZONING), 0, zp_Public),
      zp_Redev = if_else(!is.na(ZONING), 0, zp_Redev),
      zp_RH2 = if_else(!is.na(ZONING), 0, zp_RH2), 
      zp_RH3_RM1 = if_else(!is.na(ZONING) & (ZONING == fourplex), 1, zp_RH3_RM1),
      zp_FormBasedMulti = if_else(!is.na(ZONING) & !(ZONING %in% density_restricted), # Tool only permits user to define form-based rezonings
                                  1,
                                  zp_FormBasedMulti),
      zp_FormBasedMulti = if_else(!is.na(ZONING) & (ZONING %in% density_restricted), # Tool only permits user to define form-based rezonings
                                  0,
                                  zp_FormBasedMulti),
      zp_DensRestMulti = if_else(!is.na(ZONING) & (ZONING == sixplex), 1, zp_DensRestMulti),
      zp_DensRestMulti = if_else(!is.na(ZONING) & !(ZONING %in% density_restricted), 0, zp_DensRestMulti),
      
      
      # Extract height from zoning name
      height = as.numeric(str_extract(ZONING, "\\d+")),
      height = height_setter(ZONING, height, ex_height2024),
      height_deduction = if_else(height <= 50, 10, 15),
      n_floors_residential = (height - height_deduction) %/% 10,
      
      # Lot coverage discount -> affects Envelope_1000
      lot_coverage_discount = if_else(ACRES > 1, .55, .75),

      # Envelope_1000
      ground_floor = (ACRES * 43560) * lot_coverage_discount,
      n_floors_residential = if_else((ACRES * 43560 <= 12000 & (height > 85)), # Cap at 12 for towers on small lots
                                     pmin(n_floors_residential, 12), 
                                     n_floors_residential),
      expected_built_envelope = case_when(
        height <= 85 ~ ground_floor * n_floors_residential,
        height > 85 & (ACRES * 43560 < 12000) ~ ground_floor * n_floors_residential,
        height > 85 & (ACRES * 43560 < 45000) ~ ground_floor * 7 + 12000 * pmax(n_floors_residential - 7, 0),
        TRUE ~ ground_floor * 7 + round(ACRES) * 12000 * pmax(n_floors_residential - 7, 0)
      ),
      expected_built_envelope = expected_built_envelope * building_efficiency_discount,
      expected_built_envelope = pmin(expected_built_envelope, max_envelope * 1000),
      Envelope_1000_new = expected_built_envelope / 1000,
      
      expected_units_if_dev = expected_built_envelope / typical_unit_size,
      
      existing_sqft = if_else(Upzone_Ratio != 0, Envelope_1000 / Upzone_Ratio, 0),
      
      # No downzoning allowed
      Envelope_1000 = pmax(Envelope_1000_new, Envelope_1000),
      
      # Upzone ratio
      Upzone_Ratio = if_else(existing_sqft > 0, Envelope_1000 / existing_sqft, 0), # This, to me, is wrong, but it's how Blue Sky data is coded
      
      # Density restrictions
      expected_units_if_dev = if_else(!is.na(ZONING) & ZONING == fourplex, pmin(expected_units_if_dev, 4), expected_units_if_dev),
      expected_units_if_dev = if_else(!is.na(ZONING) & ZONING == sixplex, pmin(expected_units_if_dev, 6), expected_units_if_dev)

    )
  # BR density restrictions
  if (scenario == 'BR') {
    df$expected_units_if_dev <- pmin(df$expected_units_if_dev, df$builders_remedy_du_acre * df$ACRES, na.rm=T)
  }
  sdbl <- 1 + .4 * .6
  if (stack_sdbl) {
      # Add density bonus
      df <- df %>% mutate(
        Envelope_1000 = if_else(expected_units_if_dev > 5, Envelope_1000 * sdbl, Envelope_1000),
        Upzone_Ratio = if_else(existing_sqft > 0, Envelope_1000 / existing_sqft, 0),
        expected_units_if_dev = if_else(expected_units_if_dev > 5, expected_units_if_dev * sdbl, expected_units_if_dev)
      )
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
  print(df %>% group_by(ZONING) %>% summarize(round(sum(net_units)), n()))
  print(df %>% arrange(desc(net_units)) %>% select(mapblklot, net_units, expected_built_envelope) %>% head(10))
  print(paste0('Dataframe update took: ', round(Sys.time() - start, 1)))
  return(df)
}

update_df <- cmpfun(update_df_)

generate_plot <- function() {
  mapboxer(maxBounds = c(c(-122.5747, 37.7), c(-122.3192, 37.83)),
           center = c((-122.62473 - 122.26922)/2, (37.65792 + 37.85880)/2),
           attributionControl = FALSE,
           style = basemaps$Carto$positron,
           maxZoom = 16,
           minZoom = 10,
           zoom = 11.1
  ) %>%
    add_source(df_mapbox, id='parcels_source') %>%
    add_layer(style=fill_style, popup='APN: {{mapblklot}}')
}

calculate_shortfall <- function(df) {
  return(upzone(df))
}

simulate_buildout <- function(to_plot) {
  sf_use_s2(T)
  to_plot <- to_plot[runif(nrow(to_plot)) < to_plot$pdev, ]
  to_plot[,'expected_units_if_dev'] <- to_plot$expected_units_if_dev / max(to_plot$expected_units_if_dev) * 1000
  # leafletProxy("mainPlot", data = to_plot) %>%
  #   clearGroup('parcels') %>%
  #   clearGroup('dynamicMarkers') %>%
  #   addCircles(lng = to_plot$lng,
  #              lat = to_plot$lat,
  #              radius = ~expected_units_if_dev,
  #              group = 'dynamicMarkers')
}

# Server logic
server <- function(input, output, session) {
  updatedData <- reactiveVal(NA)
  user_rezoning <- reactiveValues(lists=list())
  
  # Update the reactive value whenever input features change
  observeEvent(c(input$scenario, input$years_slider, user_rezoning$lists, input$stack_sdbl), {
    if (input$years_slider <= 10 & input$years_slider >= 5){
      if (input$scenario == "BR") {
        
      updatedData(update_df(input$scenario, input$years_slider, user_rezoning$lists, T))
      } else{
        updatedData(update_df(input$scenario, input$years_slider, user_rezoning$lists, F))
        
      }
    }
  })
  observeEvent(input$scenario, {
    if (input$scenario == "BR") {
      updateSwitchInput(session, "stack_sdbl", value = TRUE)
    } else {
      updateSwitchInput(session, "stack_sdbl", value = FALSE)
    }
  })
  output$dynamicRezoneHeader <- renderUI({
    # Check if the list is empty
    if(length(user_rezoning$lists) == 0) {
      h4('Or add your own!')
    } else {
      # If not empty, select a random message
      # messages <- c('Why not add another?\nThis city surely has got room for more', 
      #               'Why not add another?\nThis city surely has got room for more', 
      #               'You can rezone again\n', 
      #               
      #               'Now that\'s starting to look like a city.\n Why not upzone even more?')


      if ((length(user_rezoning$lists) == 1) && ('list0' %in% names(user_rezoning$lists)) && nchar(user_rezoning$lists$list0$new_expr[1]) == 47) {
      chosen_message <- "Or add your own!\nWhy not click 'specify where' to rezone with precision."
      }
      
      else if ((length(user_rezoning$lists) == 1)) {
        chosen_message <- 'Add another rezoning!\nWant to define another rezoning? You can create as many as you like.'
      }
      
      else if ((length(user_rezoning$lists) > 1)) {
        chosen_message <- 'Add another rezoning!\nLooks like you got the hang of it!'
      }
      
      else if ((length(user_rezoning$lists) > 2)) {
        chosen_message <- 'Nice work!\nLooks like you got the hang of it!'
      }
      else if ((length(user_rezoning$lists) > 3)) {
        chosen_message <- 'Nice work!'
      }
      
      
      # Split long messages for display
      if(nchar(chosen_message) > 30) {
        split_point <- regexpr("\n", chosen_message)
        part1 <- substr(chosen_message, 1, split_point[1]-1)
        part2 <- substr(chosen_message, split_point[1]+1, nchar(chosen_message))
        tagList(h4(part1), HTML(part2))
      } else {
        h4(chosen_message)
      }
    }
  })
  
  output$mainPlot <- renderMapboxer({
    generate_plot()
  })
  
  # observe({
  #   overlay <- input$peg
  #   multipolygon <- readRDS('./peg.RDS')
  #   if (overlay) {
  #     # Add the multipolygon layer when overlay is TRUE
  #     # leafletProxy("mainPlot") %>%
  #     #   addPolygons(data = multipolygon, fill = .1, group="peg", c='red', weight=.5)
  #     print('FIX')
  #   } 
  # })
  # 
  # observe({
  #   overlay <- input$affh
  #   multipolygon <- readRDS('./high_opp.RDS')
  #   if (overlay) {
  #     # Add the multipolygon layer when overlay is TRUE
  #     # leafletProxy("mainPlot") %>%
  #     #   addPolygons(data = multipolygon, fill = NA, group="affh", color='black', weight=1)
  #     # 
  #     print("FIX")
  #   } 
  # })
  # observeEvent(c(input$resimulateBtn, input$map, input$scenario), {
  #   if (input$map == 'sim') {
  #     df <- updatedData()
  #     df['block'] <- str_sub(df$mapblklot, 1, 4)
  #     df$n_stories <- (df$height - 5) %/% 10
  #     df[!is.na(df$ZONING) & df$ZONING == fourplex & is.na(df$n_stories), 'n_stories'] <- 4
  #     df[!is.na(df$ZONING) & df$ZONING == decontrol & is.na(df$n_stories), 'n_stories'] <- 4
  #     to_plot <- filter(df, !is.na(df$ZONING) & !is.na(expected_units) & expected_units > 0)
  #     simulate_buildout(to_plot)
  #   }
  # })
  
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
    df[!is.na(df$ZONING) & df$ZONING %in% c(fourplex, sixplex) & is.na(df$n_stories), 'n_stories'] <- 4
    df[!is.na(df$ZONING) & df$ZONING == decontrol & is.na(df$n_stories), 'n_stories'] <- 4
    
    
    # Render
    start <- Sys.time()
    
    if (input$map == 'heights') {
      print('plot height')
      to_plot <- filter(df, !is.na(ZONING) & !is.na(expected_units) & expected_units > 0)
      apns <- unique(to_plot$mapblklot)
      print(paste0('Group by took: ', round(Sys.time() - start, 1)))
      start <- Sys.time() 
      new_map <- do.call(set_filter,
                         list(map = mapboxer_proxy("mainPlot"),
                              layer_id = LAYER_ID,
                              filter = c(list('in', 'mapblklot'), apns)))
      # Find most common color and make it the default
      print(paste('nrow to plot', nrow(to_plot)))
      default_color <- names(which.max(table(pal(to_plot$n_stories))))
      to_color <- to_plot[(pal(to_plot$n_stories) != default_color) & !duplicated(to_plot$mapblklot), c('mapblklot', 'n_stories')]
      print(paste('nrow_tocolor', nrow(to_color)))
      
      apn_to_color <- unlist(unname(mapply(function(apn, color) c(apn, color), 
                                           to_color$mapblklot, 
                                           pal(to_color$n_stories), 
                                           SIMPLIFY = F)))
      
      if (is.null(apn_to_color)) {
        arg_list <- list(new_map,
                         layer_id=LAYER_ID, 
                         property='fill_color', 
                         value=default_color)
      } else {
        arg_list <- list(new_map,
                         layer_id=LAYER_ID, 
                         property='fill_color', 
                         value=c(list('match', 
                                      list('get', 'mapblklot')), 
                                 unlist(unname(apn_to_color)), 
                                 default_color))
      }
      do.call(set_paint_property, arg_list) %>%
        update_mapboxer()
      # mapboxer_proxy("map") %>%
      #   set_paint_property(layer_id = LAYER_ID,
      #                      property = 'fill_color',
      # value = list('match',
      #              list('get', 'mapblklot'),
      #              '4304002', 'red',
      #              '7284001', 'green',
      #              'black')) %>%
      
      print(paste0('Render took: ', round(Sys.time() - start, 1)))
      
      
    } 
    else if (input$map == "potential") {
      sf_use_s2(T)
      to_plot <- df
      to_plot$missing_potential <- to_plot$expected_units_skyscraper - to_plot$expected_units
      to_plot$missing_potential <- pmax(to_plot$missing_potential, 0, na.rm=T) / to_plot$ACRES
      to_plot$missing_potential <- log10(1 + to_plot$missing_potential)
      
      
      # Example usage
      pal_pot <- customColorNumeric(
        domain = c(0, 5)
      )
      
      
      apns <- unique(to_plot$mapblklot)
      new_map <- do.call(set_filter,
                         list(map = mapboxer_proxy("mainPlot"),
                              layer_id = LAYER_ID,
                              filter = c(list('in', 'mapblklot'), apns)))
      to_plot <- to_plot[!duplicated(to_plot$mapblklot) & (to_plot$missing_potential >= 1),]
      
      apn_to_color <- unlist(unname(mapply(function(apn, color) c(apn, color), 
                                           to_plot$mapblklot, 
                                           pal_pot(to_plot$missing_potential), 
                                           SIMPLIFY = F)))
      
      arg_list <- list(new_map,
                       layer_id=LAYER_ID, 
                       property='fill_color', 
                       value=c(list('match', 
                                    list('get', 'mapblklot')), 
                               unlist(unname(apn_to_color)), 
                               pal_pot(0)))
      do.call(set_paint_property, arg_list) %>%
        update_mapboxer()
      
      print('done')
    } 
    else if (input$map == 'E[u]') {
      to_plot <- filter(df, !is.na(ZONING) & !is.na(net_units) & net_units > 0)
      sf_use_s2(T)
      to_plot$net_units <- log10(1 + (to_plot$net_units/to_plot$ACRES))
      pal_eu <- customColorNumeric(
        domain = c(0, 4), n=3
      )
      apns <- unique(to_plot$mapblklot)
      new_map <- do.call(set_filter,
                         list(map = mapboxer_proxy("mainPlot"),
                              layer_id = LAYER_ID,
                              filter = c(list('in', 'mapblklot'), apns)))
      to_plot <- to_plot[!duplicated(to_plot$mapblklot) & (to_plot$net_units >= 1),]
      
      apn_to_color <- unlist(unname(mapply(function(apn, color) c(apn, color), 
                                           to_plot$mapblklot, 
                                           pal_eu(to_plot$net_units), 
                                           SIMPLIFY = F)))
      
      arg_list <- list(new_map,
                       layer_id=LAYER_ID, 
                       property='fill_color', 
                       value=c(list('match', 
                                    list('get', 'mapblklot')), 
                               unlist(unname(apn_to_color)), 
                               pal_eu(0)))
      do.call(set_paint_property, arg_list) %>%
        update_mapboxer()
      
    } else if (input$map == 'sim') {
      to_plot <- filter(df, !is.na(expected_units) & expected_units > 0)
      simulate_buildout(to_plot)
    }
  })
  
  
  # output$pieChart <- renderPlotly({
  #   added_capacity <- round(calculate_shortfall(df = updatedData()))
  #   print(added_capacity)
  #   
  #   pie_data <- data.frame(
  #     category = c("Added Capacity", "Remaining Shortfall"),
  #     value = c(added_capacity, 36282 - added_capacity)
  #   )
  # 
  #   plot_ly(pie_data, labels = ~category, values = ~value, type = 'pie',
  #           marker = list(colors = c('green', 'grey')),
  #           textinfo = 'hilvalue',
  #           insidetextorientation = 'radial') %>%
  #     layout(showlegend = F)
  # })
  # 
  output$dynamic_delete_button <- renderUI({
    if(requirements$count > 0) {
      actionButton("delete_requirement", "Remove last")
      
    }
  }
  )
  
  output$customHtmlJs <- renderUI({
    shortfallValue <- round(calculate_shortfall(df = updatedData()))
    
    htmlOutput <- HTML(paste0(
      '<div id="container"></div>
      <script>
      var bar = new ProgressBar.SemiCircle(container, {
        strokeWidth: 6,
        trailColor: "#eee",
        trailWidth: 1,
        easing: "easeInOut",
        duration: 2000,
        svgStyle: {width: "100%", height: "150", transform: "translate(0%, 0%)",},
        text: { 
              position: "absolute",
              left: "50%",
              top: "75%",
              padding: "0",
              margin: "0",
              transform: "translate(-100%, -100%)",
        },
        from: {color: "#FFEA82"},
        to: {color: "#ED6A5A"},
              
        step: (state, bar) => {

          var value = Math.round(bar.value() * 100);
          if (value > 100) {
            bar.path.setAttribute("stroke", "#ED6A5A");
            
          }
          else {
            bar.path.setAttribute("stroke", state.color);
          }
          if (value == 0) {
            bar.setText("Can you build enough homes?");
          } else if  (value >= 100) {
            bar.setText("You built enough homes!");
          } else {
            bar.setText(value + "% of what\'s needed");
          }
        }
      });
      bar.text.style.fontFamily = \'"Raleway", Helvetica, sans-serif\';
      bar.text.style.fontSize = "2rem";
      bar.animate(', if((shortfallValue / 36282) > 1) 2*((shortfallValue / 36282) %/% 1) - 1 else (shortfallValue / 36282), '); // Reactive value from server
      </script>'
    ))
    print(paste('bar.animate',  shortfallValue / 36282))
    
    print(paste('bar.set',  ((shortfallValue / 36282) %/% 1) ))
    htmlOutput
  })
  
  output$helpText <- renderUI({
    added_capacity <- round(calculate_shortfall(df = updatedData()))
    if (added_capacity == 0) {
      return(NULL)
    }
    print(added_capacity)
    updated_help <- paste0(
      "<br>In ", input$years_slider," years, this ", ifelse((added_capacity > 36282), 'amazing ', ''), "rezoning proposal helped build ",
      formatC(added_capacity, format="f", big.mark=",", digits=0),
      " "
    )
    
    congrats <- paste0(
      "homes, exceeding the legal minimum target by ", 
      formatC(added_capacity - 36282, format="f", big.mark=",", digits=0),
      " homes. Academic research shows this will reduce local rents and displacement. Go you!"
    )
    
    sad <- paste0(
      "of 36,282 homes required by the city's housing plan, leaving the city ",
      
      formatC(36282 - added_capacity, format="f", big.mark=",", digits=0),
      " homes short."
    )
    
    if (length(user_rezoning$lists) == 0) {
      sad <- paste0(sad, ' Why not try adding your own rezoning plan to get the city on track?')
    } else {
      sad <- paste0(sad, " That's a lot of people who'd like to live here but can't.")
    }
    
    if (added_capacity > 36282) {
      result <-  tags$div(HTML(updated_help, congrats), style="font-size: 1.15em;")
      shinyjs::runjs("sprayConfetti();") # Make sure this JS function is defined
    } else {
      result <-  tags$div(HTML(updated_help, sad), style="font-size: 1.15em;")
    }
    
    return(result)
  })
  
  requirements <- reactiveValues(count = 0, ids = list())
  
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
      select(mapblklot, ZONING, ACRES, pdev, net_units, expected_units, Envelope_1000, Upzone_Ratio)
    
    yields <- paste0('Lot ', format(values$mapblklot),
                     " yields ", round(values$net_units),
                     ' units with P(dev) = ', round(100*values$pdev, 1), '% ',
                     'with ', values$ZONING, ' and ', round(values$ACRES, 1),
                     ' acres and ', round(values$Envelope_1000), 
                     ' envelope and upzone ratio ', round(values$Upzone_Ratio), '\n', collapse='')
    net_units_by_zone <- st_drop_geometry(updatedData()) %>% group_by(ZONING) %>% summarise(round(sum(net_units)))
    return(paste0(paste0('Top Lots\n', yields), paste("Zoning Summary\n", paste(net_units_by_zone$ZONING, net_units_by_zone$`round(sum(net_units))`, collapse = "\n"))))
  })
  
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
  
  output$dynamic_sort1 <- renderUI({
    rez_list <- user_rezoning$lists
    items <- sapply(rez_list, function(item) {
      print(item$new_expr)
      english <- convert_logical_expression_to_english(item$new_expr)
      if (!is.null(english) && english != '') {
        return(paste(c(as_n_stories(item$new_height_description), 
                       english),
                     collapse=", "))
      } else {
        return(paste0(as_n_stories(item$new_height_description), ' everywhere.'))
      }
      
    }
    )
    
    
    if (length(items) != length(user_rezoning$lists)) {
      print('hi!')
    }
    
    rank_list(NULL, items, 'sortable', input_id='sortable')
  })
  
  observeEvent(c(input$item_deleted, input$sortable), {
    # Code to execute when an item is added
    if (!is.null(input$item_deleted)) {
      # Go through user_rezoning$lists and find whats deletted
      print("An item was not added!")
    }
    
    # For deleting
    for (elem in names(user_rezoning$lists)) {
      if (! elem %in% input$sortable) {
        print('removing')
        print(names(user_rezoning$lists))
        user_rezoning$lists <- user_rezoning$lists[-which(names(user_rezoning$lists) == elem)]
        print(names(user_rezoning$lists))
      }
    }
    
    # For reordering
    if (length(user_rezoning$lists) == length(input$sortable)){
      print('sorting')
      user_rezoning$lists <- user_rezoning$lists[input$sortable]
      print(names(user_rezoning$lists))
    }
  })
  
  output$dynamicLegend <- renderUI({
    if (input$map == "heights") {
      layers <- c(#'unchanged',
                  '65 feet (6 stories)',
                  '85 feet(8 stories)',
                  '140 feet (14 stories)',
                  '240 feet (24 stories)',
                  '300 feet (30 stories)'
      )
      colors <- c(#"#D2EBEB", 
                  "#F4AD3E", 
                  "#EC7520", 
                  "#E44520", 
                  "#AE2922", 
                  "#6C2F18"
                  )
      if (input$scenario %in% c('A', 'B', 'C', 'D', 'E')) {
        legend_name <- 'Zoning + Bonus'
      } else {
        legend_name <- 'Base Zoning'
      }
    } else if (input$map == "potential") { # Potential
      # Default or other conditions
      legend_name <- 'Potential'
      layers <- c("<10 du/ac", "10-100 du/ac", "100-1000 du/ac", '1000+ du/ac')
      colors <- c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C") # Replace with appropriate color codes
    } else if (input$map == "E[u]") { # Expect units added per acre
      legend_name <- 'Expected du/ac added'
      layers <- c("<10 du/ac", "10-100 du/ac", "100+ du/ac")
      colors <- c("#EFF3FF", "#6BAED6", "#08519C") # Replace with appropriate color codes      
    } else if (input$map == 'sim') {
      assertthisshouldfail
    }
    
    
    
    # Generate the HTML and JavaScript for the legend
    HTML(paste0(
      "<div class='map-overlay' id='legend'>Base Zoning</div>
       <script>
         document.getElementById('legend').innerHTML = '", legend_name, "'; // Clear existing legend
         var layers = ", jsonlite::toJSON(layers, auto_unbox = TRUE), ";
         var colors = ", jsonlite::toJSON(colors, auto_unbox = TRUE), ";
         var legend = document.getElementById('legend');
         layers.forEach((layer, i) => {
           const color = colors[i];
           const item = document.createElement('div');
           const key = document.createElement('span');
           key.className = 'legend-key';
           key.style.backgroundColor = color;
           const value = document.createElement('span');
           value.innerHTML = `${layer}`;
           item.appendChild(key);
           item.appendChild(value);
           legend.appendChild(item);
         });
       </script>"
    ))
  })
  
  shortfallValue <- reactive({
    round(calculate_shortfall(df = updatedData()))
  })
  
  create_mailto_link <- function(subject, body, to, cc=NULL) {
    # Encoding special characters
    subject_enc <- URLencode(subject)
    body_enc <- URLencode(body)
    to_enc <- URLencode(to)
    
    # Creating mailto link
    mailto <- paste0("mailto:", to_enc, "?subject=", subject_enc, "&body=", body_enc)
    
    # Adding BCC if provided
    if (!is.null(cc) && nchar(cc) > 0) {
      bcc_enc <- URLencode(cc)
      mailto <- paste0(mailto, "&bcc=", bcc_enc)
    }
    cc <- paste(c("rich.hillis@sfgov.org", "joshua.switzky@sfgov.org", "rachael.tanner@sfgov.org",
                  "lisa.chen@sfgov.org", "sue.diamond@sfgov.org", "kathrin.moore@sfgov.org", 
                  "derek.braun@sfgov.org", "joel.koppel@sfgov.org", "board.of.supervisors@sfgov.org", "theresa.imperial@sfgov.org"), collapse = ",")
    cc_enc <- URLencode(cc)
    mailto <- paste0(mailto, "&cc=", cc_enc)
    
    return(a('Email', href=mailto))
  }
  
  output$dynamicEmailButton1 <- renderUI({
    if(shortfallValue() >= 36282) {
      return(
        tagList(
          h4("Make it law")

       )
      )
    }
  })
  output$dynamicEmailButton2 <- renderUI({
    if(shortfallValue() >= 36282) {
      return(
        tagList(
          sendEmailMod(cc=input$subscribe),
          HTML('SF Planning to consider this rezoning.'),
        )
      )
    }
  })
  output$dynamicEmailButton3 <- renderUI({
    if(shortfallValue() >= 36282) {
      return(
        tagList(
          checkboxInput("subscribe", "And recieve email updates from SF YIMBY after writing in!", value = TRUE) # Adds an opt-out checkbox
        )
      )
    }
  })
  
  sendEmailMod <- function(to_email = "sf.housing.choice@sfgov.org",
                           cc=NULL){
    
    scenario_full_name <- NULL
    if (input$scenario %in% c('yimby3', 'E', 'D')) {
      if (input$scenario == 'yimby3') {
        scenario_full_name <- " the People's Plan rezoning"
      } else if (input$scenario == 'E') {
        scenario_full_name <- " SF Planning's current rezoning proposal"
      } else if (input$scenario == 'D') {
        scenario_full_name <- " SF Planning's Fall rezoning proposal"
      }
    }
    
    plain_english_user_rezonings <- sapply(user_rezoning$lists, function(item) {
      english <- convert_logical_expression_to_english(item$new_expr)
      if (!is.null(english) && english != '') {
        return(paste(c(as_n_stories(item$new_height_description), 
                       english),
                     collapse=", "))
      } else {
        return(paste0(as_n_stories(item$new_height_description), ' everywhere.'))
    }})
    append_to_scenario <- NULL
    if (input$scenario %in% c('yimby3', 'E', 'D') && length(user_rezoning$lists) > 0) {
      append_to_scenario <- paste(" and adding to it the following:", plain_english_user_rezonings)
    }
    else if (length(user_rezoning$lists) > 0) {
      append_to_scenario <- paste('-', paste(plain_english_user_rezonings, sep='\n'))
    }

    if(!is.null(append_to_scenario)) {
      output <- paste0(':', paste('\n', append_to_scenario, collapse = ''))
    } else {
      output <- '.'
    }
    
    text = paste(
      'Dear SF Planning,',
      paste0(paste0('For the Expanding Housing Opportunity rezoning, I support', scenario_full_name),
             output),
      paste0('This would ensure the city met its legal housing obligations and zoned for ', shortfallValue(), ' units of housing, which would bring down the rent in the city and ease our housing shortage.'),
      paste('Thank you,', sep='\n'),
      sep='\n\n'
    )

    if (!is.null(cc) && nchar(cc) > 0 && cc != FALSE){
      cc <- 'hi@sfyimby.org'
    } else {
      cc <- NULL
    }
    create_mailto_link(subject='Expanding Housing Opportunity',
                       body=text,
                       to=to_email,
                       cc=cc)
    }
  
  observe({
    # Validate 'stories' input
    if(!is.na(input$stories ) && input$stories > 25) {
      # Reset to default value or provide feedback to the user
      updateNumericInput(session, "stories", value = 25)
    }
    if(!is.na(input$stories) && input$stories < 4) {
      # Reset to default value or provide feedback to the user
      updateNumericInput(session, "stories", value = 4)
    }
  })
  
  observe({
    # Validate 'stories' input
    if(input$years_slider > 10) {
      # Reset to default value or provide feedback to the user
      updateNumericInput(session, "years_slider", value = 10)
    }
    if(input$years_slider < 5) {
      # Reset to default value or provide feedback to the user
      updateNumericInput(session, "years_slider", value = 5)
    }
  })
  
  observeEvent(input$rezone, {
    if (is.na(input$stories)) {
      return(NULL)
    }
    new_height <- 5 + 10*input$stories
    new_height_description <- paste0(new_height, "' Height Allowed")
    new_expr <- paste0('TRUE & !((ex_height2024 >= ', new_height, ') & sb330_applies)')
    for (prefix in requirements$ids) {
      to_add <- NULL
      parcel_filter <- input[[paste0(prefix, '-parcel_filter')]]
      if (parcel_filter == 'PEG') {
        to_add <- 'peg'
      }
      else if (parcel_filter == 'Already Rezoned') {
        to_add <- '!is.na(ZONING)'
      }
      else if (parcel_filter == 'Corner Lot') {
        to_add <- 'is_corner'
      }
      else if (parcel_filter == 'Economic Opportunity') {
        econ_threshold <- input[[paste0(prefix, '-economic_score')]]
        to_add <- paste0('(econ_affh >= ', as.numeric(econ_threshold) / 100, ')')
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
        to_add <- paste0('(affh2023 %in% ', in_expr, ')')
      }
      else if (parcel_filter == 'Transit') {
        distance <- as.numeric(input[[paste0(prefix, '-distance')]])
        relevant_transit <- input[[paste0(prefix, '-transit_options')]]
        conditions <- c()
        if('Caltrain Stops' %in% relevant_transit) {
          conditions <- c(conditions, paste0('(transit_dist_caltrain <= ', distance, ')'))
        }
        
        if('BART Stops' %in% relevant_transit) {
          conditions <- c(conditions, paste0('(transit_dist_bart <= ', distance, ')'))
        }
        
        if('Muni Rapid Network' %in% relevant_transit) {
          conditions <- c(conditions, paste0('(transit_dist_rapid_stops <= ', distance, ')'))
        }
        if('All Muni Lines' %in% relevant_transit) {
          conditions <- c(conditions, paste0('(transit_dist <= ', distance, ')'))
        }
        to_add <- paste0('(', paste(conditions, collapse = ' | '), ')')
      }
      else if (parcel_filter == 'Rapid Bus Line') {
        distance <- input[[paste0(prefix, '-distance')]]
        to_add <- paste0('(transit_dist_rapid <= ', distance, ')')
      }
      else if (parcel_filter == 'Commercial Corridor') {
        distance <- input[[paste0(prefix, '-distance')]]
        to_add <- paste0('(commercial_dist <= ', as.numeric(distance), ')')
      }
      else if (parcel_filter == 'Parks') {
        distance <- input[[paste0(prefix, '-distance')]]
        to_add <- paste0('(park_dist <= ', as.numeric(distance), ')')
      }
      else if (parcel_filter == 'Colleges') {
        distance <- input[[paste0(prefix, '-distance')]]
        to_add <- paste0('(college_dist <= ', as.numeric(distance), ')')
      }
      else if (parcel_filter == 'Lot Size') {
        sqft_lot_size <- input[[paste0(prefix, '-lot_size')]]
        to_add <- paste0('(ACRES >= ', as.numeric(sqft_lot_size) / 43560, ')')
      }
      else if (parcel_filter == 'Neighborhood') {
        nhood <- input[[paste0(prefix, '-hood')]]
        to_add <- paste0('(nhood == "', nhood, '")')
        to_add <- paste0('(', paste0(to_add, collapse=' | '), ')')
      }
      if (input[[paste0(prefix, '-is_in')]] == FALSE) {
        to_add <- paste0('(!', to_add, ')')
      }
      new_expr <- paste(new_expr, to_add, sep=' & ')
    }
    
    # Find name for list
    k <- 0
    while(paste0('list', k) %in% names(user_rezoning[['lists']])) {
      k <- k + 1
    }
    user_rezoning[['lists']][[paste0('list', k)]] <- list(new_height_description = new_height_description, 
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
