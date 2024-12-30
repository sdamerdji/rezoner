
customColorNumeric <- function(domain, n=5) {
  
  colorFunc <- colorRampPalette(brewer.pal(n, 'Blues'))
  
  # Generate a continuous palette
  continuousPalette <- colorFunc(n)
  
  # Map the domain to the 1:n
  scaleFunc <- function(x) {
    scaled <- (x - min(domain)) / (max(domain) - min(domain))
    indices <- round(scaled * (n-1)) + 1
    indices <- pmin(pmax(indices, 1), n) # Ensure indices stay within bounds
    continuousPalette[indices]
  }
  
  return(scaleFunc)
}

convert_logical_expression_to_english <- function(expression) {
  # Remove the common prefix
  expression <- gsub("TRUE & !\\(\\(ex_height2024 >= (\\d+))\\) & sb330_applies\\) & ", "", expression)
  # Correctly split the expression into parts, considering OR conditions
  parts <- unlist(strsplit(expression, " & | \\| ", perl=TRUE))
  english_parts <- c()
  for (part in parts) {
    negated <- grepl("^!\\(", part) | grepl("^\\(!\\(", part) | grepl("^\\(!", part)
    part <- gsub("^\\(!", "", part) # Remove the negation operator for further processing
    part <- gsub("^!\\(\\(", "", part) # Remove the negation operator for further processing
    part <- gsub("^[!\\(\\)]+", "", part)
    part <- gsub("^!\\(", "", part) # Remove the negation operator for further processing
    if (negated) {
      part <- gsub("\\)$", "", part) # Remove any closing parenthesis that might be left after removing negation
    }

    if(grepl("transit_dist", part)) {
      distance <- gsub(".*<= (0\\.\\d+)\\)*", "\\1", part)
      if (grepl('(\\(|\\))',distance)) {
        distance <- gsub(".*<= ((0\\.\\d+)|0|1)\\)*", "\\1", part)
      }

      transit_type <- gsub("^\\(*transit_dist_([^ ]*)\\)* <=.*", "\\1", part)
      transit_name <- switch(transit_type,
                             bart = "BART",
                             caltrain = "Caltrain",
                             rapid_stops = "MUNI's rapid transit network",
                             'any MUNI line') # Default case
      distance_phrase <- if (negated) "not within" else "within"
      english_parts <- c(english_parts, paste(distance_phrase, distance, "miles of", transit_name))
    } else if(grepl("commercial_dist", part)) {
      distance <- gsub(".*<= (0\\.\\d+)", "\\1", part)
      if (grepl('(\\(|\\))',distance)) {
        distance <- gsub(".*<= ((0\\.\\d+)|0|1)\\)", "\\1", part)
      }

      distance_phrase <- if (negated) "not within" else "within"
      english_parts <- c(english_parts, paste(distance_phrase, distance, "miles of a commercial corridor"))
    } else if(grepl("park_dist", part)) {
      distance <- gsub(".*<= (0\\.\\d+)", "\\1", part)
      if (grepl('(\\(|\\))',distance)) {
        distance <- gsub(".*<= ((0\\.\\d+)|0|1)\\)", "\\1", part)
      }
      
      distance_phrase <- if (negated) "not within" else "within"
      english_parts <- c(english_parts, paste(distance_phrase, distance, "miles of a 1+ acre park"))
    } else if(grepl("college_dist", part)) {
      distance <- gsub(".*<= (0\\.\\d+)", "\\1", part)
      if (grepl('(\\(|\\))',distance)) {
        distance <- gsub(".*<= ((0\\.\\d+)|0|1)\\)", "\\1", part)
      }
      
      distance_phrase <- if (negated) "not within" else "within"
      english_parts <- c(english_parts, paste(distance_phrase, distance, "miles of a college"))
    } 
    else if(grepl("econ_affh", part)) {
      econ_affh <- gsub(".*>= (0\\.\\d+)", "\\1", part)
      if (grepl('(\\(|\\))', econ_affh)) {
        econ_affh <- gsub(".*>= ((0\\.\\d+)|0|1)\\)", "\\1", part)
      }
      
      distance_phrase <- if (negated) "not over" else "over"
      english_parts <- c(english_parts, paste(distance_phrase, econ_affh, "economic score"))
    }
    else if(grepl("nhood == ", part)) {
      neighborhood <- gsub("nhood == \"(.*)\"\\)*", "\\1", part)
      english_parts <- c(english_parts,
                         if (negated) paste("not in the", neighborhood, "neighborhood") else paste("in the", neighborhood, "neighborhood") )
    } else if(part == "!peg" | (negated && grepl("peg", part))) {
      english_parts <- c(english_parts, "not in a PEG")
    } else if(grepl("peg", part)) {
      english_parts <- c(english_parts, "in a PEG")
    } else if(grepl("ACRES >= ", part)) {
      acres_threshold <- gsub("\\(*ACRES >= ([0-9.]+)\\)*", "\\1", part)
      acres_threshold <- as.character(signif(as.numeric(acres_threshold), 2))
      acres_phrase <- if (negated) "not on lots over" else "on lots over"
      english_parts <- c(english_parts, paste(acres_phrase, acres_threshold, "acres"))
    } else if(grepl('^affh2023', part)) {
      extracted <- gsub(".*?((Low|Moderate|High)).*", "\\1", part)
      english_parts <- c(english_parts, if (negated) paste('TCAC Map Score of less than', extracted) else paste('TCAC Map Score of at least', extracted))
    } else if(grepl('^\\(affh2023', part)) {
      extracted <- gsub(".*?((Low|Moderate|High)).*", "\\1", part)
      english_parts <- c(english_parts, if (negated) paste('TCAC Map Score of less than', extracted) else paste('TCAC Map Score of at least', extracted))
    } else if(grepl('^is.na\\(ZONING', part)) {
      english_parts <- c(english_parts, 'already rezoned')
    }else if(grepl('^!is.na\\(ZONING', part)) {
      english_parts <- c(english_parts, 'not already rezoned')
    }else if(grepl('^is_corner', part)) {
      english_parts <- c(english_parts, 'on corner lot')
    }else if(grepl('^!is_corner', part) | (negated && grepl("is_corner", part))) {
      english_parts <- c(english_parts, 'on non-corner lot')
    }
  }

  return(paste(english_parts, collapse = "; "))
}

as_n_stories <- function(zoning_name) {
  height <- as.numeric(str_extract(zoning_name, "\\d+"))
  n_stories <- (height - 5) %/% 10
  return(paste0(n_stories, ' stories'))
}

# # Apply parsing to each expression and handle potential NULLs gracefully
 expressions <- c(
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & ((transit_dist_bart <= 0.25))",
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & (!peg) & (nhood == \"Mission\") & (transit_dist_caltrain < 0.25)",
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & (peg) & (nhood == \"Outer Mission\") & (transit_dist_rapid <= 0.2)",
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & (transit_dist_caltrain <= 0.35) | (transit_dist_rapid <= 0.35) & (commercial_dist <= 0.12)",
   # "TRUE & !((ex_height2024 >= 85) & sb330_applies) & ((transit_dist_caltrain <= 0.25) | (transit_dist_bart <= 0.25) | (transit_dist_rapid <= 0.25))",
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & (!(transit_dist_bart <= 1))",
   # "TRUE & !((ex_height2024 >= 45) & sb330_applies) & (transit_dist <= 0.04)",
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & (!(ACRES >= 1.14630394857668))",
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & (ACRES >= 0.114784205693297)",
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & !is.na(ZONING) & (!is.na(affh2023) & affh2023 %in% c('High Resource', 'Highest Resource'))",
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & (!(affh2023 %in% c('Moderate Resource', 'High Resource', 'Highest Resource')))",
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & (!!is.na(ZONING))",
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & ((nhood == \"Bernal Heights\") | (nhood == \"Castro/Upper Market\"))",
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & (!((nhood == \"Bernal Heights\") | (nhood == \"Bayview Hunters Point\") | (nhood == \"Castro/Upper Market\") | (nhood == \"Chinatown\") | (nhood == \"Excelsior\") | (nhood == \"Financial District/South Beach\") | (nhood == \"Glen Park\") | (nhood == \"Golden Gate Park\") | (nhood == \"Haight Ashbury\")))",
   # "TRUE & !((ex_height2024 >= 105) & sb330_applies) & (econ_affh >= 0.88)",
   "TRUE & !((ex_height2024 >= 85) & sb330_applies) & (commercial_dist <= 0.05)",
   "TRUE & !((ex_height2024 >= 85) & sb330_applies) & ((transit_dist_bart <= 0.1))"

 )


