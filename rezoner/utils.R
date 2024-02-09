convert_logical_expression_to_english <- function(expression) {
  # Remove the common prefix
  expression <- gsub("TRUE & !\\(\\(ex_height2024 > (\\d+))\\) & sb330_applies\\) & ", "", expression)
  # Correctly split the expression into parts, considering OR conditions
  parts <- unlist(strsplit(expression, " & | \\| ", perl=TRUE))
  english_parts <- c()
  for (part in parts) {
    negated <- grepl("^!\\(", part) | grepl("^\\(!\\(", part)
    part <- gsub("^\\(!", "", part) # Remove the negation operator for further processing

    part <- gsub("^!\\(", "", part) # Remove the negation operator for further processing
    part <- gsub("\\)$", "", part) # Remove any closing parenthesis that might be left after removing negation

    if(grepl("transit_dist", part)) {
      distance <- gsub(".*< (0\\.\\d+)", "\\1", part)
      if (grepl('(\\(|\\))',distance)) {
        distance <- gsub(".*< ((0\\.\\d+)|0|1)\\)", "\\1", part)
      }

      transit_type <- gsub("^\\(*transit_dist_([^ ]*) <.*", "\\1", part)
      transit_name <- switch(transit_type,
                             bart = "BART",
                             caltrain = "Caltrain",
                             rapid = "MUNI's rapid transit network",
                             'any MUNI line') # Default case
      distance_phrase <- if (negated) "not within" else "within"
      english_parts <- c(english_parts, paste(distance_phrase, distance, "miles of", transit_name))
    } else if(grepl("nhood == ", part)) {
      neighborhood <- gsub("\\(nhood == \"(.*)\"", "\\1", part)
      english_parts <- c(english_parts, paste("in the", neighborhood, "neighborhood"))
    } else if(part == "!peg" || negated && grepl("peg", part)) {
      english_parts <- c(english_parts, "not in a PEG")
    } else if(part == "peg") {
      english_parts <- c(english_parts, "in a PEG")
    } else if(grepl("ACRES > ", part)) {
      acres_threshold <- gsub("\\(ACRES > ([0-9.]+)\\)", "\\1", part)
      acres_threshold <- as.character(signif(as.numeric(acres_threshold), 2))
      acres_phrase <- if (negated) "not on lots over" else "on lots over"
      english_parts <- c(english_parts, paste(acres_phrase, acres_threshold, "acres"))
    }
  }

  return(paste(english_parts, collapse = "; "))
}

# # Apply parsing to each expression and handle potential NULLs gracefully
 expressions <- c(
   # "TRUE & !((ex_height2024 > 105) & sb330_applies) & (transit_dist_bart < 0.25)",
   # "TRUE & !((ex_height2024 > 105) & sb330_applies) & (!peg) & (nhood == \"Mission\") & (transit_dist_caltrain < 0.25)",
   # "TRUE & !((ex_height2024 > 105) & sb330_applies) & (peg) & (nhood == \"Outer Mission\") & (transit_dist_rapid < 0.2)",
   # "TRUE & !((ex_height2024 > 105) & sb330_applies) & (transit_dist_caltrain < 0.35) | (transit_dist_rapid < 0.35) & (commercial_dist < 0.12)",
   # "TRUE & !((ex_height2024 > 85) & sb330_applies) & ((transit_dist_caltrain < 0.25) | (transit_dist_bart < 0.25) | (transit_dist_rapid < 0.25))",
   # "TRUE & !((ex_height2024 > 105) & sb330_applies) & (!(transit_dist_bart < 1))",
   "TRUE & !((ex_height2024 > 45) & sb330_applies) & (transit_dist < 0.04)",
   "TRUE & !((ex_height2024 > 105) & sb330_applies) & (!(ACRES > 1.14630394857668))"

 )
#
 results <- sapply(expressions, convert_logical_expression_to_english)
 print(results)
