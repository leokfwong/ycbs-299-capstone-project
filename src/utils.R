####################################################################################################
######################                                                        ######################
######################                  CAPSTONE PROJECT - UTILS              ######################
######################                                                        ######################
####################################################################################################

#######################
### UTILS FUNCTIONS ###
#######################

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format = "2012-%m-%d"))
  
  ifelse(d >= WS | d < SE, "Winter",
         ifelse(d >= SE & d < SS, "Spring",
                ifelse(d >= SS & d < FE, "Summer", "Fall")))
}


mergePredictors <- function(df, df_raw, vars, merge_by) {
  
  # Initialize empty list
  df_lst <- list()
  
  # Iterate through predictors and create command to aggregate
  for (var in vars) {
    
    # Create aggregate function to evaluate
    cmd <- paste0("aggregate(data = ", df_raw, ", ", var, " ~ ", paste(merge_by, collapse = " + ") ,", FUN = mean)")
    
    # Aggregate data
    tmp_df <- eval(parse(text = cmd))
    
    # Store dataframe to list
    df_lst <- c(list(tmp_df), df_lst)
    
  }
  
  # Merge all dataframes together
  df <- Reduce(
    function(x, y) {
      merge(
        x = x,
        y = y,
        by = merge_by,
        all.x = TRUE
      )
    },
    c(list(df), df_lst)
  )
  
  # Return merged dataframe
  return (df)
  
}