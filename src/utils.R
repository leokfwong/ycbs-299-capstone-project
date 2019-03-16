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


compareModelsAnova <- function(vars, outcome) {
  
  models_lst <- list()
  fmla <- paste(outcome, "~", paste(vars, collapse = "+"))
  model <- glm(fmla, data = df_train, family = quasipoisson)
  models_lst <- c(models_lst, list(model))
  
  # Compare models
  for (i in 1:length(vars)) {
    
    tmp_vars = vars[!vars %in% vars[i]]
    fmla <- paste(outcome, "~", paste(tmp_vars, collapse = "+"))
    print(fmla)
    model <- glm(fmla, data = df_train, family = quasipoisson)
    pseudoR2 <- 1 - model$deviance/model$null.deviance
    print(pseudoR2)
    models_lst <- c(models_lst, list(model))
    
    df_test_po <- df_test
    df_test_po$pred <- predict(model, newdata = df_test_po, type = "response")
    print(df_test_po %>% mutate(residual = pred - calls_count) %>% summarize(rmse = sqrt(mean(residual^2))))
    
  }
  
  for (j in 2:length(models_lst)) {
    
    print(anova(models_lst[j][[1]], models_lst[1][[1]], test = "LRT"))
    
  }
  
}