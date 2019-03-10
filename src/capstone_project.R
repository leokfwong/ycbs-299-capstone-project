####################################################################################################
######################                                                        ######################
######################                      CAPSTONE PROJECT                  ######################
######################                                                        ######################
####################################################################################################

# Set working directory
setwd("~/GitHub/YCBS299/capstone_project")
source("src/utils.R")

###################
### IMPORT DATA ###
###################

# There are 4829 unique days, ranging from 2005-01-01 to 2018-03-22

# Import cleaned data
calls_raw <- read.csv("data/prepdata.csv",
                      header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

####################
### PREPARE DATA ###
####################

# Assign calls_raw to df_clean
df_clean <- calls_raw

# Rename variables
names(df_clean)[names(df_clean) %in% c("Date.Time", "Mean.Temp...C.", "Total.Rain..mm.", "Total.Snow..cm.")] <- c("date_time", "temp_mean", "rain_total", "snow_total")

# Derive variables of interest (potential features)
df_clean <- within(df_clean, {
  
  season <- getSeason(as.Date(date, "%Y-%m-%d"))
  day_of_week <- gsub("0", "Sunday", day_of_week)
  day_of_week <- gsub("1", "Monday", day_of_week)
  day_of_week <- gsub("2", "Tuesday", day_of_week)
  day_of_week <- gsub("3", "Wednesday", day_of_week)
  day_of_week <- gsub("4", "Thursday", day_of_week)
  day_of_week <- gsub("5", "Friday", day_of_week)
  day_of_week <- gsub("6", "Saturday", day_of_week)
  is_holiday <- as.logical(is_holiday)
  # temp_mean <- as.numeric(scale(temp_mean))
  # rain_total <- as.numeric(scale(rain_total))
  # snow_total <- as.numeric(scale(snow_total))
  
})

#########################
### DATA MANIPULATION ###
#########################

# Aggregate data so that every row represent a day with the sum of calls.
# Note the the additional variables are features that a specific to an overall day.
# (ie. cannot use neighborhood, since within a day there could be many).

df_callsperday <- aggregate(data = df_clean, nom_ville ~ year + month + day, FUN = length)

df_callsperday <- mergePredictors(df = df_callsperday,
                                  df_raw = "df_clean",
                                  vars = c("temp_mean", "rain_total", "snow_total"), 
                                  merge_by = c("year", "month", "day"))

names(df_callsperday)[names(df_callsperday) == "nom_ville"] <- "calls_count"

df_callsperday <- within(df_callsperday, {
  
  calls_count <- round(calls_count)
  month <- as.character(month)
  
})

df_callsperday <- subset(df_callsperday, !is.na(temp_mean) & !is.na(rain_total) & !is.na(snow_total))

# Define training dataset as any data points between 2010 and 2017. 
# The rationale is that there was a spike in number of calls in 2008 when the roles
# of fire fighters expanded to first response.
df_train <- subset(df_callsperday, year > 2009 & year < 2018)
# Define testing dataset as any data points after 2018.
df_test <- subset(df_callsperday, year == 2018)

# 
# df_train_mean <- aggregate(data = df_train, calls_count ~ month + day, FUN = mean)
# 
# df_train_mean <- mergePredictors(df = df_train_mean, 
#                                  df_raw = "df_train",
#                                  vars = c("temp_mean", "rain_total", "snow_total"), 
#                                  merge_by = c("month", "day"))
# 
# df_train_mean$calls_count <- round(df_train_mean$calls_count)

################
### MODELING ###
################

outcome = "calls_count"
# vars = c("day_of_week", "is_holiday", "temp_mean", "rain_total", "snow_total", "month")
vars = c("month", "temp_mean", "rain_total", "snow_total")

# Compute the mean and variance to see if there is a significant difference
mean(df_train$calls_count)
var(df_train$calls_count)
# mean(df_train_mean$calls_count)
# var(df_train_mean$calls_count)

# Define the Y and x
fmla <- paste(outcome, "~", paste(vars, collapse = " + "))

# Run model (Note we use quasipoisson since var >>> mean)
po_model <- glm(fmla, data = df_train, family = quasipoisson)
# po_model_mean <- glm(fmla, data = df_train_mean, family = poisson)

pseudoR2 <- 1 - po_model$deviance/po_model$null.deviance
pseudoR2
# pseudoR2_mean <- 1 - po_model_mean$deviance/po_model_mean$null.deviance
# pseudoR2_mean

# Test prediction
df_test_po <- df_test
df_test_po_mean <- df_test
df_test_po$pred <- predict(po_model, newdata = df_test_po, type = "response")
df_test_po %>% mutate(residual = pred - calls_count) %>% summarize(rmse = sqrt(mean(residual^2)))
# df_test_po_mean$pred <- predict(po_model_mean, newdata = df_test_po, type = "response")
# df_test_po_mean %>% mutate(residual = pred - calls_count) %>% summarize(rmse = sqrt(mean(residual^2)))

# NEXT STEPS:
# 1) Compare models using different features (AIC).
# 2) Split data into train and test and minimize MSE.
# 3) Try different models (ie. Random Forest).

# Gradient boosting

library(ranger)
library(dplyr)
df_md_rf <- df_test
rf_model <- ranger(fmla, df_train, num.trees = 500, respect.unordered.factors = "order")
df_md_rf$pred <- predict(rf_model, df_test)$predictions

df_md_rf %>% mutate(residual = pred - calls_count) %>% summarize(rmse = sqrt(mean(residual^2)))


library(vtreat)
df_md_gb <- df_train
treatplan <- designTreatmentsZ(df_md_gb, vars)
newvars <- treatplan$scoreFrame %>% filter(code %in% c("clean", "lev")) %>% magrittr::use_series(varName)

df_md_gb.treat <- prepare(treatplan, df_md_gb, varRestriction = newvars)

library(xgboost)
cv <- xgb.cv(data = as.matrix(df_md_gb.treat),
             label = df_md_gb$calls_count,
             objectives = "reg:linear", 
             nrounds = 100, nfold = 5, eta = 0.3, depth = 6)

elog <- as.data.frame(cv$evaluation_log)
nrounds <- which.min(elog$test_rmse_mean)
nrounds


df_md_gb <- df_test
treatplan <- designTreatmentsZ(df_md_gb, vars)
newvars <- treatplan$scoreFrame %>% filter(code %in% c("clean", "lev")) %>% magrittr::use_series(varName)

df_md_gb.treat <- prepare(treatplan, df_md_gb, varRestriction = newvars)

gb_model <- xgboost(data = as.matrix(df_md_gb.treat),
                    label = df_md_gb$calls_count,
                    nrounds = nrounds,
                    objectives = "reg:linear",
                    eta = 0.3, 
                    depth = 6)

df_md_gb.treat$pred <- predict(gb_model, as.matrix(df_md_gb.treat))
gb_pred <- df_md_gb.treat$pred

df_md_gb <- cbind(df_md_gb, gb_pred)

df_md_gb %>% mutate(residual = gb_pred - calls_count) %>% summarize(rmse = sqrt(mean(residual^2)))
