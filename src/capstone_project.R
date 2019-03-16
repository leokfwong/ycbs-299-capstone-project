############################################################################################
##################                                                        ##################
##################                      CAPSTONE PROJECT                  ##################
##################                                                        ##################
############################################################################################

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


#########################
### DATA MANIPULATION ###
#########################

# Aggregate data so that every row represent a day with the sum of calls.
# Note the the additional variables are features that are specific to a given day.
# (ie. cannot use neighborhood, since within a day there could be many).

# Aggreagate by unique day (year-month-day) and count number of calls
df_callsperday <- aggregate(data = df_clean, nom_ville ~ year + month + day, FUN = length)

# Merge predictors back to aggregated dataset (features of each day)
df_callsperday <- mergePredictors(df = df_callsperday,
                                  df_raw = "df_clean",
                                  vars = c("temp_mean", "did_rain", "did_snow", 
                                           "is_holiday", "day_of_week"),
                                  merge_by = c("year", "month", "day"))

# Rename variable
names(df_callsperday)[names(df_callsperday) == "nom_ville"] <- "calls_count"

# Derive and format predictors
df_callsperday <- within(df_callsperday, {
  
  month <- as.factor(month)
  did_rain <- as.factor(did_rain)
  did_snow <- as.factor(did_snow)
  is_holiday <- as.factor(is_holiday)
  day_of_week <- gsub("0", "Monday", day_of_week)
  day_of_week <- gsub("1", "Tuesday", day_of_week)
  day_of_week <- gsub("2", "Wednesday", day_of_week)
  day_of_week <- gsub("3", "Thursday", day_of_week)
  day_of_week <- gsub("4", "Friday", day_of_week)
  day_of_week <- gsub("5", "Saturday", day_of_week)
  day_of_week <- gsub("6", "Sunday", day_of_week)
  day_of_week <- as.factor(day_of_week)
  year <- as.numeric(year)
  is_pre <- ifelse(as.numeric(as.character(year)) <= 2009, 1, 0)
  year_is_pre <- year * is_pre
  
})

# Remove records where any of the following predictors are missing
df_callsperday <- subset(df_callsperday, !is.na(temp_mean) & !is.na(did_rain) & !is.na(did_snow) & !is.na(is_holiday))

# Define train and test sets (80/20 split)
set.seed(101)
# Percentage of split set to 80
bound <- floor((nrow(df_callsperday)/5)*4)
# Sample rows
df_callsperday <- df_callsperday[sample(nrow(df_callsperday)), ]
# Assign train and test data
df_train <- df_callsperday[1:bound, ]
df_test <- df_callsperday[(bound+1):nrow(df_callsperday), ]

################
### MODELING ###
################

library(dplyr)

outcome = "calls_count"
# vars = c("day_of_week", "is_holiday", "temp_mean", "rain_total", "snow_total", "month")
vars = c("year", "month", "temp_mean", "did_rain", "is_holiday", "day_of_week", "is_pre", "year_is_pre")

# Compute the mean and variance to see if there is a significant difference
mean(df_train$calls_count)
var(df_train$calls_count)

# Define the Y and x
fmla <- paste(outcome, "~", paste(vars, collapse = " + "))


# compareModelsAnova(vars, outcome)

##########################
### GLM - QUASIPOISSON ###
##########################

# Run model (Note we use quasipoisson since var >>> mean)
po_model <- glm(fmla, data = df_train, family = quasipoisson)

# Calculate pseudo R2 (the closer to 1, the better)
pseudoR2 <- 1 - po_model$deviance/po_model$null.deviance
pseudoR2

# Test prediction
df_test_po <- df_test
df_test_po$pred <- predict(po_model, newdata = df_test_po, type = "response")
df_test_po %>% mutate(residual = pred - calls_count) %>% summarize(rmse = sqrt(mean(residual^2)))

# TODO MAKE FUNCTION
SS.total <- with(df_test_po, sum((calls_count - mean(calls_count))^2))
SS.residual <- with(df_test_po, sum((calls_count - pred)^2))
SS.regression <- with(df_test_po, sum((pred - mean(calls_count))^2))
SS.regression/SS.total


# Test prediction example with 2019
# df_test_2019 <- df_test
# df_test_2019$year <- 2021
# df_test_2019$is_pre <- ifelse(df_test_2019$year <= 2009, 1, 0)
# df_test_2019$year_is_pre <- df_test_2019$year * df_test_2019$is_pre
# df_test_2019$pred <- predict(po_model, newdata = df_test_2019, type = "response")

#####################
### RANDOM FOREST ###
#####################

library(ranger)
library(dplyr)
df_md_rf <- df_test
rf_model <- ranger(fmla, df_train, num.trees = 500, respect.unordered.factors = "order")
df_md_rf$pred <- predict(rf_model, df_test)$predictions

## TODO Get R2 for train and test

df_md_rf %>% mutate(residual = pred - calls_count) %>% summarize(rmse = sqrt(mean(residual^2)))

#########################
### GRADIENT BOOSTING ###
#########################

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

elog_train <- elog[c("iter", "train_rmse_mean")]
names(elog_train) <- c("iter", "rmse_mean")
elog_train$dataset <- "Train"
elog_test <- elog[c("iter", "test_rmse_mean")]
names(elog_test) <- c("iter", "rmse_mean")
elog_test$dataset <- "Test"
elog_stack <- rbind(elog_train, elog_test)
library(ggplot2)
ggplot(elog_stack, aes(x = factor(iter), y = rmse_mean, colour = dataset)) + geom_point() + xlab("nrounds")

gb_model <- xgboost(data = as.matrix(df_md_gb.treat),
                    label = df_md_gb$calls_count,
                    nrounds = nrounds,
                    objectives = "reg:linear",
                    eta = 0.3, 
                    depth = 6)

df_md_gb_test <- df_test
treatplan <- designTreatmentsZ(df_md_gb_test, vars)
newvars <- treatplan$scoreFrame %>% filter(code %in% c("clean", "lev")) %>% magrittr::use_series(varName)

df_md_gb_test.treat <- prepare(treatplan, df_md_gb_test, varRestriction = newvars)



df_md_gb_test.treat$pred <- predict(gb_model, as.matrix(df_md_gb_test.treat))
gb_pred <- df_md_gb_test.treat$pred

df_md_gb_test <- cbind(df_md_gb_test, gb_pred)

df_md_gb_test %>% mutate(residual = gb_pred - calls_count) %>% summarize(rmse = sqrt(mean(residual^2)))

# Check the importance of each feature
# xgb.importance(colnames(df_md_gb.treat), model = gb_model)



# plot
plot(x=df_md_gb_test$gb_pred,y=df_md_gb_test$calls_count)
abline(lm(df_md_gb_test$calls_count ~ df_md_gb_test$gb_pred), col = "blue")




