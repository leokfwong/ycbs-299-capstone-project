####################################################################################################
######################                                                        ######################
######################                      CAPSTONE PROJECT                  ######################
######################                                                        ######################
####################################################################################################

# Set working directory
setwd("H:/CTN222Database/Data Management/Stats/Enrollment Checklist/src")

###################
### IMPORT DATA ###
###################

# Interventions des pompiers de Montr?al
calls_raw <- read.csv("G:/YCBS - 299/data/donneesouvertes-interventions-sim.csv",
                      header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

# Casernes de pompiers sur l'?le de Montr?al
stations_raw <- read.csv("G:/YCBS - 299/data/casernes.csv",
                         header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

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


#########################
### DATA MANIPULATION ###
#########################

# Derive variables of interest (potential features)
df_clean <- within(calls_raw, {

  day <- format(strptime(creation_date_time, format = "%Y-%m-%d %H:%M:%S"), "%d")
  month <- format(strptime(creation_date_time, format = "%Y-%m-%d %H:%M:%S"), "%m")
  year <- format(strptime(creation_date_time, format = "%Y-%m-%d %H:%M:%S"), "%y")
  hour <- format(strptime(creation_date_time, format = "%Y-%m-%d %H:%M:%S"), "%H")
  day_of_week <- weekdays(as.Date(creation_date_time, "%Y-%m-%d"), FALSE)
  season <- getSeason(as.Date(creation_date_time, "%Y-%m-%d"))

})

# Aggregate data so that every row represent a day with the sum of calls.
# Note the the additional variables are features that a specific to an overall day.
# (ie. cannot use neighborhood, since within a day there could be many).
df_perday <- unique(
  merge(x = aggregate(data = df_clean, incident_nbr ~ year + month + day, FUN = length),
        y = df_clean[, c("year", "month", "day", "day_of_week", "season")],
        by = c("year", "month", "day"),
        all.x = TRUE
  )
)

# Compute the mean and variance to see if there is a significant difference
mean(df_perday$incident_nbr)
var(df_perday$incident_nbr)

# Define the Y and x
fmla <- incident_nbr ~ day_of_week + season

# Run model (Note we use quasipoisson since var >>> mean)
model <- glm(fmla, data = df_perday, family = quasipoisson)

# Test prediction
newdat <- data.frame(day_of_week = "Friday", season = "Winter")
pred <- predict(model, newdata = newdat, type = "response")

# NEXT STEPS:
# 1) Compare models using different features (AIC).
# 2) Split data into train and test and minimize MSE.
# 3) Try different models (ie. Random Forest).

# Gradient boosting