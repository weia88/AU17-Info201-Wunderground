library(jsonlite)
library(httr)
library(dplyr)
# Returns city 10-day forecast given state and city name. 
# Note: In returned dataframe:
# 'pop' = probability of precipitation.
# 'qpf' = quantitative precipitation forecast
all.states <- read.csv(file = "data/states.csv")

# Creates a function that obtains a certain state and city's 10 day forcast information 
GetForecast <- function(state, city){
  formatted.city <- gsub(" ", "_", city)
  url <- paste0("http://api.wunderground.com/api/", api.key, "/forecast10day/q/", state, "/", formatted.city, "/.json")
  request <- GET(url)
  response <- fromJSON(content(request, "text"))
  if(!is.data.frame(response$forecast$simpleforecast$forecastday))
    return ("Error")
  simple.forecast <- flatten(response$forecast$simpleforecast$forecastday)
  return (simple.forecast)
}

# Processes data for a given location.
GetValues <- function(location){
  values <- location %>%
              select(date.day, date.month, date.year, high.fahrenheit, low.fahrenheit, pop, avehumidity)
  return(values)
}

# Creates a function that obtains the alert of a certain city and state
GetAlert <- function(state, city){
  formatted.city <- gsub(" ", "_", city)
  url <- paste0("http://api.wunderground.com/api/", api.key, "/alerts/q/", state, "/", formatted.city, "/.json")
  request <- GET(url)
  response <- fromJSON(content(request, "text"))
  if(is.na(dim(response$alerts)[1] == 0 && dim(response$alerts)[2] == 0)){ 
    filtered.alert <- "NO ALERT IN THIS CURRENT AREA"
  } else {
    alert <- response$alerts[, c(2:5)] 
    filtered.alert <- toupper(paste(alert$description, "Began:", alert$date, "Ends:", alert$expires, sep = " "))
  }
  return (filtered.alert)
}

# Retrives monthly historical data for the given state/city.
GetHistoricalData <- function(state, city){
  months <- formatC(c(1:12), width = 2, flag = "0")
  formatted.city <- gsub(" ", "_", city)
  urls <- paste0("http://api.wunderground.com/api/", api.key, "/planner_", months, "01", months, "31/q/", state, "/", formatted.city, ".json")

  data <- lapply(urls, function(url){
    request <- GET(url)
    response <- fromJSON(content(request, "text"))
    return (response$trip)
  })

  highlist <- sapply(data, function(x) { return (x$temp_high$avg$F) })
  lowlist <- sapply(data, function(x) { return (x$temp_low$avg$F) })
  df <- data.frame(low = lowlist, high = highlist)
  return (df)
}
