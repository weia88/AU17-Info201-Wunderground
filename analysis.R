library(jsonlite)
library(httr)
library(dplyr)
source("api-keys.r")

all.states <- read.csv(file = "data/states.csv")

# Returns city 10-day forecast given state and city name. 
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

months <- formatC(c(2, 5, 8, 11), width = 2, flag = "0")

# Retrives monthly historical data for the given state/city.
GetHistoricalData <- function(state, city, progress){
  formatted.city <- gsub(" ", "_", city)
  # Requires multiple api calls, perform this using lapply
  urls <- paste0("http://api.wunderground.com/api/", api.key, "/planner_", months, "01", months, "31/q/", state, "/", formatted.city, ".json")
  data <- lapply(urls, function(url){
    # Increment our progress bar
    if(!is.null(progress))
      progress$inc(1 / length(months) / 2)
    request <- GET(url)
    response <- fromJSON(content(request, "text"))
    return (response$trip)
  })
  
  # API data is a bit oddly formatted, make a dataframe by first making vectors out of the data we want
  highlist <- sapply(data, function(x) { return (x$temp_high$avg$F) })
  lowlist <- sapply(data, function(x) { return (x$temp_low$avg$F) })
  precip <- sapply(data, function(x) { 
    return (x$precip$avg$`in`)})
  df <- data.frame(low = lowlist, high = highlist, precip = precip)
  return (df)
}

# Store this description for later use
about.description <- "This web application intends to help those traveling to new areas of the country, whether
it's for a short time or permanently. We use data from the Wunderground Weather API to make our plots. This webapp 
was built by Andrew Wei, Austin Hsieh, Gol-Dann Slater, and Jin Zhou, and its source is available at 
https://github.com/weia88/AU17-Info201-Wunderground."
