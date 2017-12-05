library(jsonlite)
library(httr)
library(dplyr)
# Returns city 10-day forecast given state and city name. 
# Note: In returned dataframe:
# 'pop' = probability of precipitation.
# 'qpf' = quantitative precipitation forecast
GetForecast <- function(state, city){
  formatted.city <- gsub(" ", "_", city)
  url <- paste0("http://api.wunderground.com/api/", api.key, "/forecast10day/q/", state, "/", formatted.city, "/.json")
  request <- GET(url)
  response <- fromJSON(content(request, "text"))
  simple.forecast <- flatten(response$forecast$simpleforecast$forecastday)
  return (simple.forecast)
}

GetValues <- function(location){
  values <- location %>%
              select(date.day, date.month, date.year, high.fahrenheit, low.fahrenheit, pop, avehumidity)
  return(values)
}