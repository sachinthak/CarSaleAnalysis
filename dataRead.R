library(RCurl)
library(XML)
library(dplyr)
library(tidyr)

readData <- function()
{
  mazda_dat <- carsalesScrape(make = 'mazda',minPrice = 5000, maxPrice = 15000)
  toyota_dat <- carsalesScrape(make = 'toyota',minPrice = 5000, maxPrice = 15000)
  ford_dat <- carsalesScrape(make = 'ford',minPrice = 5000, maxPrice = 15000)
  saveRDS(mazda_dat,'data/mazda.RDS') 
  saveRDS(toyota_dat,'data/toyota.RDS') 
  saveRDS(ford_dat,'data/ford.RDS') 
  
  dat = rbindlist(list(mazda_dat,toyota_dat,ford_dat),use.names = T)
}