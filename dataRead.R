


fetchData <- function()
{
  mazda_dat <- carsalesScrape(make = 'mazda',minPrice = 5000, maxPrice = 15000)
  toyota_dat <- carsalesScrape(make = 'toyota',minPrice = 5000, maxPrice = 15000)
  ford_dat <- carsalesScrape(make = 'ford',minPrice = 5000, maxPrice = 15000)
  nissan_dat <- carsalesScrape(make = 'nissan',minPrice = 5000, maxPrice = 15000)
  
  saveRDS(mazda_dat,'data/mazda.RDS') 
  saveRDS(toyota_dat,'data/toyota.RDS') 
  saveRDS(ford_dat,'data/ford.RDS') 
  saveRDS(nissan_dat,'data/nissan.RDS')
  
  dat = rbindlist(list(mazda_dat,toyota_dat,ford_dat,nissan_dat),use.names = T)
  dat[,c('make','model') :=tstrsplit(car, ' ', keep = c(2,3))]
  dat[, ':='(make = toupper(make), model = toupper(model))]
  # Filter out make,model combinations that have very few observations
  
  listMakeModel <- dat[, .N, by = .(make,model)][N>10, .(make,model, keep = T)]
  dat <- listMakeModel[dat, on = .(make,model)]
  dat <- dat[!is.na(keep)] 
  dat[, keep := NULL ]
  
  # some data type conversions
  dat[, year := as.numeric(year)]
  dat[, Body := as.factor(Body)]
  dat[, Transmission := as.factor(Transmission)]
  
  # Only interested in Sedan, Hatch and SUV
  dat <- dat[ Body %in% c('Sedan','Hatch','SUV')]
  
  # Only interested in used cars and private seller cars
  dat <- dat[adType %in% c('Used Car','Private Seller Car')]
  
  # Remove transmission NA 
  dat <- dat[!is.na(Transmission)]
  write.csv2(dat,'data/cars.csv',row.names = F)
}

readData <- function()
{
 dat = setDT(read.csv2('data/cars.csv',header = T))
 return(dat)
}  


