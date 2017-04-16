
## A function that 'scrapes' results from carsales.com.au for currently advertised 'used' cars (private and dealer)

## args:
## make = make of the vehicle, e.g. Volkswagen
## model = model of vehicle, e.g. Golf
## limit = the amount of results to pull in per page (maximum currently 60)
## minPrice = minimum price of the vehicle
## maxPrice = maximum price of the vehicle

carsalesScrape <- function(make="Volkswagen", model = '', minPrice = 0, maxPrice = 50000, limit=60, maxResults = 30000){

  
  # Clean up parameters
  make <- gsub(" ", "%20", make)
  model <- gsub(" ", "%20", model)
  limit <- as.numeric(limit)
  
  if(limit>60){stop("Results limit cannot be greater than 60")}
  
  
  fileUrl <- paste0('https://www.carsales.com.au/cars/results?limit=',limit,'&q=(And.Service.Carsales._.(C.Make.',make,'._.Model.',model,'.)_.Price.range(',minPrice,'..',maxPrice,').)&area=Stock&vertical=car&WT.z_srchsrcx=makemodel')
  print(paste("searching for:",make,model))
  
  ## Retrieve total number of results
  results <- getURL(fileUrl,ssl.verifypeer=TRUE)
  results_parsed <- htmlTreeParse(results, useInternalNodes=TRUE, encoding='UTF-8')
  numResults <- xpathSApply(results_parsed, "//div[@class='n_width-max results-title']", xmlValue)
  numResults <- as.numeric(paste(unlist(strsplit(unlist(numResults), "[^0-9]+")),collapse = ''))
  totalResults <- NULL
  print(paste("number of results:",numResults[1]))
  

  
  ## Iteratively pull in blocks of results based on totalResults divided by limit argument
  for (i in 0:(ceiling(min(numResults[1],maxResults)/limit)-1)){
    
    print (paste0("getting results: loop ",i+1," of ",(ceiling(min(numResults[1],maxResults)/limit))))
    flush.console()
    fOffset <- i * limit
    fileUrl <- paste0('https://www.carsales.com.au/cars/results?offset=',fOffset,'&setype=pagination&q=%28And.Service.Carsales._.%28C.Make.',make,'._.Model.',model,'.%29_.Price.range%28',minPrice,'..',maxPrice,'%29.%29&limit=',limit,'&area=Stock&vertical=car&WT.z_srchsrcx=makemodel')
    totalResults <- c(totalResults,getURL(fileUrl,ssl.verifypeer=FALSE))
  }
  
  ## Parse results
  results_parsed <- htmlTreeParse(totalResults, useInternalNodes=TRUE, encoding='UTF-8')
  
  carFeatureText <- xpathSApply(results_parsed, "//div[@class='feature-text']", xmlValue)
  carFeatureType  <- xpathSApply(results_parsed, "//div[@class='feature-title']", xmlValue)  
  carFeatures <- cbind.data.frame(carFeatureType, carFeatureText, stringsAsFactors=FALSE)
 
  # the reason for doing it this way instead of the much easier way that is commented out 
  # is, sometimes some of the features such as body is missing for some records
  id = array(dim = length(carFeatureText))
  i = 0
  for (n in 1:length(carFeatureText)){
    if (carFeatureType[n] == 'Odometer'){
      i = i + 1
     
    }
    id[n] = i
  }
  carFeatures$id = id
  #carFeatures$id <- rep(1:(nrow(carFeatures)/4),each=4)
 
   carFeatures <- spread(carFeatures, carFeatureType, carFeatureText)
  carNA <- which(!is.na(carFeatures$Colour)) #Find the 'sponsored links'
  carFeatures$Odometer <- gsub(" km", "", carFeatures$Odometer)
  carFeatures$Odometer <- as.numeric(gsub(",", "", carFeatures$Odometer))
  carFeatures <- carFeatures[,-c(1,3)]
  
  ## Pull out ad type
  adType <- xpathSApply(results_parsed, "//div/span[@class='car-condition']", xmlValue)
  
  ## Pull out price
  price <- xpathSApply(results_parsed, "//div[@class='price']", xmlValue)
  price <- as.numeric(gsub("[^0-9]","",price))
  
  ## Pull out car title and manufacture year
  carTitle <- xpathSApply(results_parsed, "//h2", xmlValue)
  carTitle <- gsub("[\r\n]","",carTitle)
  carTitle <- gsub("^\\s+", "", carTitle)
  carTitle <- substring(carTitle, 1,50)
  carTitle <- unlist(strsplit(carTitle,"  +"))
  car <- carTitle[carTitle!="Take your saved cars wherever you go"]
  
  ## Pull out state
  state <- xpathSApply(results_parsed, "//span[@class='state']", xmlValue)
  
  ## Pull year of sale for vehicle (from title)
  year <- substr(car,1,4) ## extract year from car description
  URL <- paste("http://www.carsales.com.au",xpathSApply(results_parsed, "//a[@data-webm-clickvalue='sv-view-title']", xmlGetAttr, "href"), sep="")
  
  ## Column bind each variable to a data frame
  carData <- cbind.data.frame(car, year, carFeatures, price, URL, stringsAsFactors=FALSE)
  #carData <- carData[-carNA,]
  carData <- cbind.data.frame(carData, state, adType)
  setDT(carData)
  ## Write to a CSV in working folder
  #write.csv(carData, "carsales.csv")
  return(unique(carData))
}

