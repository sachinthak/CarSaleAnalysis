library(RCurl)
library(XML)
library(dplyr)
library(tidyr)
library(data.table)
library(lme4)
library(randomForest)
library(ggplot2)
library(merTools)
source('dataRead.R')


#fetchData()
dat <- readData()

# fit some  models
fitlm <- lm(dat, formula = 'log(price) ~ 1 + make + Body + model  + log(year) + log(Odometer) + state + adType')

fitrf <- randomForest( price ~ make + Body + model + year + Odometer + state +
                       Transmission + adType,
                     data=dat, 
                     importance=TRUE, 
                     ntree=500)

fitlmer <- lmer(dat, formula = 'log(price) ~ 1 + log(year) + log(Odometer) + (1|Body) + (1|state) + (1|Transmission) + (1|make) + (1 |make:model) + (1 |adType)')

# calculate R2
predlm <- predict(fitlm, dat)
predlmer <- predict(fitlmer,dat)
predrf <- predict(fitrf,dat)

1-sum((log(dat$price)-predlm)^2)/sum((log(dat$price)-mean(log(dat$price)))^2) 
1-sum((log(dat$price)-predlmer)^2)/sum((log(dat$price)-mean(log(dat$price)))^2) 
1-sum((dat$price-predrf)^2)/sum((dat$price-mean(dat$price))^2) 

# do some plotting
ggplot(dat) + geom_point(aes(x=log(dat$price), y=predlmer,col = state)) + 
  geom_abline(slope = 1, intercept = 0, col = 'red')

ggplot(dat) + geom_point(aes(x=dat$price, y=predrf,col = state)) + 
  geom_abline(slope = 1, intercept = 0, col = 'red')



# predict for some specific instances

make_pred <-  'MAZDA'
model_pred <- '2'
datPred <- data.table(
  make = make_pred,
  model = model_pred,
  year = 2012,
  Odometer = 76000,
  Transmission = 'Automatic',
  state = 'VIC',
  Body = 'Hatch',
  adType = 'Used Car'
)

preds_lmer <- predictInterval(fitlmer, newdata = datPred, n.sims = 999, returnSims = T, level = .95)
exp(preds_lmer)

# try fitting a completely pooled linear model
exp(predict(fitlm,datPred,interval = 'prediction',level = .95))
  
# try fitting a no pooled simple linear model
fitlm_no_pool <- lm(dat[make == make & model == model], formula = 'log(price) ~ 1  + Body  + log(year) + log(Odometer) + state + adType')
exp(predict(fitlm_no_pool,datPred,interval = 'prediction',level = .95))


# random forest prediction needs special handling of factors
common <- intersect(names(dat), names(datPred)) 
for (p in common) { 
  if (class(dat[[p]]) == "factor") { 
    datPred[[p]] <- as.factor(datPred[[p]])
    levels(datPred[[p]]) <- levels(dat[[p]]) 
  } 
}

resrf <- predict(fitrf,datPred,predict.all = T)
quantile(resrf$individual,c(.5,.025,.975))




ggplot(dat[make == 'MAZDA' & model == '2']) + geom_point(aes(x=log(price), y=predlm,col = state)) + 
  geom_abline(slope = 1, intercept = 0, col = 'red') + coord_fixed()

predlmer <- predict(fitlmer, dat[make == 'TOYOTA' & model == 'YARIS'])
ggplot(dat[make == 'TOYOTA' & model == 'YARIS']) + geom_point(aes(x=log(price), y=predlmer,col = adType)) + 
  geom_abline(slope = 1, intercept = 0, col = 'red') + coord_fixed()

predlmer[which(predlmer>9.8)]
exp(predict(fitlm,datPred,interval = 'prediction',level = .95))

