library(RCurl)
library(XML)
library(dplyr)
library(tidyr)
library(data.table)
library(lme4)
library(randomForest)
library(ggplot2)
library(merTools)
library(sjPlot)
library(sjmisc)

source('dataRead.R')


#fetchData()
dat <- readData()

dat <- dat[make %in% c('TOYOTA','FORD','MAZDA')]
# fit some  models
fitlm <- lm(dat, formula = 'log(price) ~ 1 + make + Body + model  + log(year) + log(Odometer) + state + adType')

fitrf <- randomForest( price ~ make + Body + model + year + Odometer + state +
                       Transmission + adType,
                     data=dat, 
                     importance=TRUE, 
                     ntree=500)

fitlmer <- lmer(dat, formula = 'log(price) ~ 1 + log(year) + log(Odometer) + (1|Body) + (1|state) + (1|Transmission) + (1|make) + (1 | make:model)  + (1 |adType)')

# calculate R2
predlm <- predict(fitlm, dat)
predlmer <- predict(fitlmer,dat)
predrf <- predict(fitrf,dat)

1-sum((log(dat$price)-predlm)^2)/sum((log(dat$price)-mean(log(dat$price)))^2) 
1-sum((log(dat$price)-predlmer)^2)/sum((log(dat$price)-mean(log(dat$price)))^2) 
1-sum((dat$price-predrf)^2)/sum((dat$price-mean(dat$price))^2) 

# do some plotting
sjp.lmer(fitlmer, y.offset = .4)

ggplot(dat) + geom_point(aes(x=log(dat$price), y=predlmer,col = state)) + 
  geom_abline(slope = 1, intercept = 0, col = 'red')

ggplot(dat) + geom_point(aes(x=dat$price, y=predrf,col = state)) + 
  geom_abline(slope = 1, intercept = 0, col = 'red')



# predict for some specific instances

make_pred <-  'TOYOTA'
model_pred <- 'YARIS'
datPred <- data.table(
  make = make_pred,
  model = model_pred,
  year = 2008,
  Odometer = 51500,
  Transmission = 'Automatic',
  state = 'VIC',
  Body = 'Hatch',
  adType = 'Used Car'
)

# make_pred <-  'MAZDA'
# model_pred <- '2'
# datPred <- data.table(
#   make = make_pred,
#   model = model_pred,
#   year = 2009,
#   Odometer = 86232,
#   Transmission = 'Automatic',
#   state = 'VIC',
#   Body = 'Hatch',
#   adType = 'Used Car'
# )

preds_lmer <- predictInterval(fitlmer, newdata = datPred, n.sims = 999, level = .95,  returnSims = T)
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



# do some plotting to compare different model fits

pred_lmer <- predict(fitlmer,dat[make == make_pred & model == model_pred])
pred_lm <- predict(fitlm,dat[make == make_pred & model == model_pred])
pred_lm_no_pool <- predict(fitlm_no_pool,dat[make == make_pred & model == model_pred])
pred_rf <- predict(fitrf,dat[make == make_pred & model == model_pred])

datVis <- copy(dat[make == make_pred & model == model_pred])
datVis[,':='(multLevel = pred_lmer, compPool = pred_lm, noPool = pred_lm_no_pool,
             randomForest = log(pred_rf))]
idVar = c("make", "model" , "year" ,"Body", "Odometer", "Transmission",
          "price",  "state", "adType" )
measVar = c("multLevel","compPool", "noPool", "randomForest")
datVis <- melt(datVis,id.vars = idVar, measure.vars = measVar, variable.name = 'method', value.name = 'prediction')

# fitted vs observed
ggplot(datVis,aes(x = log(price), y = prediction)) + geom_point() + 
#  geom_smooth(method = 'loess',se = T, col = 'black',alpha = .5) +
  geom_abline(slope = 1, intercept = 0, col = 'red') + facet_grid( method ~.)

# residuals vs  observed 
ggplot(datVis,aes(x = log(price), y = log(price) - prediction)) + geom_point() + 
  #  geom_smooth(method = 'loess',se = T, col = 'black',alpha = .5) +
  geom_abline(slope = 0, intercept = 0, col = 'red') + facet_grid( method ~.)


datVis[,1 - sum((log(price)-prediction)^2)/sum((log(price)-mean(prediction))^2), by = method ]
