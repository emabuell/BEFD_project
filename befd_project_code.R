library(dplyr)
#install.packages('Metrics')
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(fpp2)
library(forecast)
library(lmtest)
library(ggplot2)


lr.cv = function(train,test){
  ml_features = c('AVG_WAIT_DURATION',
                  'ONLINE_SALES_DAY_BEFORE',
                  'HOUR',
                  'HOURS_OPEN',
                  'PEOPLE_IN_LINE_LAG',
                  'WEEK_NUMBER',
                  'RAIN',
                  'IS_OCTOBER',
                  'AVG_WAIT_PER_STEP_LAG'
                  # 'YEAR'
  )
  lm = lm(AVG_WAIT_DURATION~.,data=train[ml_features])
  pred = predict(lm,test[ml_features])
  
  r = round(summary(lm)$r.squared,2)
  aic = AIC(lm)
  rmse = rmse(pred, test$AVG_WAIT_DURATION)
  results = data.frame(r = r, aic = aic, rmse = rmse)
  
  # plot(modelDf$AVG_WAIT_DURATION, type = 'l', main = 'Linear Regression')
  # lines(fitted(lm),col=2)
  # lines((nrow(train)+1):(nrow(train)+5),pred, col = 3)
  
  return(list(results, pred))
}




###################
# Data Processing #
###################

# Set working directory
setwd('C:\\Users\\emanu\\OneDrive\\Desktop\\progetto_business')

# Read wait time data
ww = read.csv('waitwhile_visits_20200901_20211103.csv')
ww$WAIT_DURATION = ww$WAIT_DURATION/60

# Read dummy data
dv = read.csv('dummy_variables.csv')
dv$DATE = as.Date(dv$DATE, format = '%m/%d/%Y')

# Read online ticket scans
os = read.csv('online_ticket_scans.csv')
os = os %>% mutate_at('Check.In.Date', strptime, format="%m/%d/%Y %H:%M")
os$TICKET_HOUR = as.character(round(os$Check.In.Date, units = "hours"))

# Read on-site register sales (excluding online which is in df above)
tb = read.csv('Regiscare Dashboard Data.csv')
tb = tb %>% mutate_at('Full.Timestamp', strptime, format="%m/%d/%Y %H:%M")
tb$TICKET_HOUR = as.character(round(tb$Full.Timestamp, units = "hours"))
tb = tb %>% filter(Payment.Type != 'Online')


# Combine online and on-site ticket counts into a single df by hour of park enter
ticketQuantityDf = rbind(
  os %>% 
    group_by(TICKET_HOUR) %>% 
    summarize(TICKET_COUNT = sum(Attraction.Quantity)) %>% 
    mutate(TICKET_TYPE = 'ONLINE')
  ,
  tb %>% 
    group_by(TICKET_HOUR) %>% 
    summarize(TICKET_COUNT = sum(Quantity)) %>% 
    mutate(TICKET_TYPE = 'ONSITE')
)

# Group online and on-site ticket counts
ticketQuantityDf = ticketQuantityDf %>% 
  group_by(TICKET_HOUR) %>% 
  summarize(TICKET_COUNT = sum(TICKET_COUNT))

# Define function to process the data
model.data.processing = function(ww, dv) {

  # Change dates to date format
  ww = ww %>% mutate_at(vars(ends_with("_TIME")), strptime, format="%m/%d/%Y %H:%M")
  
  # Truncate waitlist time to hour
  ww$WAITLIST_TIME_HOUR = round(ww$WAITLIST_TIME, units = "hours")
  
  # Combine line exit timestamps into one column
  ww$EXIT_TIME = coalesce(ww$SERVE_TIME, ww$REMOVED_TIME, ww$CANCEL_TIME, ww$NO_SHOW_TIME, ww$EXPIRED_TIME)
  
  # Create date (timezone aware)
  ww$HOUR = format(ww$WAITLIST_TIME_HOUR, format = "%H")
  ww$DAY = format(as.Date(ww$WAITLIST_TIME_HOUR), format = "%d")
  ww$MONTH = format(as.Date(ww$WAITLIST_TIME_HOUR), format = "%m")
  ww$YEAR = format(as.Date(ww$WAITLIST_TIME_HOUR), format = "%Y")
  ww$DATE = as.Date(with(ww,paste(ww$YEAR,ww$MONTH,ww$DAY,sep="-")),"%Y-%m-%d")
  ww$DATE = if_else(ww$HOUR != '00', ww$DATE, ww$DATE - 1)
  
  # Get the visit associated to the longest line each hour
  ww_max_length = ww[order(ww$WAITLIST_TIME_HOUR,-ww$ORIGINAL_POSITION),]
  ww_max_length = ww_max_length[!duplicated(ww_max_length$WAITLIST_TIME_HOUR),]
  
  # Loop through each max line by hour and sum party size for people still in line
  PEOPLE_IN_LINE = c()
  for (row in 1:nrow(ww_max_length)) {
    max_record = ww_max_length[row,]
    wwLine = ww[ww$WAITLIST_TIME < max_record$WAITLIST_TIME,]
    wwLine = wwLine[wwLine$EXIT_TIME > max_record$WAITLIST_TIME,]
    PEOPLE_IN_LINE = c(PEOPLE_IN_LINE,sum(wwLine$PARTY_SIZE,na.rm=T))
  }
  rm(max_record)
  rm(wwLine)
  rm(row)
  
  # Create WAIT_PER_STEP
  ww$WAIT_PER_STEP = ww$WAIT_DURATION / ww$ORIGINAL_POSITION
  
  # Generate new columns as feature and target variables
  modelDf = cbind(ww %>%
                    group_by(across(all_of(c('WAITLIST_TIME_HOUR','DATE','HOUR')))) %>%
                    summarise(MAX_WAIT_DURATION = max(WAIT_DURATION, na.rm=TRUE),
                              QUANT_95_WAIT_DURATION = quantile(WAIT_DURATION, c(.95), na.rm=TRUE),
                              AVG_WAIT_DURATION = mean(WAIT_DURATION, na.rm=TRUE),
                              SD_WAIT_DURATION = sd(WAIT_DURATION, na.rm = TRUE),
                              AVG_WAIT_PER_STEP = mean(WAIT_PER_STEP, na.rm=TRUE),
                              MAX_ORIGINAL_POSITION = max(ORIGINAL_POSITION),
                              N = n()
                    ) %>% 
                    arrange(WAITLIST_TIME_HOUR)
                  ,data.frame(PEOPLE_IN_LINE))
  
  # Create uniform hours and days data frame
  d = subset(dv, YEAR %in% c(2020,2021))[['DATE']]
  h = c('19','20', '21', '22', '23')
  x = data.frame(expand.grid(d, h, stringsAsFactors = FALSE))
  x$WAITLIST_TIME_HOUR = paste(x$Var1, " ",x$Var2,":00:00",sep="")
  
  # Join uniform date and hour dataframe and modelDf file
  modelDf = merge(x = x, y = modelDf, by = "WAITLIST_TIME_HOUR", all.x = TRUE)
  
  # Remove temporary columns
  modelDf$DATE = modelDf$Var1
  modelDf$HOUR = modelDf$Var2
  modelDf = modelDf[ -c(2:3) ]
  
  # Convert HOUR number to integer
  modelDf$HOUR_NUMBER = as.integer(modelDf$HOUR)
  
  # Join dummy variables to the modelDf
  modelDf = merge(x = data.frame(modelDf), y = subset(dv, YEAR %in% c(2020,2021)), 
                  by = "DATE", all.x = TRUE)
  modelDf = merge(x = data.frame(modelDf), y = ticketQuantityDf, by.x = "WAITLIST_TIME_HOUR", 
                  by.y = 'TICKET_HOUR', all.x = TRUE)
  
  # Offset data by one week
  modelDf[is.na(modelDf)] = 0
  modelDf = modelDf %>% 
    mutate(PEOPLE_IN_LINE_LAG=lag(PEOPLE_IN_LINE,15)) %>% 
    mutate(AVG_WAIT_PER_STEP_LAG=lag(AVG_WAIT_PER_STEP,15)) %>% 
    mutate(TICKET_COUNT_LAG=lag(TICKET_COUNT,15))
  
  # Make all NA values 0 in modelDf
  modelDf[is.na(modelDf)] = 0
  
  rm(dv)
  rm(d)
  rm(ww_max_length)
  rm(x)
  rm(h)
  rm(PEOPLE_IN_LINE)
  
  return(modelDf)
  
}

# Filter data to single attraction
wwH = ww[ww$LOCATION_NAME=='Hayride of Doom',]
modelDfH = model.data.processing(wwH,dv)
modelDfH[modelDfH$N < 3,c('AVG_WAIT_PER_STEP','AVG_WAIT_DURATION')] = 0

# Filter data to single attraction
wwC = ww[ww$LOCATION_NAME=='Castle of the Dead',]
modelDfC = model.data.processing(wwC,dv)
modelDfC[modelDfC$N <= 3,c('AVG_WAIT_PER_STEP','AVG_WAIT_DURATION')] = 0

# Filter data to single attraction
wwF = ww[ww$LOCATION_NAME=='Forest of Darkness',]
modelDfF = model.data.processing(wwF,dv)
modelDfF[modelDfF$N < 3,c('AVG_WAIT_PER_STEP','AVG_WAIT_DURATION')] = 0

# Filter data to single attraction
wwM = ww[ww$LOCATION_NAME=='Terror Zone Maze',]
modelDfM = model.data.processing(wwM,dv)

# Set modelDf to a single attraction  
modelDf = modelDfH

########################
# Descriptive Analysis #
########################

# Trend by hour
par(mfrow=c(1,1))
plot(modelDf$MAX_WAIT_DURATION, type = "l", lty = 1)
lines(modelDf$QUANT_95_WAIT_DURATION, col='4')
lines(modelDf$AVG_WAIT_DURATION, col='2')

# Plot correlation between features
corr_cols = c('AVG_WAIT_DURATION',
              'AVG_WAIT_PER_STEP',
              'PEOPLE_IN_LINE',
              'TICKET_COUNT',
              'AVG_WAIT_PER_STEP_LAG',
              'PEOPLE_IN_LINE_LAG',
              'TICKET_COUNT_LAG',
              'ONLINE_SALES_DAY_BEFORE')
chart.Correlation(modelDf[corr_cols], histogram = TRUE, method = "pearson")

# DOW Box Plot
boxplot(AVG_WAIT_DURATION ~ WEEKDAY,modelDf, main = 'Avg Wait Duration by DOW', 
        names=c("Friday","Saturday","Sunday"))

# WEEK_NUMBER Box Plot
boxplot(AVG_WAIT_DURATION ~ WEEK_NUMBER,modelDf)

# Compare 2020 with 2021 ween number wait times
par(mfrow=c(1,2))
boxplot(AVG_WAIT_DURATION ~ WEEK_NUMBER,modelDf[modelDf$YEAR == 2020,],main = "2020")
boxplot(AVG_WAIT_DURATION ~ WEEK_NUMBER,modelDf[modelDf$YEAR == 2021,],main = "2021")

# YEAR Box Plot
boxplot(AVG_WAIT_DURATION ~ YEAR,modelDf)

# MONTH Box Plot
boxplot(AVG_WAIT_DURATION ~ MONTH,modelDf, main = 'Average Wait Duration by Month', 
        names=c("September","October",'November'))

# Compare 2020 and 2021 Wait Duration by MONTH
par(mfrow=c(1,2))
boxplot(AVG_WAIT_DURATION ~ MONTH,modelDf[modelDf$YEAR == 2020,])
boxplot(AVG_WAIT_DURATION ~ MONTH,modelDf[modelDf$YEAR == 2021,])

# HOUR Box Plot
par(mfrow=c(1,1))
boxplot(AVG_WAIT_DURATION ~ HOUR,modelDf)

# Compare 2020 and 2021 Wait Duration by HOUR
par(mfrow=c(1,2))
boxplot(AVG_WAIT_DURATION ~ HOUR,modelDf[modelDf$YEAR == 2020,])
boxplot(AVG_WAIT_DURATION ~ HOUR,modelDf[modelDf$YEAR == 2021,])

# Plot Acf of AVG_WAIT_DURATION
par(mfrow=c(1,1))
Acf(modelDfH$AVG_WAIT_DURATION, main = 'ACF for Average Wait Duration - Hayride', xlab='Lag in Hours (5 hours per day, 3 days in a week)')

###############################
# Linear Regression - HAYRIDE #
###############################

modelDf = modelDfH
modelDf_no_0 = modelDf[modelDf$HOURS_OPEN != 0,]

features = c('AVG_WAIT_DURATION','AVG_WAIT_PER_STEP_LAG',
             'PEOPLE_IN_LINE_LAG',
             'HOUR_NUMBER','YEAR','WEEKDAY','MONTH','HOURS_OPEN','IS_OPEN_DAYS',
             'WEEK_NUMBER','BAD_DAYS','RAIN','TICKET_COUNT_LAG',
             'ONLINE_SALES_DAY_BEFORE')

min.model = lm(AVG_WAIT_DURATION ~ 1, data=modelDf[features])
biggest <- formula(lm(AVG_WAIT_DURATION~.,modelDf[features]))
fwd.model = step(min.model, direction='forward', scope=biggest)
summary(fwd.model)

dwtest(fwd.model)
# 
# beta = data.frame(coef(summary(fwd.model)))
# write.csv(beta,'lm_maze.csv')

plot(modelDf$AVG_WAIT_DURATION, type = 'l')
lines(fitted(fwd.model),col=2)

plot(residuals(fwd.model),type='l')

Acf(residuals(fwd.model))



#################
# Holdout Check #
#################
library(Metrics)

lr.cv = function(train,test){
  ml_features = c('AVG_WAIT_DURATION',
                  'ONLINE_SALES_DAY_BEFORE',
                  'PEOPLE_IN_LINE_LAG',
                  'WEEK_NUMBER',
                  'RAIN',
                  'HOUR_NUMBER',
                  'IS_OPEN_DAYS',
                  'MONTH',
                  'YEAR',
                  'AVG_WAIT_PER_STEP_LAG')
  lm = lm(AVG_WAIT_DURATION~.,data=train[ml_features])
  r = round(summary(lm)$r.squared,2)
  aic = AIC(lm)
  
  pred = predict(lm,test[ml_features])
  y_test = test$AVG_WAIT_DURATION
  rmse = rmse(pred, y_test)
  
  # results = list(r, aic, rmse)
  results = data.frame(r = r, aic = aic, rmse = rmse)
  
  return(results)
}

arima.cv = function(train,test){
  modelTrain = as.ts(train$AVG_WAIT_DURATION, freq = 5, order_by = train$WAITLIST_TIME_HOUR)
  modelTest = as.ts(test$AVG_WAIT_DURATION, freq = 5, order_by = test$WAITLIST_TIME_HOUR)
  peopleInLineTrain = as.ts(train$PEOPLE_IN_LINE_LAG, freq = 5, order_by = train$WAITLIST_TIME_HOUR)
  peopleInLineTest = as.ts(test$PEOPLE_IN_LINE_LAG, freq = 5, order_by = test$WAITLIST_TIME_HOUR)
  onlineSalesDayBeforeTrain = as.ts(train$ONLINE_SALES_DAY_BEFORE, freq = 5, order_by = train$WAITLIST_TIME_HOUR)
  onlineSalesDayBeforeTest = as.ts(test$ONLINE_SALES_DAY_BEFORE, freq = 5, order_by = test$WAITLIST_TIME_HOUR)
  

  arima.model = Arima(modelTrain, order=c(4,1,3), xreg=peopleInLineTrain+onlineSalesDayBeforeTrain)

  r = 0
  aic = summary(arima.model)$aic
  
  pred = predict(arima.model, n.ahead=length(peopleInLineTest), newxreg=peopleInLineTest+onlineSalesDayBeforeTest)
  rmse = rmse(as.numeric(pred$pred), modelTest)
  # results = list(r, aic, rmse)
  results = data.frame(r = r, aic = aic, rmse = rmse)
  return(results)
}

process.results = function(result_list, rowNames = foldDates, colNames = c("r squared", "aic", "rmse")) {
  resultsDf = do.call(rbind, result_list)
  rownames(resultsDf) = rowNames
  colnames(resultsDf) = colNames
  return(resultsDf)
}

foldDates = unique(modelDf[modelDf$AVG_WAIT_DURATION != 0 & modelDf$YEAR == 2021,]$DATE)
lm.results.list = list()
arima.results.list = list()
i = 1

for (foldDate in foldDates){
  print(foldDate)
  test = modelDf[modelDf$DATE>=foldDate,]
  train = modelDf[modelDf$DATE<foldDate,]
  
  lm.results = lr.cv(train,test)
  lm.results.list[[i]] = lm.results
  
  arima.results = arima.cv(train,test)
  arima.results.list[[i]] = arima.results
  i = i+1
}   #18931 final one,18887 starting one (in just one day)

lmResultsDf = process.results(lm.results.list)
arimaResultsDf = process.results(arima.results.list)

cbind(lmResultsDf$rmse,arimaResultsDf$rmse)
print(paste(mean(lmResultsDf$rmse),mean(arimaResultsDf$rmse)))
################
# ARIMA Models #
################


#Set up time series data for ARIMA
modelTs = as.ts(modelDf$AVG_WAIT_DURATION, freq = 5, order_by = modelDf$WAITLIST_TIME_HOUR)
hoursTs = as.ts(modelDf$HOURS_OPEN, freq = 5, order_by = modelDf$WAITLIST_TIME_HOUR)
peopleInLineTs = as.ts(modelDf$PEOPLE_IN_LINE_LAG, freq = 5, order_by = modelDf$WAITLIST_TIME_HOUR)
wpsTs = as.ts(modelDf$AVG_WAIT_PER_STEP_LAG, freq = 5, order_by = modelDf$WAITLIST_TIME_HOUR)
yearTs = as.ts(modelDf$YEAR, freq = 5, order_by = modelDf$WAITLIST_TIME_HOUR)
ticketCountTs = as.ts(modelDf$TICKET_COUNT_LAG, freq = 5, order_by = modelDf$WAITLIST_TIME_HOUR)
onlineSalesDayBeforeTs = as.ts(modelDf$ONLINE_SALES_DAY_BEFORE, freq = 5, order_by = modelDf$WAITLIST_TIME_HOUR)

test = modelDf[modelDf$DATE>=foldDate,]
train = modelDf[modelDf$DATE<foldDate,]

modelTrain = as.ts(train$AVG_WAIT_DURATION, freq = 5, order_by = train$WAITLIST_TIME_HOUR)
peopleInLineTrain = as.ts(train$PEOPLE_IN_LINE_LAG, freq = 5, order_by = train$WAITLIST_TIME_HOUR)

modelTest = as.ts(test$AVG_WAIT_DURATION, freq = 5, order_by = test$WAITLIST_TIME_HOUR)
peopleInLineTest = as.ts(test$PEOPLE_IN_LINE_LAG, freq = 5, order_by = test$WAITLIST_TIME_HOUR)

# ARIMA model
arimaModel = auto.arima(modelTrain, xreg=peopleInLineTrain)
pred = predict(arimaModel, n.ahead = length(peopleInLineTest), newxreg = peopleInLineTest)
pred$pred
plot(modelTs)
lines(fitted(arimaModel),col=2)
Acf(residuals(arimaModel))
# 
# ###
# # GAM
# ###
# 
# library(gam)
# g1 <- gam(AVG_WAIT_DURATION~s(TICKET_COUNT),data = modelDf)
# summary(g1)
# 
# modelDf['year_month_weekday'] = paste(modelDf$YEAR,modelDf$MONTH,modelDf$WEEKDAY)
# modelDfGam = modelDf[c('AVG_WAIT_DURATION',
#                              'AVG_WAIT_PER_STEP',
#                              'PEOPLE_IN_LINE',
#                              'HOUR_NUMBER',
#                              # 'YEAR',
#                              # 'WEEKDAY',
#                              # 'MONTH',
#                              # 'year_month_weekday',
#                              'HOURS_OPEN',
#                              # 'IS_OPEN_DAYS',
#                              'WEEK_NUMBER',
#                              # 'ONLINE_ONLY',
#                              # 'BAD_DAYS',
#                              # 'RAIN',
#                              # 'NUM_ATTRACTIONS',
#                              'TICKET_COUNT')]
# 
# # modelDfGam = cbind(modelDf[c('AVG_WAIT_DURATION',
# #                        'AVG_WAIT_PER_STEP',
# #                        'PEOPLE_IN_LINE',
# #                        'HOUR_NUMBER',
# #                        # 'YEAR',
# #                        # 'WEEKDAY',
# #                        # 'MONTH',
# #                        # 'year_month_weekday',
# #                        'HOURS_OPEN',
# #                        # 'IS_OPEN_DAYS',
# #                        'WEEK_NUMBER',
# #                        # 'ONLINE_ONLY',
# #                        # 'BAD_DAYS',
# #                        # 'RAIN',
# #                        # 'NUM_ATTRACTIONS',
# #                        'TICKET_COUNT'
# #                        )],
# # 
# #               modelDf[c('AVG_WAIT_PER_STEP',
# #                         'PEOPLE_IN_LINE',
# #                         # 'HOUR_NUMBER',
# #                         # 'YEAR',
# #                         # 'WEEKDAY',
# #                         # 'MONTH',
# #                         # 'year_month_weekday',
# #                         # 'HOURS_OPEN',
# #                         # 'IS_OPEN_DAYS',
# #                         # 'WEEK_NUMBER',
# #                         # 'ONLINE_ONLY',
# #                         # 'BAD_DAYS',
# #                         # 'RAIN',
# #                         # 'NUM_ATTRACTIONS',
# #                         'TICKET_COUNT'
# #               )]^2)
# 
# names(modelDfGam) = c('AVG_WAIT_DURATION',
#                       'AVG_WAIT_PER_STEP',
#                       # 'PEOPLE_IN_LINE',
#                       'HOUR_NUMBER',
#                       'HOURS_OPEN',
#                       'WEEK_NUMBER',
#                       'TICKET_COUNT',
#                       'AVG_WAIT_PER_STEP2',
#                       'PEOPLE_IN_LINE2',
#                       'TICKET_COUNT2')
# 
# g3 = gam(AVG_WAIT_DURATION~., data=modelDfGam)
# sc = gam.scope(modelDfGam, response=1, smoother='s', arg=c("df=2","df=3","df=4"))
# g4 = step.Gam(g3, scope=sc, trace=T)
# summary(g4)
# 
# plot(modelDf$AVG_WAIT_DURATION, type = 'l')
# lines(fitted(g4),col=2)
# 
# 
# ############################################################
# # PREDICTING PEOPLE_IN_LINE
# ############################################################
# Acf(peopleInLineTs)
# modelDfPil = modelDf[c(
#                       # 'AVG_WAIT_DURATION',
#                      # 'AVG_WAIT_PER_STEP',
#                      # 'PEOPLE_IN_LINE',
#                      'HOUR_NUMBER',
#                      'YEAR','WEEKDAY','MONTH','HOURS_OPEN','IS_OPEN_DAYS',
#                      'WEEK_NUMBER','ONLINE_ONLY','BAD_DAYS','RAIN',
#                      'NUM_ATTRACTIONS','TICKET_COUNT')]
# 
# min.model = lm(PEOPLE_IN_LINE ~ 1, data=modelDfPil)
# biggest <- formula(lm(PEOPLE_IN_LINE~.,modelDfPil))
# pil.lm = step(min.model, direction='forward', scope=biggest)
# summary(pil.lm)
# plot(modelDfPil$PEOPLE_IN_LINE, type = 'l')
# lines(fitted(pil.lm),col=2)
# Acf(residuals(pil.lm))
# 
# 
# 
# 
# pil.arima = auto.arima(peopleInLineTs, xreg=ticketCountTs)
# summary(pil.arima)
# Acf(residuals(pil.arima))
# plot(modelDfPil$PEOPLE_IN_LINE, type = 'l')
# lines(fitted(pil.arima),col=2)
# 
# ###
# # GAM + ARIMA
# ###
# 
# modelDfGam = modelDf[c(
#                         # 'AVG_WAIT_DURATION',
#                        'AVG_WAIT_PER_STEP',
#                        'PEOPLE_IN_LINE',
#                        'HOUR_NUMBER',
#                        # 'YEAR',
#                        # 'WEEKDAY',
#                        # 'MONTH',
#                        'HOURS_OPEN',
#                        # 'IS_OPEN_DAYS',
#                        'WEEK_NUMBER',
#                        # 'ONLINE_ONLY',
#                        # 'BAD_DAYS',
#                        # 'RAIN',
#                        # 'NUM_ATTRACTIONS',
#                        'TICKET_COUNT')]
# 
# 
# g3 = gam(PEOPLE_IN_LINE~., data=modelDfGam)
# sc = gam.scope(modelDfGam, response=1, smoother='lo', arg=c("df=2","df=3","df=4"))
# g4 = step.Gam(g3, scope=sc, trace=T)
# summary(g4)
# 
# plot(modelDf$PEOPLE_IN_LINE, type = 'l')
# lines(fitted(g4),col=2)
# 
# plot(residuals(g4),type = 'l')
# Acf(residuals(g4))
# 
# pil.arima = auto.arima(residuals(g4))
# summary(pil.arima)
# plot(modelDfPil$PEOPLE_IN_LINE, type = 'l')
# lines(fitted(g4)+fitted(pil.arima),col=2)
# 
# ##
# # ARIMA + LM
# ##
# 
# modelDfGam = modelDf[c(
#   # 'AVG_WAIT_DURATION',
#   'AVG_WAIT_PER_STEP',
#   # 'PEOPLE_IN_LINE',
#   'HOUR_NUMBER',
#   # 'YEAR',
#   # 'WEEKDAY',
#   # 'MONTH',
#   'HOURS_OPEN',
#   # 'IS_OPEN_DAYS',
#   'WEEK_NUMBER',
#   # 'ONLINE_ONLY',
#   # 'BAD_DAYS',
#   # 'RAIN',
#   # 'NUM_ATTRACTIONS',
#   'TICKET_COUNT')]
# 
# pil.arima = auto.arima(peopleInLineTs, xreg=ticketCountTs)
# 
# pil.lm = lm(residuals(pil.arima)~.,data=modelDfGam)
# summary(pil.lm)
# 
# min.model = lm(residuals(pil.arima) ~ 1, data=modelDfGam)
# biggest <- formula(lm(residuals(pil.arima)~.,modelDfGam))
# fwd.model = step(min.model, direction='forward', scope=biggest)
# summary(fwd.model)
# 
# plot(modelDfPil$PEOPLE_IN_LINE, type = 'l')
# lines(fitted(pil.arima)+fitted(pil.lm),col=2)
# 
# residuals = modelDfPil$PEOPLE_IN_LINE - (fitted(pil.arima)+fitted(pil.lm))
# Acf(residuals)
# 
# 
# 
# 
# ######################################
# ######################################
# ######################################
# ######################################
# ######################################
# ##
# ##
# ##
# ##
# ##
# ##
# ##
# ##
# ## GRAVEYARD
# ##
# ##
# ##
# ##
# ##
# ##
# ##
# ##
# ######################################
# ######################################
# ######################################
# ######################################
# ######################################
# 
# # train = modelDf[0:235,]
# # test = modelDf[236:300,]
# # train_no_0 = train[train$HOURS_OPEN != 0,]
# # test_no_0 = test[test$HOURS_OPEN != 0,]
# 
# # # Test LR model with padding
# # lm = lm(AVG_WAIT_DURATION ~ PEOPLE_IN_LINE+AVG_WAIT_PER_STEP+YEAR, data = train)
# # summary(lm)
# # plot(test_no_0$AVG_WAIT_DURATION, type = 'l')
# # lines(predict(lm, test_no_0),col=2)
# # test_pred = predict(lm, test_no_0)
# # mse_no_1 = mean((test_pred-test_no_0$AVG_WAIT_DURATION)^2)
# 
# # # Test LR model with padding removed
# # lm = lm(AVG_WAIT_DURATION ~ PEOPLE_IN_LINE+AVG_WAIT_PER_STEP+YEAR, data = train_no_0)
# # summary(lm)
# # plot(test$AVG_WAIT_DURATION, type = 'l')
# # lines(predict(lm, test_no_0),col=2)
# # test_pred = predict(lm, test_no_0)
# # mse_no_2 = mean((test_pred-test_no_0$AVG_WAIT_DURATION)^2)
# 




##########################################
# Regression Tree 
##########################################

#install.packages("rpart")
library(rpart)
library(Metrics)

# function for evaluation

rt.cv = function(train,test){
  ml_features = c('AVG_WAIT_DURATION',
                  'ONLINE_SALES_DAY_BEFORE',
                  'HOUR',
                  'HOURS_OPEN',
                  'PEOPLE_IN_LINE_LAG',
                  'WEEK_NUMBER',
                  'RAIN',
                  'IS_OCTOBER',
                  'AVG_WAIT_PER_STEP_LAG'
                  # 'YEAR'
  )
  ml_features <- colnames(df_train_tree);
  rt.mod = rpart(AVG_WAIT_DURATION~.,data=train[ml_features])
  pred = predict(rt.mod,test[ml_features])
  
  #r = round(summary(rt.mod)$r.squared,2)
  #aic = AIC(rt.mod)
  rmse = rmse(pred, test$AVG_WAIT_DURATION)
  results = data.frame( rmse = rmse)
  
  # plot(modelDf$AVG_WAIT_DURATION, type = 'l', main = 'Linear Regression')
  # lines(fitted(lm),col=2)
  # lines((nrow(train)+1):(nrow(train)+5),pred, col = 3)
  
  return(results)
}

process.results = function(result_list, rowNames = foldDates, colNames = c("rmse")) {
  resultsDf = do.call(rbind, result_list)
  rownames(resultsDf) = rowNames
  colnames(resultsDf) = colNames
  return(resultsDf)
}


# Create decision tree using regression
#df_train_tree <- modelDf_no_0[modelDf_no_0$YEAR == '2020',
                            #  -c(1,2,4,5,7,8,11,13,26)];
#df_train_tree  <- df_train_tree[,-c(3,4)]
#df_train_tree  <- df_train_tree[,-c(3,14)]
#df_train_tree  <- df_train_tree[,-c(6,9,10,11,12)]
df_train_tree <- modelDf_no_0[,-c(1,2,4,5,7,8,11,13,26)];
df_train_tree  <- df_train_tree[,-c(3,4)]
df_train_tree  <- df_train_tree[,-c(3,14)]
df_train_tree  <- df_train_tree[,-c(6,9,10,11,12)]

df_train_tree$HOUR <- as.factor(df_train_tree$HOUR)
df_train_tree$WEEKDAY <- as.factor(df_train_tree$WEEKDAY)
df_train_tree$MONTH <- as.factor(df_train_tree$MONTH)
df_train_tree$HOURS_OPEN <- as.factor(df_train_tree$HOURS_OPEN)
#df_train_tree$IS_OPEN_DAYS <- as.factor(df_train_tree$IS_OPEN_DAYS)
df_train_tree$WEEK_NUMBER <- as.factor(df_train_tree$WEEK_NUMBER)
df_train_tree$ONLINE_ONLY <- as.factor(df_train_tree$ONLINE_ONLY)
#df_train_tree$BAD_DAYS <- as.factor(df_train_tree$BAD_DAYS)
#df_train_tree$POWER_OUT <- as.factor(df_train_tree$POWER_OUT)
#df_train_tree$RAIN <- as.factor(df_train_tree$RAIN)
#df_train_tree$BOGO <- as.factor(df_train_tree$BOGO)
#df_train_tree$NUM_ATTRACTIONS <- as.factor(df_train_tree$NUM_ATTRACTIONS)
df_train_tree$WEEKDAY <- as.factor(df_train_tree$WEEKDAY)

fit <- rpart(df_train_tree$AVG_WAIT_DURATION ~ .,
             method = "anova", data = df_train_tree)
#Output to be present as PNG file
#png(file = "decTreeGFG.png", width = 600,
   # height = 600)

# Plot
plot(fit, uniform = TRUE,margin = 0.2,main = 'Regression Tree')
text(fit, use.n = TRUE, cex = 0.7)

#install.packages('rpart.plot')
#install.packages('maptree')
library(rpart.plot)
#library(maptree)
#draw.tree(fit,cex=3)
dev.new(width=5, height=4)
rpart.plot(fit)

# Saving the file
#dev.off()

# Print model
summary(fit)

# Create test data
df_tree_test <- modelDf_no_0[modelDf_no_0$YEAR == '2021',];


df_tree_test$HOUR <- as.factor(df_tree_test$HOUR)
df_tree_test$WEEKDAY <- as.factor(df_tree_test$WEEKDAY)
df_tree_test$MONTH <- as.factor(df_tree_test$MONTH)
df_tree_test$HOURS_OPEN <- as.factor(df_tree_test$HOURS_OPEN)
df_tree_test$IS_OPEN_DAYS <- as.factor(df_tree_test$IS_OPEN_DAYS)
df_tree_test$WEEK_NUMBER <- as.factor(df_tree_test$WEEK_NUMBER)
df_tree_test$ONLINE_ONLY <- as.factor(df_tree_test$ONLINE_ONLY)
df_tree_test$BAD_DAYS <- as.factor(df_tree_test$BAD_DAYS)
df_tree_test$POWER_OUT <- as.factor(df_tree_test$POWER_OUT)
df_tree_test$RAIN <- as.factor(df_tree_test$RAIN)
df_tree_test$BOGO <- as.factor(df_tree_test$BOGO)
#df_tree_test$NUM_ATTRACTIONS <- as.factor(df_tree_test$NUM_ATTRACTIONS)
df_tree_test$WEEKDAY <- as.factor(df_tree_test$WEEKDAY)


rmse_no_pad <- sqrt(mean((predict(fit,df_train_tree)-df_train_tree$AVG_WAIT_DURATION)^2));  #14.78 among all year
rmse_no_pad

# plot all data fit without zero padding 
cat("Predicted value:\n")
test_pred_tree <- predict(fit, df_train_tree, method = "anova");
dev.new(width=5, height=4)

plot(test_pred_tree,col = 'red',xlab = NA,xaxt = "n" ,main = 'fitting regression tree without padding',ylab = 'AVG_WAIT_TIME')

lines(test_pred_tree,col = 'red',xlab = NA,xaxt = "n")

lines(df_train_tree$AVG_WAIT_DURATION,
      col = 'black',xlab = NA,xaxt = "n")

axis(side = 1, at = seq(1,nrow(df_train_tree),5),
     labels = 
       as.character(modelDf$DATE)[seq(1,nrow(df_train_tree),5)],
     las = 2,cex.axis=.75)

legend(80, 95, legend=c("prediction", "actual"),
       col=c("red", "black"), lty=1:2, cex=0.8)

#png(file = "dec_tree_pred.png", width = 600,
#  height = 600)

# acf for non padded residuals

dev.new(width=5, height=4)
par(mfrow = c(1,2))
Acf(predict(fit,df_train_tree)-df_train_tree$AVG_WAIT_DURATION,main = 'acf tree no pad')
Pacf(predict(fit,df_train_tree)-df_train_tree$AVG_WAIT_DURATION,main = 'acf tree no pad')

dwtest(df_train_tree$AVG_WAIT_DURATION~.,data = df_train_tree)  # in general


# plot fitted train data on padded time series



df_train_tree.0 <- modelDf[,-c(1,2,4,5,7,8,11,13,26)];
df_train_tree.0  <- df_train_tree.0[,-c(3,4)]
df_train_tree.0  <- df_train_tree.0[,-c(3,14)]
df_train_tree.0  <- df_train_tree.0[,-c(6,9,10,11,12)]

df_train_tree.0$HOUR <- as.factor(df_train_tree.0$HOUR)
df_train_tree.0$WEEKDAY <- as.factor(df_train_tree.0$WEEKDAY)
df_train_tree.0$MONTH <- as.factor(df_train_tree.0$MONTH)
df_train_tree.0$HOURS_OPEN <- as.factor(df_train_tree.0$HOURS_OPEN)
#df_train_tree$IS_OPEN_DAYS <- as.factor(df_train_tree$IS_OPEN_DAYS)
df_train_tree.0$WEEK_NUMBER <- as.factor(df_train_tree.0$WEEK_NUMBER)
df_train_tree.0$ONLINE_ONLY <- as.factor(df_train_tree.0$ONLINE_ONLY)
#df_train_tree$BAD_DAYS <- as.factor(df_train_tree$BAD_DAYS)
#df_train_tree$POWER_OUT <- as.factor(df_train_tree$POWER_OUT)
#df_train_tree$RAIN <- as.factor(df_train_tree$RAIN)
#df_train_tree$BOGO <- as.factor(df_train_tree$BOGO)
#df_train_tree$NUM_ATTRACTIONS <- as.factor(df_train_tree$NUM_ATTRACTIONS)
df_train_tree.0$WEEKDAY <- as.factor(df_train_tree.0$WEEKDAY)

fit <- rpart(df_train_tree.0$AVG_WAIT_DURATION ~ .,
             method = "anova", data = df_train_tree.0)  #refit the model on padded data

rmse_pad <- sqrt(mean((predict(fit,df_train_tree.0)-df_train_tree.0$AVG_WAIT_DURATION)^2));   # 11.4 among all to year
rmse_pad

cat("Predicted value:\n")
test_pred_tree <- predict(fit, df_train_tree.0, method = "anova");
dev.new(width=5, height=4)

plot(test_pred_tree,col = 'red',xlab = NA,xaxt = "n" ,main = 'fitting regression tree with padding',ylab = 'AVG_WAIT_TIME')

lines(test_pred_tree,col = 'red',xlab = NA,xaxt = "n")

lines(df_train_tree.0$AVG_WAIT_DURATION,
      col = 'black',xlab = NA,xaxt = "n")

axis(side = 1, at = seq(1,nrow(df_train_tree.0),5),
     labels = 
       as.character(modelDf$DATE)[seq(1,nrow(df_train_tree.0),5)],
     las = 2,cex.axis=.75)


legend(120, 95, legend=c("prediction", "actual"),
       col=c("red", "black"), lty=1:2, cex=0.8)

#plot Acf residuals padded data

dev.new(width=5, height=4)
par(mfrow = c(1,2))
Acf(predict(fit,df_train_tree.0)-df_train_tree.0$AVG_WAIT_DURATION,main = 'acf tree pad')
Pacf(predict(fit,df_train_tree.0)-df_train_tree.0$AVG_WAIT_DURATION,main = 'Pacf tree pad')


# doing the incremental testing non zero padded data

foldDates = unique(modelDf[df_train_tree$AVG_WAIT_DURATION != 0 & df_train_tree$YEAR == 2021,]$DATE)
rt.results.list = list()
i = 1

for (foldDate in foldDates){
  print(foldDate)
  test = modelDf[df_train_tree$DATE>=foldDate,]
  train = modelDf[df_train_tree$DATE<foldDate,]
  
  rt.results = rt.cv(train,test)
  rt.results.list[[i]] = rt.results
  
  i = i+1
}  # around 18931 accumulated in all the year for the padded

rtResultsDf_nopad = process.results(rt.results.list)  

rtResultsDf_nopad



# doing the incremental testing zero padded data

foldDates = unique(modelDf[modelDf$AVG_WAIT_DURATION != 0 & modelDf$YEAR == 2021,]$DATE)
rt.results.list = list()
i = 1

for (foldDate in foldDates){
  print(foldDate)
  test = modelDf[modelDf$DATE>=foldDate,]
  train = modelDf[modelDf$DATE<foldDate,]
  
  rt.results = rt.cv(train,test)
  rt.results.list[[i]] = rt.results
  
  i = i+1
}  # around 18931 accumulated in all the year for the padded

rtResultsDf_pad = process.results(rt.results.list)  # had to delete the returned predictions on the cv function
  
rtResultsDf_pad






##########################################
# GAM
##########################################


# functions for the cv


gam.cv = function(train,test){
  ml_features = c('AVG_WAIT_DURATION',
                  'ONLINE_SALES_DAY_BEFORE',
                  'HOUR',
                  'HOURS_OPEN',
                  'PEOPLE_IN_LINE_LAG',
                  'WEEK_NUMBER',
                  'RAIN',
                  'IS_OCTOBER',
                  'AVG_WAIT_PER_STEP_LAG'
                  # 'YEAR'
  )
  ml_features <- colnames(train);   #does not find colnames train
  mod_gam_ <- gam(train$AVG_WAIT_DURATION ~ HOUR+WEEKDAY+
                     MONTH+WEEK_NUMBER+s(ONLINE_SALES_DAY_BEFORE,df = 3)+
                     s(PEOPLE_IN_LINE_LAG,df = 3)+s(AVG_WAIT_PER_STEP_LAG,df = 3)+
                     s(TICKET_COUNT_LAG,df  = 3),
                   data = train)#[ml_features])
  
  sc <- gam.scope(df_train_tree,
                  arg = c('df=2','df = 3','df=4'));
  gam.mod <- step.Gam(mod_gam_,scope  = sc,direction = 'backward'); 
  pred = predict.Gam(gam.mod,test)#[ml_features])
  #r = round(summary(rt.mod)$r.squared,2)
  #aic = AIC(rt.mod)
  rmse = rmse(pred, test$AVG_WAIT_DURATION)
  results = data.frame( rmse = rmse)
  
  # plot(modelDf$AVG_WAIT_DURATION, type = 'l', main = 'Linear Regression')
  # lines(fitted(lm),col=2)
  # lines((nrow(train)+1):(nrow(train)+5),pred, col = 3)
  
  return(results)
}

process.results = function(result_list, rowNames = foldDates, colNames = c("rmse")) {
  resultsDf = do.call(rbind, result_list)
  rownames(resultsDf) = rowNames
  colnames(resultsDf) = colNames
  return(resultsDf)
}




# using non padded data

df_train_tree <- modelDf_no_0[,-c(1,2,4,5,7,8,11,13,26)];
df_train_tree  <- df_train_tree[,-c(3,4)]
df_train_tree  <- df_train_tree[,-c(3,14)]
df_train_tree  <- df_train_tree[,-c(6,9,10,11,12)]

df_train_tree$HOUR <- as.factor(df_train_tree$HOUR)
df_train_tree$WEEKDAY <- as.factor(df_train_tree$WEEKDAY)
df_train_tree$MONTH <- as.factor(df_train_tree$MONTH)
df_train_tree$HOURS_OPEN <- as.factor(df_train_tree$HOURS_OPEN)
#df_train_tree$IS_OPEN_DAYS <- as.factor(df_train_tree$IS_OPEN_DAYS)
df_train_tree$WEEK_NUMBER <- as.factor(df_train_tree$WEEK_NUMBER)
df_train_tree$ONLINE_ONLY <- as.factor(df_train_tree$ONLINE_ONLY)
#df_train_tree$BAD_DAYS <- as.factor(df_train_tree$BAD_DAYS)
#df_train_tree$POWER_OUT <- as.factor(df_train_tree$POWER_OUT)
#df_train_tree$RAIN <- as.factor(df_train_tree$RAIN)
#df_train_tree$BOGO <- as.factor(df_train_tree$BOGO)
#df_train_tree$NUM_ATTRACTIONS <- as.factor(df_train_tree$NUM_ATTRACTIONS)
df_train_tree$WEEKDAY <- as.factor(df_train_tree$WEEKDAY)



library(gam)

mod_gam <- gam(df_train_tree$AVG_WAIT_DURATION ~HOUR+WEEKDAY+
                 MONTH+WEEK_NUMBER+s(ONLINE_SALES_DAY_BEFORE,df = 3)+
                 s(PEOPLE_IN_LINE_LAG,df = 3)+s(AVG_WAIT_PER_STEP_LAG,df = 3)+
                 s(TICKET_COUNT_LAG,df  = 3),
               data = df_train_tree)
summary(mod_gam)

dev.new(width=5, height=4)
par(mfrow=c(3,3))
plot(mod_gam,se = T)
#png(file = "gam.png", width = 600,
    #height = 600)

gam_test <- predict.Gam(mod_gam,new_data = df_tree_test)
#dev.new(width=5, height=4)
#par(mfrow=c(1,1))             #data art
#plot(df_tree_test)
#lines(gam_test,col= 'red')


# backward Gam model selection  #### BEST ONE

mod_gam_0 <- gam(df_train_tree$AVG_WAIT_DURATION ~HOUR+WEEKDAY+
                   MONTH+WEEK_NUMBER+s(ONLINE_SALES_DAY_BEFORE,df = 3)+
                   s(PEOPLE_IN_LINE_LAG,df = 3)+s(AVG_WAIT_PER_STEP_LAG,df = 3)+
                   s(TICKET_COUNT_LAG,df  = 3),
                 data = df_train_tree)

sc <- gam.scope(df_train_tree,
                arg = c('df=2','df = 3','df=4'));
gam_selection <- step.Gam(mod_gam_0,scope  = sc,direction = 'backward'); 

summary(gam_selection)

AIC(gam_selection)   #1607.075
#export summary of the model

require(broom) # for tidy()
require(knitr) # for kable()

kable(tidy(gam_selection))   #output ready table to put in the powerpoint

summary(gam_selection)

dev.new(width=5, height=4)
par(mfrow=c(3,3))
plot(gam_selection,se = T)
#png(file = "gam.png", width = 600,
 #   height = 600)

# ACF for GAM model

pred_gam <- predict.Gam(gam_selection,newdata = df_train_tree);

dev.new(width=5, height=4)
par(mfrow=c(1,1))
plot(pred_gam,xaxt = 'n',xlab = '')
lines(predict(gam_selection,newdata = df_train_tree),col= 'red')
lines(df_train_tree$AVG_WAIT_DURATION,col= 'black')
axis(side = 1, at = seq(1,nrow(df_train_tree),5),
     labels = 
       as.character(modelDf_no_0$DATE)[seq(1,nrow(df_train_tree),5)],
     las = 2,cex.axis=.75)
legend(1, 95, legend=c("prediction", "actual"),
       col=c("red", "black"), lty=1:2, cex=0.8)


dev.new(width=5, height=4)
par(mfrow=c(1,2))
Acf(gam_selection$residuals,main = 'Acf Gam residuals')   ### ACF and
Pacf(gam_selection$residuals,main = 'Pacf Gam residuals,no pad')  ### PACF without zeros

dwtest(gam_selection,data = df_train_tree)
# ACF with zero padding

df_train_tree.0 <- modelDf;

df_train_tree.0$HOUR <- as.factor(df_train_tree.0$HOUR)
df_train_tree.0$WEEKDAY <- as.factor(df_train_tree.0$WEEKDAY)
df_train_tree.0$MONTH <- as.factor(df_train_tree.0$MONTH)
df_train_tree.0$HOURS_OPEN <- as.factor(df_train_tree.0$HOURS_OPEN)
#df_train_tree$IS_OPEN_DAYS <- as.factor(df_train_tree$IS_OPEN_DAYS)
df_train_tree.0$WEEK_NUMBER <- as.factor(df_train_tree.0$WEEK_NUMBER)
df_train_tree.0$ONLINE_ONLY <- as.factor(df_train_tree.0$ONLINE_ONLY)
#df_train_tree$BAD_DAYS <- as.factor(df_train_tree$BAD_DAYS)
#df_train_tree$POWER_OUT <- as.factor(df_train_tree$POWER_OUT)
#df_train_tree$RAIN <- as.factor(df_train_tree$RAIN)
#df_train_tree$BOGO <- as.factor(df_train_tree$BOGO)
#df_train_tree$NUM_ATTRACTIONS <- as.factor(df_train_tree$NUM_ATTRACTIONS)
df_train_tree.0$WEEKDAY <- as.factor(df_train_tree.0$WEEKDAY)

mod_gam_0 <- gam(df_train_tree.0$AVG_WAIT_DURATION ~HOUR+WEEKDAY+
                   MONTH+WEEK_NUMBER+s(ONLINE_SALES_DAY_BEFORE,df = 3)+
                   s(PEOPLE_IN_LINE_LAG,df = 3)+s(AVG_WAIT_PER_STEP_LAG,df = 3)+
                   s(TICKET_COUNT_LAG,df  = 3),
                 data = df_train_tree.0)

sc <- gam.scope(df_train_tree.0,
                arg = c('df=2','df = 3','df=4'));
gam_selection <- step.Gam(mod_gam_0,scope  = sc,direction = 'backward');
# problem with new levels of
# week number, had to refit it

dev.new(width=5, height=4)
par(mfrow=c(3,3))
plot(gam_selection,se = T)

dev.new(width=5, height=4)
par(mfrow=c(1,2))
Acf(gam_selection$residuals,main = 'Acf Gam residuals')   ### ACF and
Pacf(gam_selection$residuals,main = 'Pacf Gam residuals')  ### PACF with zeros


# doing the incremental testing GAM non padded data

foldDates = unique(modelDf[modelDf_no_0$AVG_WAIT_DURATION != 0 & 
                             modelDf_no_0$YEAR == 2021,]$DATE)
gam.results.list = list()
i = 1

for (foldDate in foldDates){
  print(foldDate)
  test = df_train_tree[modelDf_no_0$DATE>=foldDate,]
  train = df_train_tree[modelDf_no_0$DATE<foldDate,]
  
  gam.results = gam.cv(train,test)
  gam.results.list[[i]] = gam.results
  
  i = i+1
}  # around 18931 accumulated in all the year for the padded

gamResultsDf = process.results(gam.results.list)  # had to delete the returned predictions on the cv function

gamResultsDf



#############################################################
# Gradient boosting
#############################################################
library(gbm)

df_train_tree <- modelDf_no_0[,-c(1,2,4,5,7,8,11,13,26,27)];
df_train_tree  <- df_train_tree[,-c(3,4)]
df_train_tree  <- df_train_tree[,-c(3,14)]
df_train_tree  <- df_train_tree[,-c(6,9,10,11,12)]

df_train_tree$HOUR <- as.factor(df_train_tree$HOUR)
df_train_tree$WEEKDAY <- as.factor(df_train_tree$WEEKDAY)
df_train_tree$MONTH <- as.factor(df_train_tree$MONTH)
df_train_tree$HOURS_OPEN <- as.factor(df_train_tree$HOURS_OPEN)
#df_train_tree$IS_OPEN_DAYS <- as.factor(df_train_tree$IS_OPEN_DAYS)
df_train_tree$WEEK_NUMBER <- as.factor(df_train_tree$WEEK_NUMBER)
df_train_tree$ONLINE_ONLY <- as.factor(df_train_tree$ONLINE_ONLY)
#df_train_tree$BAD_DAYS <- as.factor(df_train_tree$BAD_DAYS)
#df_train_tree$POWER_OUT <- as.factor(df_train_tree$POWER_OUT)
#df_train_tree$RAIN <- as.factor(df_train_tree$RAIN)
#df_train_tree$BOGO <- as.factor(df_train_tree$BOGO)
#df_train_tree$NUM_ATTRACTIONS <- as.factor(df_train_tree$NUM_ATTRACTIONS)
df_train_tree$WEEKDAY <- as.factor(df_train_tree$WEEKDAY)


grad_boost <- gbm(df_train_tree$AVG_WAIT_DURATION~.,
                  data = df_train_tree,shrinkage = 0.1);

summary_gbm  <- summary(grad_boost);
var_names_ordered <- rev(summary_gbm$var)

par(mfrow = c(1,1))
dev.new(width=4, height=2)
summary(grad_boost,yaxt = 'n')
axis(side = 2, at = 1:length(var_names_ordered),
     labels = var_names_ordered,
     las = 2,cex.axis=.5)   




#install.packages('plotmo')
library(plotmo)
# plot gbm effects
dev.new(width=5, height=4)
par(mfrow=c(3,4))
plotmo(grad_boost, pmethod="partdep", all1=TRUE,persp.ticktype="detailed",
       data = df_train_tree,main = summary(grad_boost)$vars)



#plot fit on non padded data
dev.new(width=5, height=4)
plot(predict(grad_boost,newdata = df_train_tree),xaxt = 'n',
     xlab = NULL,main = ' ',ylab = 'AVG_WAIT_TIME' )
lines(predict(grad_boost,newdata = df_train_tree),col = 'red')
lines(df_train_tree$AVG_WAIT_DURATION,col = 'black')
axis(side = 1, at = seq(1,nrow(df_train_tree),5),
     labels = 
       as.character(modelDf_no_0$DATE)[seq(1,nrow(df_train_tree),5)],
     las = 2,cex.axis=.75)
legend(120, 80, legend=c("prediction", "actual"),
       col=c("red", "black"), lty=1:2, cex=0.8)

# plot acf and pacf on non padded data
dev.new(width=5, height=4)
par(mfrow=c(1,2),mar=c(5,6,4,1)+1)
Acf(predict(grad_boost,newdata = df_train_tree),main = 'ACF gradient boosting residuals,no pad')
Pacf(predict(grad_boost,newdata = df_train_tree),main = 'PACF gradient boosting residuals')




df_train_tree.0 <- modelDf[,-c(1,2,4,5,7,8,11,13,26,27)];
df_train_tree.0  <- df_train_tree.0[,-c(3,4)]
df_train_tree.0  <- df_train_tree.0[,-c(3,14)]
df_train_tree.0  <- df_train_tree.0[,-c(6,9,10,11,12)]

df_train_tree.0$HOUR <- as.factor(df_train_tree.0$HOUR)
df_train_tree.0$WEEKDAY <- as.factor(df_train_tree.0$WEEKDAY)
df_train_tree.0$MONTH <- as.factor(df_train_tree.0$MONTH)
df_train_tree.0$HOURS_OPEN <- as.factor(df_train_tree.0$HOURS_OPEN)
#df_train_tree$IS_OPEN_DAYS <- as.factor(df_train_tree$IS_OPEN_DAYS)
df_train_tree.0$WEEK_NUMBER <- as.factor(df_train_tree.0$WEEK_NUMBER)
df_train_tree.0$ONLINE_ONLY <- as.factor(df_train_tree.0$ONLINE_ONLY)
#df_train_tree$BAD_DAYS <- as.factor(df_train_tree$BAD_DAYS)
#df_train_tree$POWER_OUT <- as.factor(df_train_tree$POWER_OUT)
#df_train_tree$RAIN <- as.factor(df_train_tree$RAIN)
#df_train_tree$BOGO <- as.factor(df_train_tree$BOGO)
#df_train_tree$NUM_ATTRACTIONS <- as.factor(df_train_tree$NUM_ATTRACTIONS)
df_train_tree.0$WEEKDAY <- as.factor(df_train_tree.0$WEEKDAY)


grad_boost <- gbm(df_train_tree.0$AVG_WAIT_DURATION~.,
                  data = df_train_tree.0,shrinkage = 0.1);


#plot fit on padded data
dev.new(width=5, height=4)
plot(predict(grad_boost,newdata = df_train_tree.0),xaxt = 'n',
     xlab = NULL,main = ' ',ylab = 'AVG_WAIT_TIME' )
lines(predict(grad_boost,newdata = df_train_tree.0),col = 'red')
lines(df_train_tree.0$AVG_WAIT_DURATION,col = 'black')
axis(side = 1, at = seq(1,nrow(modelDf),5),
     labels = 
       as.character(modelDf$DATE)[seq(1,nrow(df_train_tree.0),5)],
     las = 2,cex.axis=.75)
legend(80, 1.5, legend=c("prediction", "actual"),
       col=c("red", "black"), lty=1:2, cex=0.8)

# plot acf and pacf on padded data
dev.new(width=5, height=4)
par(mfrow=c(1,2),mar=c(5,6,4,1)+1)
Acf(predict(grad_boost,newdata = df_train_tree.0),main = 'ACF gradient boosting residuals, pad')
Pacf(predict(grad_boost,newdata = df_train_tree.0),main = 'PACF gradient boosting residuals')
