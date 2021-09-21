# Final Project IST 707
# Mars Solar Radiation Analysis
# NASA SOLAR PROJECT 


#######################################################
#                                                     #
#                                                     #
#---------------- Library Loading  -------------------#
#                                                     #
#                                                     #
#######################################################
# Predicting solar radiation based on wheater patterns
pkgs <- c("tidytext","readtext","OptimalCutpoints",
          "OptimalCutpoints","tm","wordcloud","ggplot2",
          "ggthemes","slam","CORElearn","rpart","rattle",
          "rpart.plot","RColorBrewer","Cairo","party",
          "Cairo","dplyr","forcats","stringr","magrittr",
          "tidyverse","e1071","mlr","caret","naivebayes",
          "randomForest","readr","lubridate","RWeka",
          "readxl","plotly","chron","CORElearn")

package.check <- lapply(pkgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#######################################################
#                                                     #
#                                                     #
#------------------ Data Cleaning --------------------#
#                                                     #
#                                                     #
#######################################################

ssr = "//SolarPrediction.csv"
solar <- read.csv(ssr)
# view data to understand cleaning chanlenges
View(solar)
# seperate date column into date and time columns
# remove date from Date column, redundency
solar$Data <- gsub(' 12:00:00 AM','',solar$Data)
solar <- solar[,-1]
# rename colnames
colnames(solar) <- c('Date','Time','Radiation','Temperature','Pressure',
                     'Humidity','Wind_Direction_Degrees', 'Wind_Speed',
                     'Time_SunRise','Time_SunSet')
# any na's in data
sum(is.na(solar)) # answer: 0 na's, Great!
cleanSolar <- solar[,c(3:8)]
# view both dirty and clean datasets
View(head(cleanSolar))
#################################################
##############################################################
# converte time from char to datetime... it will add a date that is wrong but time is correct
solar$Time <- chron(times=solar$Time)
# create an hour format from time.. now that structure is datetime and not char
solar$TIME24.h<-as.POSIXct(strptime(solar$Time,format="%H"))
# seperate the correct hours from the bad dates into Hours var
Hours <- format(as.POSIXct(strptime(solar$TIME24.h,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")
# create a hours column in dataset and store correct hours
solar$Hours <- Hours
# remove all unnessary data
solar.agg <- solar[,c(1,3:8,12)]
# aggrogate all data by date & hours 
solar.agg <- aggregate(solar.agg,by=list(solar$Date,solar$Hours),mean)
# rename date and hours columns
colnames(solar.agg)[1] <- 'Date' ; colnames(solar.agg)[2] <- 'Hour' ; solar.agg <- solar.agg[,c(-3,-10)]
# view to ensure things are correct
View(solar.agg)
# Write to a file
#write.csv(solar.agg,file = '//SolarHours.csv')
#################################################################
solar.agg <- read.csv('//SolarHours.csv')
################################################################
# fix hours and check for accuracy
solar.agg$test <- as.character(solar.agg$Hour)
solar.agg$test1 <- gsub(':00:00','',solar.agg$test)
# create time integer for ploting
solar.agg$Hours <- as.integer (solar.agg$test1)
solar.agg <- solar.agg[,c(-9,-10)]
# round var's for cleaner plots
solar.agg[,c(3,4,6,7,8)] <- round(solar.agg[,c(3,4,6,7,8)])
solar.agg[,5] <- round(solar.agg[,5], digits = 1)
# write hours to csv
#write.csv(solar.agg,'//FinalSolarHours.csv')
ss <- read.csv('//FinalSolarHours.csv')
solar <- data.frame(ss)
#View(solar)
#######################################################
#                                                     #
#                                                     #
#---------------- Data Exploration -------------------#
#                                                     #
#                                                     #
#######################################################

#add cardinal wind direction columns based on wind direction degrees 
# specifiy the break paremeters based on cardinal directions 0-360 17 breaks by 22.1
rose_breaks <- c(0, 360/32, (1/32 + (1:15 / 16)) * 360, 360)
# specify lables according to N,S,E,W.. see bottom junk code for more details
rose_labs <- c(1,1,1,3,3,3,2,2,2,2,2,4,4,4,1,1,1)
# discretize for tesing clusters & decision tree no factors just integers
solar$Cardinel_Direction <- cut(solar$Wind_Direction_Degrees,breaks = rose_breaks, labels = rose_labs)
solar$Cardinel_Direction <- as.numeric(solar$Cardinel_Direction)
# Bins for solar radiation - low meduim high very high
solar$SR_Level <- cut(solar$Radiation,breaks = 4, labels = c(1,2,3,4))
solar$SR_Level <- as.numeric(solar$SR_Level)
# Bins for solar Humidity - low meduim high
solar$Humid_dis <- cut(solar$Humidity,breaks = 3, labels = c(1,2,3))
solar$Humid_dis <- as.numeric(solar$Humid_dis)
# Bins for solar Temp - low meduim high 
solar$Temp_dis <- cut(solar$Temperature,breaks = 3, labels = c(1,2,3))
solar$Temp_dis <- as.numeric(solar$Temp_dis)
# Bins for solar Wind speed - low meduim high
solar$WindSpeed_dis <- cut(solar$Wind_Speed,breaks = 3, labels = c(1,2,3))
solar$WindSpeed_dis <- as.numeric(solar$WindSpeed_dis)
# Bins for solar Wind speed - low meduim
solar$Pressure_dis <- cut(solar$Pressure ,breaks = 2, labels = c(0,1))
solar$Pressure_dis  <- as.numeric(solar$Pressure_dis )
# Bins for solar time of day Hours early Moning 0-6, morning 7-12, afternoon 13-18, evening 18-23
solar$HourSplit<- cut(solar$Hours,breaks = 4, labels = c(1,2,3,4))
solar$HourSplit <- as.numeric(solar$HourSplit)
###############
#write.csv(solar[,c(1,10:16)],'//Solar_Discretized.csv')
solar.dis <- data.frame(read.csv('//Solar_Discretized.csv')[,-1])
#View(solar.dis)
#######################################################
#                                                     #
#                                                     #
#-------------- Regression Analysis ------------------#
#                                                     #
#                                                     #
#######################################################

# creating training/testing data
## 70% of the sample size
smp <- floor(0.70 * nrow(solar))
## set the seed to make your partition reproducible
set.seed(1234)
train.smp <- sample(seq_len(nrow(solar)), size = smp)
# create traning and tesing dataset from sample
train <- solar[train.smp, c(3:9)]
test <- solar[-train.smp, c(3:9)]
#################################################
# chi-squared test of independence
#################################################
t <- chisq.test(test$Temperature,test$Radiation)            # Temp
p <- chisq.test(test$Pressure,test$Radiation)               # pressure
hd <- chisq.test(test$Humidity,test$Radiation)               # humidity
d <- chisq.test(test$Wind_Direction_Degrees,test$Radiation) # wind direction
w <- chisq.test(test$Wind_Speed,test$Radiation)             # wind speed
h <- chisq.test(test$Hours,test$Radiation)   # Hours
cat('\n','temperature Chi-squared against Radiation p-vlaue:    ',t$p.value, '\n',
    'pressure Chi-squared against Radiation p-vlaue:       ',p$p.value, '\n',
    'Humidity Chi-squared against Radiation p-vlaue:       ',hd$p.value, '\n',
    'Wind direction Chi-squared against Radiation p-vlaue: ',d$p.value, '\n',
    'wind speed Chi-squared against Radiation p-vlaue:     ',w$p.value, '\n',
    'time of day (hours) Chisquared against Radiation p-vlaue:   ',h$p.value, '\n')
#################################################
# Linear Rgression Model
#################################################
solar.dis <- solar.dis[,-1]
library(caret) # confusion matrix library
smp <- floor(0.70 * nrow(solar.dis))
## set the seed to make your partition reproducible
set.seed(1234)
train.smp <- sample(seq_len(nrow(solar.dis)), size = smp)
# create traning and tesing dataset from sample
train <- solar.dis[train.smp, ]
test <- solar.dis[-train.smp, ]
# Linear Regression
lms <- lm(SR_Level ~., data = train)
summary(lms)
# adjusted var's
lm.adjust <- lm(SR_Level ~ + HourSplit + Temp_dis + Humid_dis, data = train)
summary(lm.adjust )
# predict based on tesing data
pred.lm.1 <- predict(lms, test[,-2], method = 'class')
df1 <- data.frame(round(pred.lm.1),test$SR_Level,test$HourSplit)
colnames(df1) <- c('pred','test','Hours.Aggrogated')
# what is the Accuracy of the regression model
mean(df1$pred == df1$test ) # 53 percent accuracy
###
pred.lm.1 <- predict(lm.adjust, test[,-2], method = 'class')
df1 <- data.frame(round(pred.lm.1),test$SR_Level,test$HourSplit)
colnames(df1) <- c('pred','test','Hours.Aggrogated')
# what is the Accuracy of the regression model
mean(df1$pred == df1$test ) # 44 percent accuracy
###
#write.csv(df1,'//Regression_Pred.csv')
###
ggplot(test, aes(x = Humid_dis, y = df1$test)) +
  geom_segment(aes(xend = Humid_dis, yend = df1$pred)) +
  geom_point(shape = 2,color = 'blue') +
  geom_point(aes(y = df1$pred),color = 'red')
#################################################
# exploratory min/max for discretizing data
cat('\n','temperature min/max difference is: ',
    max(solar$Temperature) - min(solar$Temperature),
    '\n','max/min',max(solar$Temperature), min(solar$Temperature),
    '\n','pressure min/max difference is: ',
    max(solar$Pressure) - min(solar$Pressure),
    '\n','max/min',max(solar$Pressure), min(solar$Pressure),
    '\n','Wind Speed min/max difference is: ',
    max(solar$Wind_Speed) - min(solar$Wind_Speed),
    '\n','max/min',max(solar$Wind_Speed), min(solar$Wind_Speed),
    '\n','Wind Direction min/max difference is: ',
    max(solar$Wind_Direction_Degrees) - min(solar$Wind_Direction_Degrees),
    '\n','max/min',max(solar$Wind_Direction_Degrees), min(solar$Wind_Direction_Degrees),
    '\n','Humidity min/max difference is: ',
    max(solar$Humidity) - min(solar$Humidity),
    '\n', 'max/min',max(solar$Humidity), min(solar$Humidity))

#######################################################
#                                                     #
#                                                     #
#-------------- Clustering Analysis ------------------#
#                                                     #
#                                                     #
#######################################################

data1 <- data.frame(scale(solar[,3:9]))
sr <- data.frame(scale(solar[,3:9]))
withss <- sapply(1:10,
                 function(k) {
                   kmeans(data1, k, nstart = 50, 
                          iter.max = 15)$tot.withinss})

plot( 1:10, withss, type = "b", pch = 8,col = 'red', frame = FALSE, 
      xlab = "Number of clusters", 
      ylab = "Within Sum of squares")

clust_output <- kmeans(data1, centers = 5)# 1-5 are the best clusters
clust_output$size
(clust_output$centers)
View(clust_output$centers)
plot(clust_output$cluster, col = clust_output$cluster)
###
plotcluster(data1,clust_output$cluster)
pairs(clust_output$centers,col = clust_output$withinss)
###
#write.csv(clust_output$centers,'//Cluster_scaledResults.csv')
#######################################################
#                                                     #
#                                                     #
#------------ Decision Tree Analysis -----------------#
#                                                     #
#                                                     #
#######################################################

dt.solar <- solar.dis
####
## 60% of the sample size
smp_size <- floor(0.80 * nrow(dt.solar))
## set the seed to make your partition reproducible
set.seed(10)
train_ind <- sample(seq_len(nrow(dt.solar)), size = smp_size)
###
train.dt <- dt.solar[train_ind, ]
test.dt <- dt.solar[-train_ind, ]
###
## check it, pressure not discretized b/c 0.37 min/max difference
f.ws =table(dt.solar$WindSpeed_dis)     ;f.ws
f.cd =table(dt.solar$Cardinel_Direction);f.cd
f.sl =table(dt.solar$SR_Level)          ;f.sl
f.hd =table(dt.solar$Humid_dis)         ;f.hd
f.td =table(dt.solar$Temp_dis)          ;f.td
f.ps =table(dt.solar$Pressure_dis)      ;f.ps
###
str(dt.solar) ; sum(is.na(dt.solar)) # all integers ; # no na's
###
fit <- rpart(SR_Level ~ ., data = train.dt, method="class")
summary(fit)
# predictd based on test data
predicted= predict(fit,test.dt, type="class")
###
(head(predicted,n=10))
(head(test.dt, n=10))
###
plot(fit)
fancyRpartPlot(fit)# Great!
# needs pruning
tree.prune <- ctree(SR_Level ~ ., data = train,
                    controls = ctree_control(mincriterion = 0.9,
                                             minsplit = 200))
predicted.prune <- predict(tree.prune,test.dt, type="response")
plot(tree.prune)# Great!
########################################
# view results in table
submit <- data.frame(TempLevel   = test.dt$Temp_dis,
                     HumidLevel  = test.dt$Humid_dis,
                     Hours_Split = test.dt$HourSplit,
                     RadLevel    = round(predicted.prune),
                     Actual_Level= test.dt$SR_Level)
(head(submit, n=10)) #if temp & humidity is high == radiation high
########################################
Method.CORElearn <- CORElearn::attrEval(SR_Level ~ ., data=train.dt,  estimator = "InfGain")
(Method.CORElearn)
Method.CORElearn2 <- CORElearn::attrEval(SR_Level ~ ., data=train.dt,  estimator = "Gini")
(Method.CORElearn2)
Method.CORElearn3 <- CORElearn::attrEval(SR_Level ~ ., data=train.dt,  estimator = "GainRatio")
(Method.CORElearn3)

#######################################################
#                                                     #
#                                                     #
#------------------ Random Forest --------------------#
#                                                     #
#                                                     #
#######################################################

solar.rf <- solar[,c(3:9)]
set.seed(123)
smp <- sample(2,nrow(solar.rf),replace = TRUE,prob = c(0.7,0.3))
train <- solar.rf[smp==1,]
test  <- solar.rf[smp==2,]
#######
set.seed(222)
rf <- randomForest(Radiation ~.,train,mtry = 2)
plot(rf)# plot information
######
# ploting nodes
hist(treesize(rf),main = 'Random Forest model', col= 'light blue')
# plot variable importance
varImpPlot(rf,main = 'Random Forest Variable Importance') # how worse model will get without each var (gini)
# var importance qantitativly
importance(rf)
# which vars was acctually used in random forest
varUsed(rf) 
# partial Dependence plot, choose model, data, var,reponse var
#      you will be able to view on graph where it falls conpared to y
partialPlot(rf, train,Hours,'high')
?partialPlot
# extract info about single tree
getTree(rf, 1, labelVar = TRUE)
######
# change ntree, mtry,importance etc in model for better imporvment
pred.rf <- predict(rf, test[,-1], ntree = 300)
newpred <- data.frame(test$Radiation, round(pred.rf),test$Hours)
colnames(newpred)=c("test", "pred",'Hours.Aggrogated')
#View(newpred)
# what is the Accuracy of the regression model
mean(newpred$pred == newpred$test ) # 0.76 percent accuracy
#confusionMatrix(table(pred.rf,test$Radiation))
plot(p1,col=c('blue','orange','black','red'))
# view where error can be imporved, tree/interation count
plot(rf)
####################################
WOW("weka/classifiers/trees/RandomForest")
rf <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
# build a model with 10 trees instead
rf_model <- rf(SR_Level ~., data=train.dt, control=Weka_control(I=500))
e100 <- evaluate_Weka_classifier(rf_model, numFolds = 3, seed = 1, class = TRUE) #;e100
###
pred=predict(rf_model, newdata = test.dt[,-3], type = c("class"))
newpred <- data.frame(test.dt$SR_Level, round(pred),test.dt$HourSplit)
colnames(newpred)=c("test", "pred",'Hours.Aggrogated')
#View(newpred)
# what is the Accuracy of the regression model
mean(newpred$pred == newpred$test ) # 0.76 percent accuracy
#write.csv(newpred,'//randomForest_pred.csv')


#######################################################
#                                                     #
#                                                     #
#------------------      SVM      --------------------#
#                                                     #
#                                                     #
#######################################################

# set.seed(123)
# smp <- sample(2,nrow(solar.rf),replace = TRUE,prob = c(0.6,0.4))
trainsvm <- train.dt
testsvm  <- test.dt
###########
SVM <- svm(SR_Level~., data=train.dt, 
           kernel="polynomial", cost=500, 
           scale=FALSE)
###########
#View(SVM)
###########
## COnfusion Matrix for training data to check model
pred <- predict(SVM, test.dt, type="class")
(table(pred, test.dt$SR_Level)) 
df.svm <- data.frame(round(pred),test.dt$SR_Level,test.dt$HourSplit)
colnames(df.svm) <- c('pred','test','Hours.Aggrogated')
# what is the Accuracy of the regression model
mean(df.svm$pred == df.svm$test ) # 0.76 percent accuracy
###
#write.csv(df.svm,'//SVM_pred.csv')

###
###########


#######################################################
#                                                     #
#                                                     #
#------------------ Visulizations --------------------#
#                                                     #
#                                                     #
#######################################################

# find relationship to build plots from
scatter.smooth(x=solar.agg$Temperature, y=solar.agg$Radiation,lpars = list(col = "red", lwd = 3, lty = 2)) #good corrolation
scatter.smooth(x=solar.agg$Pressure, y=solar.agg$Radiation,lpars = list(col = "red", lwd = 3, lty = 2))    # ok corrolation
scatter.smooth(x=solar.agg$Humidity, y=solar.agg$Radiation,lpars = list(col = "red", lwd = 3, lty = 2))    # not great
scatter.smooth(x=solar.agg$Wind_Speed, y=solar.agg$Radiation,lpars = list(col = "red", lwd = 3, lty = 2))  # ok corrolation
scatter.smooth(x=solar.agg$Wind_Direction_Degrees, y=solar.agg$Radiation,lpars = list(col = "red", lwd = 3, lty = 2)) # intresting
####
# find pairs with gradiant radiation
scatter.smooth(x=solar.agg$Wind_Speed, y=solar.agg$Temperature,
               lpars = list(col = "red", lwd = 3, lty = 2))
scatter.smooth(x=solar.agg$Pressure, y=solar.agg$Temperature,
               lpars = list(col = "red", lwd = 3, lty = 2))
scatter.smooth(x=solar$Cardinel_Direction, y=solar$Temperature,
               lpars = list(col = "red", lwd = 3, lty = 2))


all.sct <- plot_ly(solar.agg, x = ~Radiation, y = ~Temperature, name = 'Temperature', type = 'scatter') %>%
  add_trace(y = ~Wind_Speed, name = 'Wind_Speed', mode = 'markers',opacity = 0.85) %>%
  add_trace(y = ~Humidity, name = 'Humidity', mode = 'markers',opacity = 0.65)%>%
  add_trace(y = ~Pressure, name = 'Pressure', mode = 'markers',opacity = 0.85)
##################
all.hist <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = ~solar.agg$Temperature, name = 'Temp') %>%
  add_histogram(x = ~solar.agg$Wind_Speed, name = 'wind speed') %>%
  add_histogram(x = ~solar.agg$Humidity, name = 'Humidity') %>%
  layout(barmode = "overlay",
         xaxis = list(title = "Data Spread"), 
         yaxis = list(title = "Density"))
##################
rad.hist <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = ~solar.agg$Radiation, name = 'Radiation') %>%
  layout(barmode = "overlay",
         xaxis = list(title = "Radiation"), 
         yaxis = list(title = "Density"))
##################
wts <- plot_ly(solar.agg, 
               x = ~ Wind_Speed,
               y = ~Temperature, 
               type="scatter",
               mode = "markers", 
               color = ~ Radiation,
               name = '',
               size = ~Radiation) %>%
  layout(title = "",
         xaxis = list(title = "Wind Speed"), 
         yaxis = list(title = "Temperature"))
##################
pts <- plot_ly(solar.agg, 
               x = ~ Pressure,
               y = ~Temperature, 
               type="scatter",
               mode = "markers", 
               name = '',
               color = ~ Radiation, 
               size = ~Radiation) %>%
  layout(title = "",
         xaxis = list(title = "Pressure"), 
         yaxis = list(title = "Temperature"))
########
hts <- plot_ly(solar.agg, 
               x = ~Humidity,
               y = ~Temperature, 
               type="scatter",
               mode = "markers", 
               name = '',
               color = ~ Radiation, 
               size = ~Radiation) %>%
  layout(title = "",
         xaxis = list(title = "Humidity"), 
         yaxis = list(title = "Temperature"))
#######
dts <- plot_ly(solar.agg, 
               x = ~Wind_Direction_Degrees,
               y = ~Temperature, 
               type="scatter",
               mode = "markers", 
               name = '',
               color = ~ Radiation, 
               size = ~Radiation) %>%
  layout(title = "",
         xaxis = list(title = "Wind Direction"), 
         yaxis = list(title = "Temperature"))
#############################################
#############################################
solar$Radiation <-  solar[,round(3)]
a <- plot_ly(solar, 
             x = ~ Hours,
             y = ~ Temperature, 
             type="scatter",
             mode = "markers",
             color = ~ Radiation, 
             size = ~Radiation) %>%
  layout(title = "",
         xaxis = list(title = "Hours"), 
         yaxis = list(title = "Temperature"))
chart_link = api_create(a, filename="Temperature and Daily Radiation Levels")
chart_link
##################
b <- plot_ly(solar, 
             x = ~ Hours,
             y = ~Wind_Speed, 
             type="scatter",
             mode = "markers", 
             color = ~ Radiation, 
             size = ~Radiation) %>%
  layout(title = "",
         xaxis = list(title = "Hours"), 
         yaxis = list(title = "Wind Speed"))
chart_link = api_create(b, filename="Daily Wind Speed")
chart_link
##################
c <- plot_ly(solar, 
             x = ~ Hours,
             y = ~Humidity, 
             type="scatter",
             mode = "markers",
             color = ~ Radiation, 
             size = ~Radiation) %>%
  layout(title = "",
         xaxis = list(title = "Hours"), 
         yaxis = list(title = "Humidity"))
chart_link = api_create(c, filename="Daily Wind Humidity")
chart_link
#################
d <- plot_ly(solar, 
             x = ~ Hours,
             y = ~Pressure, 
             type="scatter",
             mode = "markers",
             color = ~ Radiation, 
             size = ~Radiation) %>%
  layout(title = "",
         xaxis = list(title = "Hours"), 
         yaxis = list(title = "Pressure"))
chart_link = api_create(d, filename="Daily Pressure")
chart_link
##################
e <- plot_ly(solar, 
             x = ~ Hours,
             y = ~Wind_Direction_Degrees, 
             type="scatter",
             mode = "markers",
             color = ~ Radiation, 
             size = ~Radiation) %>%
  layout(title = "",
         xaxis = list(title = "Hours"), 
         yaxis = list(title = "Wind Direction"))
chart_link = api_create(e, filename="Daily Wind Direction")
chart_link
####################
####################
library(plotly)

###
# solar cleaned dataset
sc <- data.frame(read.csv('//FinalSolarHours.csv')[,-1])
colnames(sc)
###
# solar dataset discreatized
sd <- data.frame(read.csv('//Solar_Discretized.csv')[,-1])
colnames(sd)
###
# regression 
reg <- data.frame(read.csv('//Regression_Pred.csv')[,-1])
colnames(reg)
###
# random forest
rf <- data.frame(read.csv('//randomForest_pred.csv')[,-1])
colnames(rf)
###
svm <- data.frame(read.csv('//SVM_pred.csv')[,-1])
colnames(svm)
###
###############
# --- SVM --- #
###############
SVM.A <- plot_ly(svm,alpha = .45) %>%
  add_histogram(x = ~ test,name = 'SR Level') %>%
  add_histogram(x = ~ pred,name = 'Pred SR Level') %>%
  layout(barmode = "overlay",
         title = 'SVM: 76% Accuracy',
         xaxis = list(title = "SR: Low, Med Low, Med, High"), 
         yaxis = list(title = "Density"))
chart_link = api_create(SVM.A, filename="SVM Plots")
chart_link


######################
# --- Regression --- #
######################
RGG <- plot_ly(reg,alpha = .45) %>%
  add_histogram(x = ~ test,name = 'SR Level') %>%
  add_histogram(x = ~ pred,name = 'Pred SR Level') %>%
  layout(barmode = "overlay",
         title = 'Linear Regression: 52% Accuracy',
         xaxis = list(title = "SR: Low, Med Low, Med, High"), 
         yaxis = list(title = "Density"))
chart_link = api_create(RGG, filename="Reggresion Plots")
chart_link


#########################
# --- Random Forest --- #
#########################
RForest <- plot_ly(rf,alpha = .45) %>%
  add_histogram(x = ~ test,name = 'SR Level') %>%
  add_histogram(x = ~ pred,name = 'Pred SR Level') %>%
  layout(barmode = "overlay",
         title = 'Random Forest: 76% Accuracy',
         xaxis = list(title = "SR: Low, Med Low, Med, High"), 
         yaxis = list(title = "Density"))
chart_link = api_create(RForest, filename="RF Plots")
chart_link






