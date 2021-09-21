library(readxl)
library(ggplot2)
library(ggthemes)
library(e1071)
library(RWeka)
library(randomForest)
pkgs <- c("e1071","naivebayes","dplyr","rpart.plot","rpart",
          "RColorBrewer","party","rattle","ggplot2","ggthemes",
          "randomForest","caret")
package.check <- lapply(pkgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
rr <- read.csv("C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\RR Clean\\Training Data.csv")
rr <- rr[,c(-1,-10)]
rr[,c(2,3,6,7)] <- round(rr[,c(2,3,6,7)])
## 70% of the sample size
smp <- floor(0.70 * nrow(rr))
## set the seed to make your partition reproducible
set.seed(123)
train.smp <- sample(seq_len(nrow(rr)), size = smp)
# create traning and tesing dataset from sample
train <- rr[train.smp, ]  ; View(train); (length(train$Total_Pop))
test <- rr[-train.smp, ] ; View(test); (length(test$Total_Pop))
####################
pairs(rr,col=c('red','blue','green'))
# plot the primary factors Total Population
plot(rr$Total_Pop,rr$ARNG_Accessions,col = c('red','blue'))
cor(rr$Total_Pop,rr$ARNG_Accessions) # 0.91 Positive very close to 1 correlation
#############################################
lm.all <- lm(ARNG_Accessions ~., data = rr[,c(-4,-3,-2,-5)])
summary(lm.all)
#############################################
# linear regression for single factors
rr.lmp <- lm(ARNG_Accessions ~ Total_Pop + Avg_Unemployed + Total_Vet_Pop, data = rr)
abline(rr.lmp)
summary(rr.lmp)
plot(rr.lmp)
####################
# plot the primary factors Unemployment
plot(rr$Avg_Unemployed, rr$ARNG_Accessions,col = c('red','blue'))
cor(rr$Avg_Unemployed, rr$ARNG_Accessions) # 0.89 Positive very close to 1 correlation
# linear regression for single factors
rr.lmu <- lm(ARNG_Accessions ~ Avg_Unemployed, data = rr)
abline(rr.lmu,col = 'green')
summary(rr.lmu)
####################
# linear regressions with those two factors
rr.lm <- lm(ARNG_Accessions ~ Avg_Unemployed + Total_Pop, data = rr)
plot(rr.lm)
abline(rr.lm)
summary(rr.lm)
names(rr.lm)
###################
# fitted values for xs, plot fitted v unemployed and pop
rr.lm$fitted.values
fitted(rr.lm)
plot(rr$Total_Pop,rr.lm$fitted.values,col = 'blue')
par(new = TRUE)
plot(rr$Avg_Unemployed,rr.lm$fitted.values,col = 'red',add = TRUE)
##################
rr.lm$coefficients
abline(-1.2796851583, 0.0037068241)
abline(-1.2796851583, 0.0001402594)
##################
# calc manually
y_hat <- function(intercept, slope,expected_val){
  y <- intercept + slope * expected_val
  return(y)
}
# percentage difference calculator
# change from x to y = x-y divided by mean(x+Y) = x+y/2
delta <- function(x,y){
  d <- (y - x)/ x *100 
  return(d)
}
# grab coeffciant
###################
# sample test 1 value.... predict( model, list(val1 = x, val2 = x)) also.. list/data.frame(val1= c(,,))
# predict
p.all <- predict(rr.lm, list(Avg_Unemployed = test$Avg_Unemployed,
                             Total_Pop      = test$Total_Pop),
                             interval = 'confidence', # what is mean confidence of value
                             level = 0.95)
p.allp <- predict(rr.lm, list(Avg_Unemployed = test$Avg_Unemployed,
                             Total_Pop      = test$Total_Pop),
                             interval = 'prediction', # likelihood of predictive value
                             level = 0.95)

# predictive interval is wider then the confidence interval
# extrapolutoin is using the mkodel to make prediction outside the range of the input data
###################
summary(p.all)
plot(p.all)
all.Table <- data.frame('test' = test$ARNG_Accessions,
                        'predict' = round(p.all[,1]),
                        'diff_raw' = test$ARNG_Accessions - round(p.all[,1]),
                        'diff_pct' = delta(test$ARNG_Accessions,round(p.all[,1])))
all.Table[all.Table$diff_pct==Inf,4] <- 100 
View(all.Table)
###################
summary(p.allp)
plot(p.allp)
allp.Table <- data.frame('test' = test$ARNG_Accessions,
                         'predict' = round(p.allp[,1]),
                         'diff_raw' = test$ARNG_Accessions - round(p.allp[,1]),
                         'diff_pct' = delta(test$ARNG_Accessions,round(p.allp[,1])))
allp.Table[allp.Table$diff_pct==Inf,4] <- 100
View(allp.Table)
###################
mean(allp.Table$diff_raw)
mean(allp.Table$diff_pct)
##################
plot(all.Table$predict,col = 'blue')
par(new = TRUE)
plot(all.Table$test,col = 'red',main = 'pred.blue & test.Red')
##################
plot(density(all.Table$predict),col = 'blue')
par(new = TRUE)
plot(density(all.Table$test),col = 'red')
##################
View(train);View(test)
#################################################################################################################
#                                                                                                               #
#################################################################################################################
#                                                                                                               #
#################################################################################################################
#                                                                                                               #
#################################################################################################################
# read new demographic data
Dem.1722 <- read.csv("C:\\Users\\RandolphT\\Desktop\\Terrance Documents\\R & R Data\\Step 1 Analysis\\Demographics_2017_2022.csv")
colnames(Dem.1722)
n.col <- c()
#################################################################################################################
#                                                                                                               #
#################################################################################################################
#                                                                                                               #
#################################################################################################################
#                                                                                                               #
#################################################################################################################
library(readxl)
####
tdata <- "C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\RR Clean\\MS_TS.xlsx"
####
y08 <- read_excel(tdata, sheet = 'x2008')
y09 <- read_excel(tdata, sheet = 'x2009')
y10 <- read_excel(tdata, sheet = 'x2010')
y11 <- read_excel(tdata, sheet = 'x2011')
y12 <- read_excel(tdata, sheet = 'x2012')
y15 <- read_excel(tdata, sheet = 'x2015')
y17 <- read_excel(tdata, sheet = 'x2017')
y18 <- read_excel(tdata, sheet = 'x2018')
# Merge by ZIP_Code
ac.time <- Reduce(function(x, y) merge(x, y,by = 'ZIP_Code', all=TRUE), list(y08,y09,y10,y11,y12,y15,y17,y18))
View(head(ac.time))
# replace na's with 0
ac.time[is.na(ac.time)] <- 0 ; ac.time[ac.time == '<Null>'] <- 0
# replace 0 in name to unk
ac.time[ac.time$PO_NAME==0,7] <- 'Unk'
# re-order the data.frame 
ac.time <- ac.time[,c(1,7,2,3,4,5,6,8,9,10)]
# check change and change columns accordingly
colnames(ac.time)
colnames(ac.time) <- c('ZIP_Code','PO_Name','x08','x09','x10','x11','x12','x15','x17','x18')
View(ac.time)
# remove unk with all zero's
ac.time <- ac.time[-c(which(ac.time$PO_Name=='Unk'& ac.time$x08==0)),]
# view the Unk data left
(ac.time[which(ac.time$PO_Name=='Unk'),])
# write.csv(ac.time, file = "C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\RR Clean\\RR_Time.csv")
# create time serise 
a <- colnames(ac.time.sum)
ac.time.sum$Year <- a
ac.time.sum <- data.frame( 'Year'       = c('x08','x09','x10','x11','x12','x15','x17','x18'),
                           'Accessions' = c(sum(ac.time$x08),sum(ac.time$x09),sum(ac.time$x10), 
                                            sum(ac.time$x11),sum(ac.time$x12), sum(as.numeric(ac.time$x15)),
                                            sum(ac.time$x17),sum(ac.time$x18)))

View(ac.time.sum)
# write.csv(ac.time.sum, file = "C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\RR Clean\\RR_Time_sum.csv")
############################################
#############################################
#############################################

# linear regression for ARNG_Accessions ~ Total_Pop + Avg_Unemployed + Total_Vet_Pop
rr.pop <- lm(ARNG_Accessions ~ Total_Pop + Avg_Unemployed + Total_Vet_Pop, data = train)
# plot(rr.pop)
# abline(rr.pop)
summary(rr.pop)
rpred <- predict(rr.pop,test[,-8])
reg.pred <- data.frame(rpred,test$ARNG_Accessions)
colnames(reg.pred) <- c('pred','test')
####################
# linear regression for ARNG_Accessions ~ Avg_Unemployed + Total_Vet_Pop
rr.rf <- lm(ARNG_Accessions ~ Total_Pop + 
                              HS_higher + QMA + 
                              Avg_Unemployed + Total_Vet_Pop,
                              data = train)
# plot(rr.rf)
# abline(rr.rf)
summary(rr.rf)
rpred <- predict(rr.rf,test[,-8])
reg.pred <- data.frame(rpred,test$ARNG_Accessions)
colnames(reg.pred) <- c('pred','test')
####################
# linear regression for ARNG_Accessions ~ Avg_Unemployed + Total_Vet_Pop
rr.vet <- lm(ARNG_Accessions ~ Avg_Unemployed + Total_Vet_Pop, data = rr)
plot(rr.vet)
abline(rr.vet)
summary(rr.vet)
####################
# predictions
pred.pop <- predict(rr.pop,test)
pop <- data.frame(round(pred.pop),test$ARNG_Accessions)
colnames(pop) <- c('pred','test')
###
pred.vet <- predict(rr.vet,test)
vet <- data.frame(round(pred.vet),test$ARNG_Accessions)
colnames(vet) <- c('pred','test')
###
pred.rf <- predict(rr.rf,test)
lrf <- data.frame(round(pred.rf),test$ARNG_Accessions)
colnames(lrf) <- c('pred','test')
#####################
ggplot(test, aes(x = Total_Pop, y = pop$test)) +
  geom_segment(aes(xend = Total_Pop, yend = pop$pred)) +
  geom_point(shape = 2,color = 'blue') +
  geom_point(aes(y = pop$pred),color = 'red')
#####
ggplot(test, aes(x = Total_Pop, y = vet$test)) +
  geom_segment(aes(xend = Total_Pop, yend = vet$pred)) +
  geom_point(shape = 2,color = 'blue') +
  geom_point(aes(y = vet$pred),color = 'red')
#####
ggplot(test, aes(x = Total_Pop, y = lrf$test)) +
  geom_segment(aes(xend = Total_Pop, yend = lrf$pred)) +
  geom_point(shape = 2,color = 'blue') +
  geom_point(aes(y = lrf$pred),color = 'red')
############################################################
# Random Forest
###############
library(RWeka)
WOW("weka/classifiers/trees/RandomForest")
rf <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
# build default model with 100 trees
# build a model with 10 trees instead
rf_model <- rf(ARNG_Accessions ~., data=train, control=Weka_control(I=30))
eMod <- evaluate_Weka_classifier(rf_model, numFolds = 3, seed = 1, class = TRUE) ;eMod
###############
pred=predict(rf_model, newdata = test, type = c("class"))
newpred <- data.frame(test$ARNG_Accessions, round(pred))
colnames(newpred)=c("test", "pred")
###############
ggplot(test, aes(x = Total_Pop, y = newpred$test)) +
  geom_segment(aes(xend = Total_Pop, yend = newpred$pred)) +
  geom_point(shape = 2,color = 'blue') +
  geom_point(aes(y = newpred$pred),color = 'red')
# ##############
# 
# rf <- randomForest(ARNG_Accessions~.,train)
# (rf)
# plot(rf)# plot information
# # check attributs that we can check
# (attributes(rf))
# ######
# # ploting nodes
# hist(treesize(rf),main = 'Random Forest model', col= 'light blue')
# # plot variable importance
# varImpPlot(rf) # how worse model will get without each var (gini)
# # var importance qantitativly
# importance(rf)
# # which vars was acctually used in random forest
# varUsed(rf) 
##########################################################################################################
# load data
library(readxl)
library(randomForest)
####
rrdata <- read.csv("C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\RR Clean\\Training Data.csv")
rrdata <- rrdata[,c(-1,-10)]
rrdata <- cbind(round(rrdata[,c(2,3,6,7)]), rrdata[,c(1,4,5,8)])
rr <- rrdata[,c(4,5,7,8)]
##############
# create train & testing set

smp <- floor(0.80 * nrow(rr))
## set the seed to make your partition reproducible
set.seed(1234)
train.smp <- sample(seq_len(nrow(rr)), size = smp)
# create traning and tesing dataset from sample
train <- rr[train.smp, ]
test <- rr[-train.smp,]

##############
rf <- randomForest(ARNG_Accessions ~. ,train, ntree=1500)
###
pred.smp <- predict(rf, test[,-4], type=c("class"))
# CM <- confusionMatrix(pred.smp,test$Accessions)
newpred <- data.frame(round(pred.smp),test$ARNG_Accessions)
colnames(newpred) <- c('pred','test')
cat('predict sum  ', sum(round(pred.smp)),'\n',
    'Actual sum  ', sum(test$ARNG_Accessions),'\n', 'differance is : ',
    sum(round(pred.smp)) - sum(test$ARNG_Accessions),' Accessions' )

View(newpred)
# CM
# check accuracy... 0.915
cat('   Random Forest   ','test accuracy  ',CM$overall['Accuracy'])
###############
pred.rfa=predict(rfa, newdata = test, type = c("class"))
newpred.rfa <- data.frame(test$ARNG_Accessions, round(pred))
colnames(newpred.rfa)=c("test", "pred")
###############
############################################################################################
# Random Forest
###############
library(RWeka)
WOW("weka/classifiers/trees/RandomForest")
rf <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
# build default model with 100 trees
# build a model with 10 trees instead
rf_model <- rf(ARNG_Accessions ~., data=train, control=Weka_control(I=30))
eMod <- evaluate_Weka_classifier(rf_model, numFolds = 3, seed = 1, class = TRUE) ;eMod
###############
pred=predict(rf_model, newdata = test[,-5], type = c("class"))
newpred <- data.frame(test$ARNG_Accessions, round(pred))
colnames(newpred)=c("test", "pred")

cat('predict sum  ', sum(round(pred)),'\n',
    'Actual sum  ', sum(test$ARNG.Accessions),'\n', 'differance is : ',
    sum(round(pred)) - sum(test$ARNG.Accessions),' Accessions' )
############################################################################################
# ggplot(test, aes(x = Total_Pop, y = newpred$test)) +
#   geom_segment(aes(xend = Total_Pop, yend = newpred$pred)) +
#   geom_point(shape = 2,color = 'blue') +
#   geom_point(aes(y = newpred$pred),color = 'red')
##############
##############
# ggplot(test, aes(x = Total_Pop, y = newpred.rfa$test)) +
#   geom_segment(aes(xend = Total_Pop, yend = newpred.rfa$pred)) +
#   geom_point(shape = 2,color = 'blue') +
#   geom_point(aes(y = newpred.rfa$pred),color = 'red')+
#   ggtitle("RF Predictions (Red is prediction)") +
#   xlab("Total Pop") + ylab("Accessions")
#################

# 
# delta <- function(x,y){
#   d <- (y - x)/ x *100 
#   return(d)
# }
# newpred.rfa$diff <- round(delta(newpred.rfa$pred,newpred.rfa$test))
# a <- pop[pop$diff >=0,]; mean(a$diff)
# b <- pop[pop$diff <=0,]; mean(b$diff)
# 
# sum(pop$pred)
# sum(pop$test)





library(purrr)
library(readxl)
library(XLConnect)
file <- 'C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\2017 Zip Data.xlsx'
a <- read_excel(file, sheet = 'Soldier ETS Count_Unit')
b <- read_excel(file, sheet = 'Attrition_Unit')
c <- read_excel(file, sheet = 'Market Share_ZIP')
d <- read_excel(file, sheet = 'Educational Attainment_ZIP')
e <- read_excel(file, sheet = '% Applicants Accessed_ZIP')
f <- read_excel(file, sheet = 'ARNG Soldier HOR_ZIP')
g <- read_excel(file, sheet = 'Race-Ethnicity_ZIP')
h <- read_excel(file, sheet = 'Population_ZIP')
i <- read_excel(file, sheet = 'QMA_ZIP')
j <- read_excel(file, sheet = 'Gender_ZIP')
k <- read_excel(file, sheet = 'Diversity Index_ZIP')
l <- read_excel(file, sheet = 'Unemployment_ZIP')
m <- read_excel(file, sheet = 'Income_ZIP')


# Merge by ZIP_Code
data.zip <- Reduce(function(x, y) merge(x, y,by = 'ZIP Code', all=TRUE), list(c,d,e,f,g,h,i,j,k,l,m))
View(data.zip)
colnames(data.zip)
# ages 15 - 24
newName <- c( "ZIP Code","Location","ARNG Accessions","HS diploma or higher","ARNG Applicants Accessed","ARNG Soldiers Count",          
  "Total Pop (15-24)","White Pop (15-24)","Black Pop (15-24)","Asian Pop (15-24)","American Indian Pop (15-24)",
  "Pacific Islander Pop (15-24)","Other Race Pop (15-24)","Race 2plus Pop (15-24)","Hispanic Population (15-24)",
  "Total Pop","Military Age Pop","QMA","Male Military Age Pop","Female Military Age Pop","Diversity Index",
  "Unemployment Pop","Unemployment Rate","Median Household Income","Per Capita Income") 
colnames(data.zip) <- newName
data.zip <- na.omit(data.zip, cols = data.zip$`ARNG Accessions`)
sum(is.na(data.zip))
data.zip <- data.frame(data.zip)
data.zip <- data.zip[,c(-1,-2,-22,-25)]
data.zip <- round(data.zip)
data.zip <- data.zip[,-16]
######################################################################
data.zip <- data.zip[,c(-3,-4)]
smp <- floor(0.80 * nrow(data.zip))
## set the seed to make your partition reproducible
set.seed(1234)
train.smp <- sample(seq_len(nrow(data.zip)), size = smp)
# create traning and tesing dataset from sample
train <- data.zip[train.smp, ]
test <- data.zip[-train.smp,]

##############
rf <- randomForest(ARNG.Accessions ~. ,train, ntree=1200)
###
pred.smp <- predict(rf, test[,-1], type=c("class"))
newpred <- data.frame(test$ARNG.Accessions,round(pred.smp))
colnames(newpred) <- c('test','pred')
cat('predict sum  ', sum(round(pred.smp)),'\n',
    'Actual sum  ', sum(test$ARNG.Accessions),'\n', 'differance is : ',
    sum(round(pred.smp)) - sum(test$ARNG.Accessions),' Accessions' )

View(newpred)
plot(rf)# plot information
# check attributs that we can check
(attributes(rf))
######
# ploting nodes
hist(treesize(rf),main = 'Random Forest model', col= 'light blue')
# plot variable importance
varImpPlot(rf,main = 'Random Forest model') # how worse model will get without each var (gini)
# var importance qantitativly
importance(rf)
# which vars was acctually used in random forest
varUsed(rf) #how offten they appreard in random forest
# partial Dependence plot, choose model, data, var,reponse var
#      you will be able to view on graph where it falls conpared to y
partialPlot(rf, train,Hours,'high')
?partialPlot
#################################################################################################
gn <- function(data,column)
{
  idx <- which(colnames(data)==column)
  return(idx)
}

data.zip <- read.csv("C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\Step 1 Analysis\\Market by Zip Code 2017_2022.csv")

###
data.zip$x17.Male.Age.17_24 <- data.zip[,gn(data.zip,"X2017.Males.Age.17")]+ 
                               data.zip[,gn(data.zip,"X2017.Males.Age.18")]+
                               data.zip[,gn(data.zip,"X2017.Males.Age.19")]+
                               data.zip[,gn(data.zip,"X2017.Males.Age.20")]+                       
                               data.zip[,gn(data.zip,"X2017.Males.Age.21")]+
                               data.zip[,gn(data.zip,"X2017.Males.Age.22")]+
                               data.zip[,gn(data.zip,"X2017.Males.Age.23")]+
                               data.zip[,gn(data.zip,"X2017.Males.Age.24")]

data.zip$x17.Female.Age.17_24 <- data.zip[,gn(data.zip,"X2017.Females.Age.17")]+
                                 data.zip[,gn(data.zip,"X2017.Females.Age.18")]+
                                 data.zip[,gn(data.zip,"X2017.Females.Age.19")]+
                                 data.zip[,gn(data.zip,"X2017.Females.Age.20")]+                     
                                 data.zip[,gn(data.zip,"X2017.Females.Age.21")]+
                                 data.zip[,gn(data.zip,"X2017.Females.Age.22")]+
                                 data.zip[,gn(data.zip,"X2017.Females.Age.23")]+
                                 data.zip[,gn(data.zip,"X2017.Females.Age.24")]

##################################################################
data.zip$x22.Male.Age.17_24 <- data.zip[,gn(data.zip,"X2022.Males.Age.17")]+
                               data.zip[,gn(data.zip,"X2022.Males.Age.18")]+
                               data.zip[,gn(data.zip,"X2022.Males.Age.19")]+
                               data.zip[,gn(data.zip,"X2022.Males.Age.20")]+                       
                               data.zip[,gn(data.zip,"X2022.Males.Age.21")]+
                               data.zip[,gn(data.zip,"X2022.Males.Age.22")]+
                               data.zip[,gn(data.zip,"X2022.Males.Age.23")]+
                               data.zip[,gn(data.zip,"X2022.Males.Age.24")]


data.zip$x22.Female.Age.17_24 <- data.zip[,gn(data.zip,"X2022.Females.Age.17")]+
                                 data.zip[,gn(data.zip,"X2022.Females.Age.18")]+
                                 data.zip[,gn(data.zip,"X2022.Females.Age.19")]+
                                 data.zip[,gn(data.zip,"X2022.Females.Age.20")]+                     
                                 data.zip[,gn(data.zip,"X2022.Females.Age.21")]+
                                 data.zip[,gn(data.zip,"X2022.Females.Age.22")]+
                                 data.zip[,gn(data.zip,"X2022.Females.Age.23")]+
                                 data.zip[,gn(data.zip,"X2022.Females.Age.24")]
##################################################################
data.zip$x17.Population.Age.17_24 <- data.zip[,gn(data.zip,"X2017.Population.Age.17")]+
                                     data.zip[,gn(data.zip,"X2017.Population.Age.18")]+
                                     data.zip[,gn(data.zip,"X2017.Population.Age.19")]+
                                     data.zip[,gn(data.zip,"X2017.Population.Age.20")]+
                                     data.zip[,gn(data.zip,"X2017.Population.Age.21")]+ 
                                     data.zip[,gn(data.zip,"X2017.Population.Age.22")]+
                                     data.zip[,gn(data.zip,"X2017.Population.Age.23")]+
                                     data.zip[,gn(data.zip,"X2017.Population.Age.24")]

data.zip$x22.Population.Age.17_24 <- data.zip[,gn(data.zip,"X2022.Population.Age.17")]+
                                     data.zip[,gn(data.zip,"X2022.Population.Age.18")]+
                                     data.zip[,gn(data.zip,"X2022.Population.Age.19")]+
                                     data.zip[,gn(data.zip,"X2022.Population.Age.20")]+
                                     data.zip[,gn(data.zip,"X2022.Population.Age.21")]+
                                     data.zip[,gn(data.zip,"X2022.Population.Age.22")]+
                                     data.zip[,gn(data.zip,"X2022.Population.Age.23")]+
                                     data.zip[,gn(data.zip,"X2022.Population.Age.24")]
##################################################################
data.zip$x17.Income.34k_below <- data.zip[,gn(data.zip,"X2017.HH.Income...15000")]+
                                 data.zip[,gn(data.zip,"X2017.HH.Income..15000.24999")]+             
                                 data.zip[,gn(data.zip,"X2017.HH.Income..25000.34999")]

data.zip$x17.Income.35k_49K  <- data.zip[,gn(data.zip,"X2017.HH.Income..35000.49999")]

data.zip$x17.Income.50k_75k  <- data.zip[,gn(data.zip,"X2017.HH.Income..50000.74999")]+
                                data.zip[,gn(data.zip,"X2017.HH.Income..75000.99999")]

data.zip$x17.Income.100k_above  <- data.zip[,gn(data.zip,"X2017.HH.Income..100000.149999")]+
                                   data.zip[,gn(data.zip,"X2017.HH.Income..150000.199999")]+           
                                   data.zip[,gn(data.zip,"X2017.HH.Income..200000.")]

##################################################################
data.zip$x22.Income.34k_below <- data.zip[,gn(data.zip,"X2022.HH.Income...15000")]+
                                 data.zip[,gn(data.zip,"X2022.HH.Income..15000.24999")]+             
                                 data.zip[,gn(data.zip,"X2022.HH.Income..25000.34999")]

data.zip$x22.Income.35k_49K  <- data.zip[,gn(data.zip,"X2022.HH.Income..35000.49999")]

data.zip$x22.Income.50k_75k  <- data.zip[,gn(data.zip,"X2022.HH.Income..50000.74999")]+
                                data.zip[,gn(data.zip,"X2022.HH.Income..75000.99999")]

data.zip$x22.Income.100k_above  <- data.zip[,gn(data.zip,"X2022.HH.Income..100000.149999")]+
                                   data.zip[,gn(data.zip,"X2022.HH.Income..150000.199999")]+           
                                   data.zip[,gn(data.zip,"X2022.HH.Income..200000.")]
                                   

##################################################################
data.zip$x17.HS.or.Higher  <- data.zip[,gn(data.zip,"X2017.Education..High.School.Diploma")]+   
                              data.zip[,gn(data.zip,"X2017.Education..GED")]+ 
                              data.zip[,gn(data.zip,"X2017.Education..Some.College.No.Degree")]+
                              data.zip[,gn(data.zip,"X2017.Education..Associate.s.Degree")]+
                              data.zip[,gn(data.zip,"X2017.Education..Bachelor.s.Degree")]
                              
##################################################################
colnames(data.zip)

newdata <- data.zip[,c(3,8,9,52,55,60,61,87:92,102,107,113:127)]

########
zip.17 <- newdata[,c(1:4,15:17,20,22:25)]
zip.22 <- newdata[,c(1,5:14,18:19,21,26:29)]
##################################################################

# 5 etnicity
colnames(zip.17) <- c( "ZIP Code","Male.Population",        
                       "Female.Population","Median.Household.Income",
                       "Diversity.Index","Male.Age.17_24",           
                       "Female.Age.17_24","Population.Age.17_24",     
                       "Income.34k_below","Income.35k_49K",           
                       "Income.50k_75k","Income.100k_above")

zip.17 <- merge(c,zip.17)
zip.17 <- merge(h,zip.17)
zip.17 <- zip.17[,c(-3,-4)]
colnames(zip.17) <- c( "ZIP Code","Total.Population","ARNG.Accessions",
                       "Male.Population","Female.Population",
                       "Median.Household.Income",
                       "Diversity.Index","Male.Age.17_24",           
                       "Female.Age.17_24","Population.Age.17_24",     
                       "Income.34k_below","Income.35k_49K",           
                       "Income.50k_75k","Income.100k_above")
zip.22 <- zip.22[,c(-5:-9)]
colnames(zip.22) <- c("ZIP Code","Total.Population",       
                      "Male.Population","Female.Population",      
                      "Diversity.Index","Median.Household.Income",
                      "Male.Age.17_24","Female.Age.17_24",         
                      "Population.Age.17_24","Income.34k_below",         
                      "Income.35k_49K","Income.50k_75k",           
                      "Income.100k_above" )


a <- merge(g,zip.17)
b <- merge(a,h)
zip.2017 <- b[,c(-22,-8,-9,-2)]
zip.2017 <- merge(c,zip.2017)
##############################
colnames(zip.22) <- c("ZIP Code","X2022.Total.Population",      
                      "X2022.Male.Population","X2022.Female.Population",      
                      "X2022.Diversity.Index","X2022.Median.Household.Income",
                      "x22.Male.Age.17_24","x22.Female.Age.17_24",         
                      "x22.Population.Age.17_24","x22.Income.34k_below",         
                      "x22.Income.35k_49K","x22.Income.50k_75k",           
                      "x22.Income.100k_above")
colnames(zip.22)
colnames(zip.2017[,c(-2,-13)])
View(merge(zip.2017,data.zip[,c(3,8,9)]))
colnames(zip.2017) <- c("ZIP Code","ARNG Accessions",              
                        "Median.Household.Income","Diversity.Index",        
                        "Male.Age.17_24","Female.Age.17_24",         
                        "Population.Age.17_24","Income.34k_below",         
                        "Income.35k_49K","Income.50k_75k",           
                        "Income.100k_above","Total.Pop")
###

##############################
# write.csv(zip.17,"C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\Step 1 Analysis\\Demo_2017.csv" )
#write.csv(zip.22,"C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\Step 1 Analysis\\Demo_2022.csv"  )

###############
d17 <- read.csv("C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\Step 1 Analysis\\Demo_2017.csv")
d22 <- read.csv("C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\Step 1 Analysis\\Demo_2022.csv")
###############
#remove empty columns
d17 <- d17[,-1] ; d22 <- d22[,-1]
##############
# remove zipcode
d17 <- d17[,-1] 
##############
library(randomForest)
# predictive model
smp <- floor(0.80 * nrow(d17))
## set the seed to make your partition reproducible
set.seed(1234)
train.smp <- sample(seq_len(nrow(d17)), size = smp)
# create traning and tesing dataset from sample
train <- d17[train.smp, ]
test <- d17[-train.smp,]
p.test <- d22
##############
rf <- randomForest(ARNG.Accessions ~. ,train, ntree=1200)
###
pred.smp <- predict(rf, p.test[,-1], type=c("class"))
newpred <- data.frame(p.test,
                      Predict_2022_Accessions_samp17=round(pred.smp))





colnames(newpred) <- c('2017 Accessions','Predict 2022 Accessions samp17')
cat('predict sum  ', sum(round(pred.smp)),'\n',
    'Actual sum  ', sum(test$ARNG.Accessions),'\n', 'differance is : ',
    sum(round(pred.smp)) - sum(test$ARNG.Accessions),' Accessions' )

View(newpred)
plot(rf)# plot information
(attributes(rf))
hist(treesize(rf),main = 'Random Forest model', col= 'light blue')
varImpPlot(rf,main = 'Random Forest model') # how worse model will get without each var (gini)
importance(rf)
##################################
# predict model
pred.smp <- predict(rf, p.test, type=c("class"))
d22$Predicted.Accessions <- round(pred.smp)
#write.csv(newpred,"C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\Step 1 Analysis\\Predict_Accessions_2022.csv")

############################
############################
p22 <- data.frame(read.csv("C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\Step 1 Analysis\\Predict_Accessions_2022.csv")[,-1])
ts  <- data.frame(read.csv("C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\RR Clean\\RR_Time.csv"))
modp22 <- p22[,c(1,14)]
mdata <- merge(ts,modp22)
# mdata$2022.percent
colnames(mdata) <- c( "ZIP_Code","PO_Name","2008","2009","2010","2011","2012","2015","2017","2018","2022" )

df <- data.frame('Year'       = c("2008","2009","2010","2011","2012","2015","2017","2018","2022","2022_18p"),
                 'Accessions' = c(sum(mdata$`2008`),sum(mdata$`2009`),
                                  sum(mdata$`2010`),sum(mdata$`2011`),
                                  sum(mdata$`2012`),sum(mdata$`2015`),
                                  sum(mdata$`2017`),sum(mdata$`2018`),
                                  sum(mdata$`2022`),sum(mdata$`2022`)-(0.18*sum(mdata$`2022`))))
df[,2] <- round(df[,2])
# write.csv(df,"C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\RR Clean\\pred22Timeline.csv")
############################
#p22[,c(1,2,5,6,14)]non_Dis_16_18
abc <- data.frame(read.csv("C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\Step 1 Analysis\\non_Dis_16_18.csv"))


sumData <- data.frame(Pop.17 = sum(abc$x16_Total_Pop),
                      Pop.18 = sum(abc$x18_Total_Pop),
                      Pop.22 = sum(p22$Total.Population),
                      Div.17 = mean(abc$x16_Diversity_Idx),
                      Div.18 = mean(abc$x18_Diversity_Idx),
                      Div.22 = mean(p22$Diversity.Index),
                      MedI.17 = mean(abc$X16_Med_HH_Income),
                      MedI.18 = mean(abc$X18_Med_HH_Income),
                      MedI.22 = mean(p22$Median.Household.Income),
                      Aces.17 = sum(abc$x16_ARNG_Accessions),
                      Aces.18 = sum(abc$x18_ARNG_Accessions),
                      Aces.22 = sum(p22$Predict_2022_Accessions_samp17),
                      Aces.22.18p = sum(p22$Predict_2022_Accessions_samp17)-(0.18*sum(p22$Predict_2022_Accessions_samp17)),
                      Aces.22.28p = sum(p22$Predict_2022_Accessions_samp17)-(0.28*sum(p22$Predict_2022_Accessions_samp17)))
sumData[,c(4:6)] <- round(sumData[,c(4:6)],2)
sumData[,c(7:9)] <- round(sumData[,c(7:9)],0)
sumData[,c(13,14)] <- round(sumData[,c(13,14)],0)
# write.csv(sumData,"C:\\Users\\terra\\Desktop\\MBC 638\\Project\\R&R Project\\RR Clean\\pred22FinalSum.csv")


