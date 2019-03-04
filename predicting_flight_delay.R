install.packages("chron")
install.packages("sqldf")
install.packages("corrgram")
install.packages("ggplot2")
install.packages("scales")
install.packages("ISLR")
install.packages("caret")
install.packages("mice")
install.packages("dplyr")
install.packages("maps")
install.packages("caTools")

library(ISLR)
library(caret)
library(mice)
library(chron)
library(sqldf)
library(corrgram)
library(ggplot2)
library(scales)
library(dplyr)
library(maps)
library(caTools)
getwd()


#--------------------------------
# Step 1: Loading Flight Data
#--------------------------------
# flights data
Dataset_main= read.csv("flights.csv")
# Airlines data
airlines= read.csv("airlines.csv")
# Airport data
airports= read.csv("airports.csv")

# Use rds file to optimize space by serialization
saveRDS(Dataset_main, "flights.rds")
Dataset_main <- readRDS("flights.rds")

Dataset_bk<-Dataset_main
Dataset <- Dataset_main[sample(1:nrow(Dataset_main), 50000,replace=FALSE),]

dim(Dataset)

#--------------------------------
# Step 2: Data Exploration 
#--------------------------------
# Check for Null values
colSums(is.na(Dataset))

# Data Validation:
#--------------------------------
# 1. To get the unique value, missing count and missing prt, 
# 2. To get total number of rows and size of data 

df.info <- function(x) {
  dat  <- as.character(substitute(x))         ##data frame name
  size <- format(object.size(x), units="Mb")  ##size of dataframe in Mb
  
  ##column information
  column.info <- data.frame( column        = names(sapply(x, class)),
                             class         = sapply(x, class),
                             unique.values = sapply(x, function(y) length(unique(y))),
                             missing.count = colSums(is.na(x)),
                             missing.pct   = round(colSums(is.na(x)) / nrow(x) * 100, 2))
  
  row.names(column.info) <- 1:nrow(column.info)
  
  list(data.frame     = data.frame(name=dat, size=size),
       dimensions     = data.frame(rows=nrow(x), columns=ncol(x)),
       column.details = column.info)
}

# FLIGHTS INFO
df.info(Dataset)
# Interesting Info about flights - 
# 1. 81.75% values are missing for = {AIR_SYSTEM_DELAY, SECURITY_DELAY, AIRLINE_DELAY, LATE_AIRCRAFT_DELAY, WEATHER_DELAY}. 
# 2. 2% values are missing for some other features such as -  DEPARTURE_TIME, DEPARTURE_DELAY, ARRIVAL_TIME etc. There are two things that can be done 
# a. Impute the values using mean (as these are numeric values)
# b. Exclude the values( as the dataset is large enough)

# AIRLINES INFO
df.info(airlines)

# Info about Airports
df.info(airports)
# Interesting Info about Airports and Airlines
# There are 322 airports with 54 states.
# There are 14 flights flying between the routes.

# Ensuring that same airlines are represented in flights and airlines data
airlines

# Creating Airline Codes
airlines_code <- unique(Dataset$AIRLINE)

sort(airlines$IATA_CODE) == sort(unique(Dataset$AIRLINE))

# Data Validation: 
#--------------------------------

# A. Boxplot of delay metrics
boxplot(Dataset[ ,drop[4:length(drop)]], col=4:length(drop), main="Delay Boxplots")


# B. Barplot of delay metrics, 0-90th percentile
p.90 <- function(x){
  p <- seq(0, 0.9, 0.1)  ## we will look at 0 to 90th percentile of positive delays
  quantile(x[x > 0], probs=p, na.rm=T)
}

barplot(p.90(Dataset$LATE_AIRCRAFT_DELAY), 
        main = "Flight Delay Distributions (0-90th Percentile)", 
        xlab = "Percentile", 
        ylab = "Delay (Minutes)",
        xlim = c(0,length(p.90(Dataset$LATE_AIRCRAFT_DELAY))*3.5),
        col  = "dark blue")

barplot(c(rep(0,length(p.90(Dataset$LATE_AIRCRAFT_DELAY))), p.90(Dataset$WEATHER_DELAY)),
        col  = "dark red",
        add  = TRUE)

barplot(c(rep(0,length(p.90(Dataset$LATE_AIRCRAFT_DELAY))*2), p.90(Dataset$AIRLINE_DELAY)),
        col  = "dark green",
        add  = TRUE)

legend(0.5, 95, c("Late Aircraft", "Weather", "Airline"),
       fill=c("dark blue", "dark red", "dark green"), cex=0.65)


# C. Barplot of delay metrics, right tail outliers (90-100th percentile)
right.tail <- function(x){
  p <- seq(0.9, 1, 0.01)
  quantile(x[x > 0], probs=p, na.rm=T) 
}

barplot(right.tail(Dataset$LATE_AIRCRAFT_DELAY), 
        main = "Flight Delay Distributions (90-100th Percentile)", 
        xlab = "Percentile", 
        ylab = "Delay (Minutes)",
        xlim = c(0,length(right.tail(Dataset$LATE_AIRCRAFT_DELAY))*3.5),
        ylim = c(0, max(c(Dataset$LATE_AIRCRAFT_DELAY, Dataset$WEATHER_DELAY, Dataset$AIRLINE_DELAY), na.rm=T)),
        col  = "dark blue")

barplot(c(rep(0,length(right.tail(Dataset$LATE_AIRCRAFT_DELAY))), right.tail(Dataset$WEATHER_DELAY)),
        col  = "dark red",
        add  = TRUE)

barplot(c(rep(0,length(right.tail(Dataset$LATE_AIRCRAFT_DELAY))*2), right.tail(Dataset$AIRLINE_DELAY)),
        col  = "dark green",
        add  = TRUE)

legend(0.5, 1300, c("Late Aircraft", "Weather", "Airline"),
       fill=c("dark blue", "dark red", "dark green"), cex=0.65)


# D. Barplot: Average arrival delay time by airlines

airline.avg.delay <- aggregate(Dataset$ARRIVAL_DELAY, by=list(Dataset$AIRLINE), mean, na.rm=T)
names(airline.avg.delay) <- c("AirlineCode", "Mean.Arrival.Delay")
airline.avg.delay <- merge(airline.avg.delay, airlines, by.x="AirlineCode", by.y="IATA_CODE", all.x=TRUE)

airline.avg.delay <- airline.avg.delay[order(airline.avg.delay$Mean.Arrival.Delay), ]
airline.avg.delay <- airline.avg.delay[ ,c(3,1,2)]

airline.avg.delay

barplot(airline.avg.delay$Mean.Arrival.Delay,
        names.arg=airline.avg.delay$AirlineCode,
        col="dark blue",
        main="Mean Arrival Delay by Airline", 
        xlab="Airline Code",
        ylab="Mean Arrival Delay")


# E. Barplot: Number of airports by state
table(airports$STATE)

top10 <- as.data.frame(table(airports$STATE))
top10 <- top10[order(top10$Freq, decreasing=T), ][1:10, ]
barplot(top10$Freq,
        names.arg = top10$Var1,
        col = "dark blue",
        main = "Number of Airports by State - Top 10",
        xlab = "Frequency",
        ylab = "State",
        horiz=T)

# F: Map Visualizations
install.packages("maps")
library("maps")
map("usa")
title("Airports")
points(airports$LONGITUDE, airports$LATITUDE, col="red", cex=0.75)

map("world")
title("Airports")
points(airports$LONGITUDE, airports$LATITUDE, col="red", cex=0.75)

#-----------------------------
# Step 3: Data  Cleaning
#-----------------------------

# A. Removing Sparse and Redundant features:

# 1. Following features have ~82% missing values hence imputation is not helpful
# "AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY", "LATE_AIRCRAFT_DELAY","WEATHER_DELAY"
# 
# 2. Following features are redundant:
# SCHEDULED_TIME(Planned time amount needed for the flight trip),"ELAPSED_TIME (AIR_TIME+TAXI_IN+TAXI_OUT), "AIR_TIME",
# Note: "ARRIVAL_DELAY" has been removed for first iteration of model building as we do not consider arrival delay but can be included it as feature in later iteration

drop <- c("SCHEDULED_TIME","ELAPSED_TIME","AIR_TIME",
          "AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY",
          "LATE_AIRCRAFT_DELAY","WEATHER_DELAY")

#remove the features
Dataset = Dataset[,!(names(Dataset) %in% drop)]


# B. Substituting Origin and Destination Airports with Respective Regions using left join
install.packages("data.table", dependencies=TRUE)
library(data.table)

# 1. Adding Origin Airport Region
dt1 <- data.table(Dataset, key = "ORIGIN_AIRPORT")
dt2 <- data.table(airports, key = "IATA_CODE") 
Dataset <- dt1[dt2]
names(Dataset)
Dataset<-Dataset[,c(2:24,27)]
names(Dataset)
colnames(Dataset)[24] <- "ORIGIN_REGION"

# 2. Adding Destination Airport Region
dt1 <- data.table(Dataset, key = "DESTINATION_AIRPORT")
dt2 <- data.table(airports, key = "IATA_CODE") 
Dataset <- dt1[dt2]
names(Dataset)
Dataset<-Dataset[,c(1:24,28)]
names(Dataset)
colnames(Dataset)[25] <- "DESTINATION_REGION"


# C.Converting Date and Time variables into datetime format

# Update Col - DEPARTURE_TIME
Dataset$DEPARTURE_TIME <- times( sprintf( "%d:%04d:00", Dataset$DEPARTURE_TIME %/% 100, Dataset$DEPARTURE_TIME %% 100 ))
typeof(Dataset$DEPARTURE_TIME)
# Update Col - ARRIVAL_TIME
Dataset$ARRIVAL_TIME <- times( sprintf( "%d:%04d:00", Dataset$ARRIVAL_TIME %/% 100, Dataset$ARRIVAL_TIME %% 100 ) )
# Update Col - SCHEDULED_ARRIVAL
Dataset$SCHEDULED_ARRIVAL <- times( sprintf( "%d:%04d:00", Dataset$SCHEDULED_ARRIVAL %/% 100, Dataset$SCHEDULED_ARRIVAL %% 100 ) )
# Update Col - SCHEDULED_DEPARTURE
Dataset$SCHEDULED_DEPARTURE <- times( sprintf( "%d:%04d:00", Dataset$SCHEDULED_DEPARTURE %/% 100, Dataset$SCHEDULED_DEPARTURE %% 100 ) )
# New Col - datetime
Dataset$datetime <- as.Date(with(Dataset, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")


# D. Making Schedule Departure as a combination of Date and time of Departure.
Dataset$SCHEDULED_DEPARTURE <- as.POSIXct(paste(Dataset$datetime,Dataset$SCHEDULED_DEPARTURE),format="%Y-%m-%d %H:%M:%S")

# E. Dropping following features as these are insignificant for building the model:
# [YEAR MONTH DAY DAY_OF_WEEK] - New Feature datetime being created
# [FLIGHT_NUMBER, TAIL_NUMBER] - Irrelevant features
# [TAXI_OUT, WHEELS_OFF, DISTANCE,WHEELS_ON,TAXI_IN] - Can be attributed by SCHEDULED_DEPARTURE and SCHEDULED_ARRIVAL
# [DIVERTED,CANCELLED,CANCELLATION_REASON] - Cancellation not being considered
updated_dataset <- subset(Dataset, select=c(1:4,24,25,9,10,11,18,19))

# making a copy to create check-point
flight_dataframe <-updated_dataset

flight_dataframe <- flight_dataframe[,-11]
# listing columns
names(flight_dataframe)

# checking for null columns
ncol(is.na(flight_dataframe))

# checking for null records
colSums(is.na(flight_dataframe))

# F. Finding the missing value for imputation
flight_missingValues <- sapply(flight_dataframe, function(x) sum(is.na(x)))
# Percentage of non-null values
filterpercent <-  (nrow(flight_dataframe)-flight_missingValues)/nrow(flight_dataframe)*100
# Identify the missing records
flight_missing <- rbind(flight_missingValues,filterpercent)
flight_missing <- as.data.frame(t(flight_missing))

# Pre-omission
ncol(flight_dataframe)
nrow(flight_dataframe)

# Removing the missing data as more than 97% records are clean
flight_dataframe <- na.omit(flight_dataframe)

# Post-omission
nrow(flight_dataframe)
names(flight_dataframe)


#-------------------------------------------
# Step 3: Exploratory Analysis
#-------------------------------------------

colnames(airlines) <- c("AIRLINE","AIRLINE NAME")

# Generating Stats for visualization:
Flightstat <-  merge(x = flight_dataframe, y = airlines, by = "AIRLINE", all.x = TRUE)
summary(Flightstat)
FlightMean <- aggregate(Flightstat$DEPARTURE_DELAY, list(Air= Flightstat$AIRLINE), mean)
FlightMax <- aggregate(Flightstat$DEPARTURE_DELAY, list(Air= Flightstat$AIRLINE), max)
FlightMin <- aggregate(Flightstat$DEPARTURE_DELAY, list(Air= Flightstat$AIRLINE), min)
FlightDept_Delay_Count <- aggregate(Flightstat$DEPARTURE_DELAY, list(Air= Flightstat$AIRLINE), sum)
FlightCount <- table(flight_dataframe[,"AIRLINE"])
FlightCount <- as.data.frame(FlightCount)
FlightAggregate <- cbind(FlightDept_Delay_Count, FlightMax, FlightMean, FlightMin,FlightCount)
FlightAggregate <- FlightAggregate[-c(3,5,7,9)]
colnames(FlightAggregate) <- c("AIRLINE","DDCount","Max","Mean","Min","Count")
FlightAggregate$Mean <- round(as.numeric(FlightAggregate$Mean),1)                                                          

# A. PIE chart with mean value
with(FlightAggregate,pie(FlightAggregate$Mean, labels=paste0(as.character(FlightAggregate$AIRLINE), " ", FlightAggregate$Mean), col=rainbow(length(FlightAggregate$Mean)),radius=1))

# B. PIE Chart with Count value
FlightAggregate$AvgCount <- round((FlightAggregate$Count / sum(FlightAggregate$Count)*100),1)

with(FlightAggregate,pie(FlightAggregate$AvgCount, labels=paste0(as.character(FlightAggregate$AIRLINE), " ", FlightAggregate$AvgCount), col=rainbow(length(FlightAggregate$AvgCount)),radius=1))

# C. Categoize airlines according to Departure Delay
flight_dataframe$size <- findInterval(flight_dataframe$DEPARTURE_DELAY, c(-100,5, 15), rightmost.closed = TRUE)

ggplot(data=flight_dataframe, aes(x=AIRLINE,y=size,fill=factor(size)))+
  geom_bar(position="dodge",stat="identity")+
  coord_flip()+
  ggtitle("Categorization of Airlines based on Departure Delay")

# Creating a sample for cleaner visualization
plot_sample <- flight_dataframe[sample(1:nrow(flight_dataframe), 500,replace=FALSE),]

# D: Scatter Plot
install.packages('car')
library(car)
# Note: Edit this, range too wide
scatterplotMatrix(plot_sample)
pairs(plot_sample)

# E: Correlation Matrix
library(corrgram)
corrgram(plot_sample)
# detailed correlation matrix
corrgram(flight_dataframe, lower.panel=panel.shade, upper.panel=panel.pie)

# F: interactions effects - 3D scatter plot of 3 variables
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(flight_dataframe$DEPARTURE_DELAY, flight_dataframe$DEPARTURE_TIME, flight_dataframe$AIRLINES)


#---------------------------
# Step 4: Model Building
#---------------------------

# Encode categorical values
flight.dmodel <- dummyVars( ~ ., data=flight_dataframe, fullRank=T)
flight.d <- as.data.frame(predict(flight.dmodel, flight_dataframe))

# Creating Test and Training Data by splitting data to Train:Test - 70:30
train.index <- sample(nrow(flight_dataframe), nrow(flight_dataframe) * .7) #let keep 80% of data for training
# Create test and training data frames
flight.train <- flight_dataframe[train.index,] 
flight.test <- flight_dataframe[-train.index,]


library("parallel")
# Setting a random Seed 
set.seed(195)
#cl <- makeForkCluster(6)
#clusterSetRNGStream(cl, 5672) #set seed for every member of cluster
##registerDoParallel(cl)

# Settin 5-fold Cross-Validation
ctrl <- trainControl(method = "cv", number=10)

# Validating the test and train data
dim(flight_dataframe)
dim(flight.train)
dim(flight.test)


# BUILDING the MODEL : We are building model using different algorithms

# A: LINEAR REGRESSION MODELS
#------------------------------
flight.lm<- train(DEPARTURE_DELAY ~ ., data= flight.train, method = "lm",trControl=ctrl)
flight.lm

varImp(flight.lm)
R.flight.train.lm <-getTrainPerf(flight.lm)
R.flight.train.lm
summary(flight.lm)


p.flight.lm <- predict(flight.lm, newdata = flight.test)
R.flight.test.lm <- postResample(p.flight.lm, flight.test$DEPARTURE_DELAY)
R.flight.test.lm

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.lm)) 
qqline((p.flight.lm))

# Using Ridge(L1) & Lasso(L2) Regression
install.packages("elasticnet")
library(elasticnet)
# 1. RIDGE Regression
#------------------------
set.seed(195)
flight.ridge <- train(DEPARTURE_DELAY ~ ., preProcess=c("scale"), data= flight.train, method = "ridge", tuneLength=10, trControl=ctrl)
flight.ridge
getTrainPerf(flight.ridge)
plot(flight.ridge,main='Ridge Regression')


p.flight.ridge <- predict(flight.lm, newdata = flight.test)
R.flight.test.ridge <- postResample(p.flight.lm, flight.test$DEPARTURE_DELAY)
R.flight.test.ridge

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.ridge)) 
qqline((p.flight.ridge))

# 2. LASSO Regression
#-----------------------
set.seed(195)
flight.lasso <- train(DEPARTURE_DELAY ~ ., data= flight.train, method = "lasso", tuneLength=10, trControl=ctrl)
flight.lasso
getTrainPerf(flight.lasso)
plot(flight.lasso, main='Lasso Regression')

p.flight.lasso <- predict(flight.lasso, newdata = flight.test)
R.flight.test.lasso <- postResample(p.flight.lasso, flight.test$DEPARTURE_DELAY)
R.flight.test.lasso

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.lasso)) 
qqline((p.flight.lasso))

# Trying glmnet -  combination of ridge and lasso
set.seed(195)
flight.glmnet <- train(DEPARTURE_DELAY ~ .,data= flight.train, method="glmnet", metric="RMSE", trControl=ctrl)
flight.glmnet
varImp(flight.glmnet)
plot(flight.glmnet, main='LM glmnet')

p.flight.glmnet <- predict(flight.glmnet, newdata = flight.test)
R.flight.test.glmnet <- postResample(p.flight.glmnet, flight.test$DEPARTURE_DELAY)
R.flight.test.glmnet

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.glmnet)) 
qqline((p.flight.glmnet))

# Fitting a model that preprocess predictors on each resample fold based on our earlier findings
set.seed(195)
flight.lm.pp <- train(DEPARTURE_DELAY ~ ., data= flight.train, preProcess=c("BoxCox", "scale", "center"), method = "lm", trControl=ctrl)
flight.lm.pp

p.flight.lm.pp <- predict(flight.lm.pp, newdata = flight.test)
R.flight.test.lm.pp <- postResample(p.flight.lm.pp, flight.test$DEPARTURE_DELAY)
R.flight.test.lm.pp

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.lm.pp)) 
qqline((p.flight.lm.pp))

# 3. K-Nearest Neighbor
#----------------------------------
flight.grid <- expand.grid(k=1:10)
flight.knn <- train(DEPARTURE_DELAY ~ ., data= flight.train, method = "knn", tuneGrid=flight.grid, trControl=ctrl)
flight.knn
plot(flight.knn, main='K-Nearest Neighbor')

p.flight.knn <- predict(flight.knn, newdata = flight.test)
R.flight.test.knn <- postResample(p.flight.knn, flight.test$DEPARTURE_DELAY)
R.flight.test.knn

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.knn)) 
qqline((p.flight.knn))


# 4.1. PCR(method = "kernelpls")
#-------------------------------------------
install.packages("pls")
library(pls)
flight.pcr1 <- train(DEPARTURE_DELAY ~ ., data= flight.train, method = "kernelpls", tuneLength=18, trControl=ctrl)
flight.pcr1
plot(flight.pcr1,main='Principal Component Regression 1')

p.flight.pcr1 <- predict(flight.pcr1, newdata = flight.test)
R.flight.test.pc1 <- postResample(p.flight.pcr1, flight.test$DEPARTURE_DELAY)
R.flight.test.pc1

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.pcr1)) 
qqline((p.flight.pcr1))


# 4.2. PCR(method = "pls")
#---------------------------
flight.pcr2 <- train(DEPARTURE_DELAY ~ ., data= flight.train, method = "pls", tuneLength=18, trControl=ctrl)
flight.pcr2
plot(flight.pcr2, main='Principal Component Regression 2')


p.flight.pcr2 <- predict(flight.pcr2, newdata = flight.test)
R.flight.test.pcr2 <- postResample(p.flight.pcr2, flight.test$DEPARTURE_DELAY)
R.flight.test.pcr2

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.pcr2)) 
qqline((p.flight.pcr2))

# 4.3. PCR(method = "pcr")
#---------------------------
flight.pcr3 <- train(DEPARTURE_DELAY ~ ., data= flight.train, method = "pcr", tuneLength=10, trControl=ctrl)
flight.pcr3
plot(flight.pcr3, main='Principal Component Regression 3')

p.flight.pcr3 <- predict(flight.pcr3, newdata = flight.test)
R.flight.test.pcr3 <- postResample(p.flight.pcr3, flight.test$DEPARTURE_DELAY)
R.flight.test.pcr3

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.pcr3)) 
qqline((p.flight.pcr3))

# B. NON-LINEAR REGRESSION MODEL
#--------------------------------

# 1. DECISION TREE
#-------------------
set.seed(44)
flight.dt <- train(DEPARTURE_DELAY ~ ., data=flight.train, method="rpart",tuneLength=4,trControl=ctrl)
flight.dt
plot(flight.dt,main='Decision Tree')


p.flight.dt <- predict(flight.dt, newdata = flight.test)
R.flight.test.dt <- postResample(p.flight.dt, flight.test$DEPARTURE_DELAY)
R.flight.test.dt

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.dt)) 
qqline((p.flight.dt))

#using rpart for regression tree
library(rpart) #faster than tree
install.packages()
library(tree) #has useful functions to use with rpart

#create tree
flight.dt2 <- train(DEPARTURE_DELAY ~ ., data=flight.train, method="rpart",tuneLength=4,trControl=ctrl)
flight.dt2


# 2. BAGGING TREE
#-------------------
set.seed(44)
flight.bt <- train(DEPARTURE_DELAY ~ ., data=flight.train, method="treebag",tuneLength=4, trControl=ctrl)
flight.bt

p.flight.bt <- predict(flight.bt, newdata = flight.test)
R.flight.test.bt <- postResample(p.flight.bt, flight.test$DEPARTURE_DELAY)
R.flight.test.bt

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.bt)) 
qqline((p.flight.bt))

# 3. RANDOM FOREST
#-------------------

cores <- 10 
cl <- makeForkCluster(cores) 
clusterSetRNGStream(cl, 489)
registerDoParallel(cl)

set.seed(489)
flight.rf <- train(DEPARTURE_DELAY ~ ., data=flight.train, method="rf",tuneLength=4,trControl=ctrl)
flight.rf
plot(flight.rf,main='Random Forest')

p.flight.rf <- predict(flight.rf, newdata = flight.test)
R.flight.test.rf <- postResample(p.flight.rf, flight.test$DEPARTURE_DELAY)
R.flight.test.rf

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.rf)) 
qqline((p.flight.rf))


# 4. BOOSTING
#-------------------
set.seed(44)
flight.boost <- train(DEPARTURE_DELAY ~ ., data=flight.train, method="gbm",tuneLength=4,trControl=ctrl)
flight.boost
getTrainPerf(flight.boost)
plot(flight.boost,main="Boosting")

p.flight.boost <- predict(flight.boost, newdata = flight.test)
R.flight.test.boost <- postResample(p.flight.boost, flight.test$DEPARTURE_DELAY)
R.flight.test.boost

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.boost)) 
qqline((p.flight.boost))


## 5.GAM
#-------------------------
install.packages('gam')
library(gam)
gam.train <- train(DEPARTURE_DELAY ~ ., data=flight.train, method="gamSpline",tuneLength=10,trControl=ctrl)
R.flight.train.gam <- getTrainPerf(gam.train)
R.flight.train.gam


p.flight.test.gam <- predict(gam.train, newdata = flight.test)
R.flight.test.gam <- postResample(p.flight.test.gam, flight.test$DEPARTURE_DELAY)
R.flight.test.gam

qqnorm((flight.test$DEPARTURE_DELAY - p.flight.test.gam))
qqline((p.flight.test.gam))


#---------------------------
# Step 6: Model Evaluation
#---------------------------

# 1. Linear Models
#---------------------
models.linear<- list("linear"=flight.lm, "Ridge" = flight.ridge, "Lasso"=flight.lasso, "PCR(kernelpls)" = flight.pcr1, "PCR(pls)" = flight.pcr2, "PCR(pcr)" = flight.pcr3)

# 2. Non-Linear Models
#---------------------
models.nonlinear<- list("DecisionTree"=flight.dt, "Bagging" = flight.bt, "RandomForest"=flight.rf, "Boosting"=flight.boost, "GAM"=flight.gam)


# Result matrix: LINEAR
flight.resamples.linear<- resamples(models.linear)
summary(flight.resamples.linear)

# Plot model performances
bwplot(flight.resamples.linear, metric="MAE")
bwplot(flight.resamples.linear, metric="RMSE")
bwplot(flight.resamples.linear, metric="Rsquared")


# Result matrix: NON-LINEAR
flight.resamples.nonlinear<- resamples(models.nonlinear)
summary(flight.resamples)

# Plot model performances
bwplot(flight.resamples.nonlinear, metric="MAE")
bwplot(flight.resamples.nonlinear, metric="RMSE")
bwplot(flight.resamples.nonlinear, metric="Rsquared")


# MODEL Performances- ALL MODELS COMBINED
models.all<- list("Ridge" = flight.ridge, "Lasso"=flight.lasso, "PCR(kernelpls)" = flight.pcr1, "PCR(pls)" = flight.pcr2, "PCR(pcr)" = flight.pcr3, "DecisionTree"=flight.dt, "Bagging" = flight.bt, "RandomForest"=flight.rf, "Boosting"=flight.boost, "GAM"=flight.gam)

flight.resamples.all<- resamples(models.all)
summary(flight.resamples.all)

bwplot(flight.resamples.all, metric="MAE")
bwplot(flight.resamples.all, metric="RMSE")
bwplot(flight.resamples.all, metric="Rsquared")



