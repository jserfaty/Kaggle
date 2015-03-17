# Thanks to Stephane Soulier for the starter code
# https://www.kaggle.com/c/axa-driver-telematics-analysis/forums/t/11299/score-0-66-with-logistic-regression

# Top 25% Score

# Write functions to create features
tripTimeCalc <- function(positions){
  return(nrow(positions))
}

birdDistCalc <- function(positions){
  return(sqrt(positions[nrow(positions),1]^2+positions[nrow(positions),2]^2))
}

speedCalc <- function(positions){
  return(matrix(sqrt(diff(positions[,1],1)^2 + diff(positions[,2],1)^2)))
}

roadDistCalc <- function(speed){
  return(sum(speed))
}

accelCalc <- function(speed){
  return(matrix(c(NA,diff(speed,1))))
}

deltaAccelCalc <- function(accel){
  return(matrix(c(NA,diff(accel,1))))
}

###########################
set.seed(1)

# List all the drivers
drivers <- list.files("path to drivers")

# Select 10 random drivers to use as 0s for supervised learning
randomDrivers <- sample(drivers, size = 10)

# Calculate features for all random drivers
speed <- NULL
accel <- NULL
refData <- NULL
target <- 0
names(target) <- "target"
for(driver in randomDrivers)
{
  dirPath <- paste0("path to drivers", driver, "\\")
  for(i in 1:200)
  {
    positions <- read.csv(paste0(dirPath, i, ".csv"))
    speed <- speedCalc(positions)
    
    # To smooth out jumps GPS data, set cap on large speeds
    speed[which(speed > 45)] <- ifelse(length(speed[which(speed > 45)-1])==0,speed[which(speed > 45)+1],speed[which(speed > 45)-1])
    accel <- accelCalc(speed)
    tripTime <- tripTimeCalc(positions)
    birdDist <- birdDistCalc(positions)
    roadDist <- roadDistCalc(speed)
    deltaAccel <- deltaAccelCalc(accel)
    
    # Split features into quantiles
    speedntiles <- quantile(speed,probs=seq(0,1,0.1),na.rm=TRUE,names=FALSE)
    accelntiles <- quantile(accel[accel >= 0],probs=seq(0,1,0.1),na.rm=TRUE,names=FALSE)
    decelntiles <- quantile(accel[accel <= 0],probs=seq(0,1,0.1),na.rm=TRUE,names=FALSE)
    posdAccelntiles <- quantile(deltaAccel[deltaAccel >= 0],probs=seq(0,1,0.1),na.rm=TRUE,names=FALSE)
    negdAccelntiles <- quantile(deltaAccel[deltaAccel <= 0],probs=seq(0,1,0.1),na.rm=TRUE,names=FALSE)
    positionDataFrame <- data.frame(speed,accel)
    positionFactors <- data.frame(driver_num = paste0(driver,"_",i)
                                  ,tripTime
                                  ,birdDist
                                  ,roadDist
                                  ,speedn = t(speedntiles)
                                  ,acceln = t(accelntiles)
                                  ,deceln = t(decelntiles)
                                  ,posdacceln = t(posdAccelntiles)
                                  ,negdacceln = t(negdAccelntiles)
                                  ,stringsAsFactors=FALSE)
    positionFactorsNorm <- cbind(driver_num = positionFactors[,1],positionFactors[,-1],target)
    refData <- rbind(refData, positionFactorsNorm)
  }
}

# Repeat process for target driver
positionFactors <- NULL
target <- 1
submission <- NULL
for(driver in drivers)
{
  result <- NULL
  print(driver)
  dirPath <- paste0("path to drivers", driver, "\\")
  currentData <- NULL
  for(i in 1:200)
  {
    positions <- read.csv(paste0(dirPath, i, ".csv"))
    speed <- speedCalc(positions)
    speed[which(speed > 45)] <- ifelse(length(speed[which(speed > 45)-1])==0,speed[which(speed > 45)+1],speed[which(speed > 45)-1])
    accel <- accelCalc(speed)
    tripTime <- tripTimeCalc(positions)
    birdDist <- birdDistCalc(positions)
    roadDist <- roadDistCalc(speed)
    deltaAccel <- deltaAccelCalc(accel)
    speedntiles <- quantile(speed,probs=seq(0,1,0.1),na.rm=TRUE,names=FALSE)
    accelntiles <- quantile(accel[accel >= 0],probs=seq(0,1,0.1),na.rm=TRUE,names=FALSE)
    decelntiles <- quantile(accel[accel <= 0],probs=seq(0,1,0.1),na.rm=TRUE,names=FALSE)
    posdAccelntiles <- quantile(deltaAccel[deltaAccel >= 0],probs=seq(0,1,0.1),na.rm=TRUE,names=FALSE)
    negdAccelntiles <- quantile(deltaAccel[deltaAccel <= 0],probs=seq(0,1,0.1),na.rm=TRUE,names=FALSE)
    positionDataFrame <- data.frame(speed,accel)
    positionFactors <- data.frame(driver_num = paste0(driver,"_",i)
                                  ,tripTime
                                  ,birdDist
                                  ,roadDist
                                  ,speedn = t(speedntiles)
                                  ,acceln = t(accelntiles)
                                  ,deceln = t(decelntiles)
                                  ,posdacceln = t(posdAccelntiles)
                                  ,negdacceln = t(negdAccelntiles)
                                  ,stringsAsFactors=FALSE)
    positionFactorsNorm <- cbind(driver_num = positionFactors[,1],positionFactors[,-1],target)
    currentData <- rbind(currentData, positionFactorsNorm)
  }
  
  # If the target driver is one of the random drivers, ignore that random driver
  filteredRefData <- refData %>% filter(substring(as.character(refData$driver_num),1,4) != driver)
  train <- rbind(currentData, filteredRefData)
  train <- as.data.frame(train)
  
  # Train the RF
  fit.rf <- randomForest(x=train[,-c(1,60)],y=train[,60])
  currentData <- as.data.frame(currentData)
  
  # Apply RF to new data
  predict.rf <- predict(fit.rf,newdata=currentData[,-c(1,60)])
  
  # Create submission
  labels <- sapply(1:200, function(x) paste0(driver,'_', x))
  result <- cbind(labels, predict.rf)
  submission <- rbind(submission, result)
}
