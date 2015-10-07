library(class)
library(gmodels)
library(caret)
library(sinkr)

styles <- c('2941','3086','3281','3282','3646','3954','852189','N6101','N730023','P011', 'P012','P013','P029')
style <- 'P029'

data = read.csv("~/Documents/dev/peach/r/measurement_vectors.csv")

print('## table(data$style) ##')
print(table(data$style))

print( 'round(prop.table(table(data$style)) * 100, digits = 1)' )
print( round(prop.table(table(data$style)) * 100, digits = 1)  )

print("summary(data[c('style', 'size', 'style.size')])" )
print( summary(data[c("style", "size", 'style.size')]) )

sizes = c(3409,3815,3408,3407,3808,3406,3209,3205,3810,3609, 
          3207,3006,4214,4016,4013,4012, 3210, 4010,3610,
          3608,3809,3410,3812,4014,3607,3007,3208,4415,3814, 
          3211,3411,3813,3811,4011,3206,3614,4015,3613, 
          3611,4212,3412,4213,4215,3612,4413,4418,4216,
          4416,4211,4615,3413,4616,4414, 4614,3008,3212,
          4417,3005)

band_sizes = c(32, 34, 36, 38, 40, 42, 44, 46)

cup_sizes = c(6,7,8,9,10,11,12,13,14,15,16,17,18)

styles <- c('2941','3086','3281','3282','3646','3954','852189','N6101','N730023',
            'P011', 'P012','P013','P029')

style <- 'P029'
data = unique(data[data$style == style,])

filled_empty <- eof(data[1:11])
data[1:11] <- filled_empty$A
data.active = data[1:11]

min <- function(x, array) {
  result = array[1]
  min <- 10000
  for(i in 1:length(array)) {
    diff <- abs(x - array[i])
    if( diff < min ){
      min <- diff
      result= array[i]
    }
  }
  return (result)
}



###########  BAND ANALYSIS #############

#--  caret library ---
y2 <- data$band_size # target / response
fit <- train(data.active, y2, method="knn", trControl=trainControl("cv", 5), tuneGrid=data.frame(k=3:9))

pred <- predict(fit)
round_pred = sapply(pred, function(x) min(x, band_sizes))
abs_result =  abs(y2 - round_pred)
success = sum(abs_result == 0)
success_percent = round(success/length(abs_result) * 100, 2)

print('BAND ANALYSIS (Caret Library):')
print ( 'Success:')
print ( success )
print ( paste(success_percent, "%", sep="")  )

print ( 'Fails:'  )
fail <- length(abs_result) - success
print ( paste(100 - success_percent, "%", sep="")  )

# ---- With PC ---

pc <- prcomp(data.active)
main_pc <- pc$x[,1:3]

fit <- train(main_pc, y2, method="knn", trControl=trainControl("cv", 5), tuneGrid=data.frame(k=3:9))
pred <- predict(fit)

round_pred = sapply(pred, function(x) min(x, band_sizes))
abs_result =  abs(y2 - round_pred)
success = sum(abs_result == 0)
success_percent = round(success/length(abs_result) * 100, 2)


print('BAND ANALYSIS WITH PC (Caret Library):')
print ( 'Success:')
print ( success )
print ( paste(success_percent, "%", sep="")  )

print ( 'Fails:'  )
fail <- length(abs_result) - success
print ( paste(100 - success_percent, "%", sep="")  )

###########  CUP ANALYSIS #############

#--  caret library ---
y2 <- data$cup_size # target / response
fit <- train(data.active, y2, method="knn", trControl=trainControl("cv", 5), tuneGrid=data.frame(k=3:9))


pred <- predict(fit)
round_pred = sapply(pred, function(x) min(x, cup_sizes))
abs_result =  abs(y2 - round_pred)
success = sum(abs_result == 0)
success_percent = round(success/length(abs_result) * 100, 2)

print('CUP ANALYSIS (Caret Library):')
print ( 'Success:')
print ( success )
print ( paste(success_percent, "%", sep="")  )

print ( 'Fails:'  )
fail <- length(abs_result) - success
print ( paste(100 - success_percent, "%", sep="")  )

# ---- With PC ---

pc <- prcomp(data.active)
main_pc <- pc$x[,1:3]

fit <- train(main_pc, y2, method="knn", trControl=trainControl("cv", 5), tuneGrid=data.frame(k=3:9))
pred <- predict(fit)

round_pred = sapply(pred, function(x) min(x, cup_sizes))
abs_result =  abs(y2 - round_pred)
success = sum(abs_result == 0)
success_percent = round(success/length(abs_result) * 100, 2)


print('CUP ANALYSIS WITH PC (Caret Library):')
print ( 'Success:')
print ( success )
print ( paste(success_percent, "%", sep="")  )

print ( 'Fails:'  )
fail <- length(abs_result) - success
print ( paste(100 - success_percent, "%", sep="")  )

###########  SIZE ANALYSIS #############

#--  caret library ---

y2 <- data$size # target / response
fit <- train(data.active, y2, method="knn", trControl=trainControl("cv", 5), tuneGrid=data.frame(k=3:9))

pred <- predict(fit)
round_pred = sapply(pred, function(x) min(x, sizes))
abs_result =  abs(y2 - round_pred)
success = sum(abs_result == 0)
success_percent = round(success/length(abs_result) * 100, 2)

print('SIZE ANALYSIS (Caret Library):')
print ( 'Success:')
print ( success )
print ( paste(success_percent, "%", sep="")  )

print ( 'Fails:'  )
fail <- length(abs_result) - success
print ( paste(100 - success_percent, "%", sep="")  )


# ---- With PC ---

pc <- prcomp(data.active)
main_pc <- pc$x[,1:3]

fit <- train(main_pc, y2, method="knn", trControl=trainControl("cv", 5), tuneGrid=data.frame(k=3:9))
pred <- predict(fit)
round_pred = pred
#round_pred = sapply(pred, function(x) min(x, sizes))
abs_result =  abs(y2 - round_pred)
success = sum(abs_result == 0)
success_percent = round(success/length(abs_result) * 100, 2)


print('SIZE ANALYSIS WITH PC (Caret Library):')
print ( 'Success:')
print ( success )
print ( paste(success_percent, "%", sep="")  )

print ( 'Fails:'  )
fail <- length(abs_result) - success
print ( paste(100 - success_percent, "%", sep="")  )


############
Sonar <- data

inTrain <- createDataPartition(Sonar$size, p = 4/5, list = FALSE)
## Save the predictors and class in different objects
sonarTrain <- Sonar[ inTrain, 1:11]
sonarTest  <- Sonar[-inTrain, 1:11]

trainClass <- Sonar[ inTrain, "size"]
testClass  <- Sonar[-inTrain, "size"]

centerScale <- preProcess(sonarTrain)
centerScale

training <- predict(centerScale, sonarTrain)
testing <- predict(centerScale, sonarTest)

knnFit <- knn3(training, as.factor(trainClass), k = 11)
knnFit

pred <- predict(knnFit,testing, type = "class")

round_pred = as.numeric(as.character(pred))
abs_result =  abs(testClass - round_pred)
success = sum(abs_result == 0)
success_percent = round(success/length(abs_result) * 100, 2)


print('SIZE ANALYSIS KNN3:')
print ( 'Success:')
print ( success )
print ( paste(success_percent, "%", sep="")  )

print ( 'Fails:'  )
fail <- length(abs_result) - success
print ( paste(100 - success_percent, "%", sep="")  )


