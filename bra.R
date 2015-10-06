library(class)
library(gmodels)
library(caret)

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

data.active <- data[1:11] # features / predictors

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

ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.80, 0.20))
data.training <- data[ind==1, 1:11]
data.test <- data[ind==2, 1:11]

# band_size
data.trainLabels <- data[ind==1, 14]
data.testLabels <- data[ind==2, 14]

data_pred <- knn(train = data.training, test = data.test, cl = data.trainLabels, k=10)
y <- CrossTable(x = data.testLabels, y = data_pred, prop.chisq=FALSE)
x <- y[1]$t 

print('BAND ANALYSIS:')
print ( 'Total predictions: ' )
print ( total <- sum(x) )

print ( 'Success:')
print ( success <- sum(x[colnames(x)[col(x)] == rownames(x)[row(x)]]) )

success_percent = round(success/total * 100, 2)
print ( paste(success_percent, "%", sep="")  )

print ( 'Fails:'  )
print ( fail <- total - success )
print ( paste(100 - success_percent, "%", sep="")  )

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

###########  CUP ANALYSIS #############

ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.80, 0.20))
data.training <- data[ind==1, 1:11]
data.test <- data[ind==2, 1:11]

# cup_size
data.trainLabels <- data[ind==1, 15]
data.testLabels <- data[ind==2, 15]

data_pred <- knn(train = data.training, test = data.test, cl = data.trainLabels, k=10)

y <- CrossTable(x = data.testLabels, y = data_pred, prop.chisq=FALSE)

x <- y[1]$t

print('CUP ANALYSIS:')
print ( 'Total predictions: ' )
print ( total <- sum(x) )

print ( 'Success:')
print ( success <- sum(x[colnames(x)[col(x)] == rownames(x)[row(x)]]) )

success_percent = round(success/total * 100, 2)
print ( paste(success_percent, "%", sep="")  )

print ( 'Fails:'  )
print ( fail <- total - success )
print ( paste(100 - success_percent, "%", sep="")  )

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

###########  SIZE ANALYSIS #############

ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.80, 0.20))
data.training <- data[ind==1, 1:11]
data.test <- data[ind==2, 1:11]

# cup_size
data.trainLabels <- data[ind==1, 13]
data.testLabels <- data[ind==2, 13]

data_pred <- knn(train = data.training, test = data.test, cl = data.trainLabels, k=10)

y <- CrossTable(x = data.testLabels, y = data_pred, prop.chisq=FALSE)

x <- y[1]$t

print('SIZE ANALYSIS:')
print ( 'Total predictions: ' )
print ( total <- sum(x) )

print ( 'Success:')
print ( success <- sum(x[colnames(x)[col(x)] == rownames(x)[row(x)]]) )

success_percent = round(success/total * 100, 2)
print ( paste(success_percent, "%", sep="")  )

print ( 'Fails:'  )
print ( fail <- total - success )
print ( paste(100 - success_percent, "%", sep="")  )

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
main_pc <- pc$x[,1:2]

fit <- train(main_pc, y2, method="knn", trControl=trainControl("cv", 5), tuneGrid=data.frame(k=3:9))
pred <- predict(fit)

round_pred = sapply(pred, function(x) min(x, sizes))
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
