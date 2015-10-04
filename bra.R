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

sizes = c('34-9','38-15','34-8','34-7','38-8','34-6','32-9','32-5','38-10','36-9', 
          '32-7','30-6','42-14','40-16','40-13','40-12', '32-10', '40-10','36-10',
          '36-8','38-9','34-10','38-12','40-14','36-7','30-7','32-8','44-15','38-14', 
          '32-11','34-11','38-13','38-11','40-11','32-6','36-14','40-15','36-13', 
          '36-11','42-12','34-12','42-13','42-15','36-12','44-13','44-18','42-16',
          '44-16','42-11','46-15','34-13','46-16','44-14', '46-14','30-8','32-12',
          '44-17','30-5')

band_sizes = c(32, 34, 36, 38, 40, 42, 44, 46)

cup_sizes = c(6,7,8,9,10,11,12,13,14,15,16,17,18)

styles <- c('2941','3086','3281','3282','3646','3954','852189','N6101','N730023',
            'P011', 'P012','P013','P029')

style <- 'P029'
data = unique(data[data$style == style,])

data.active <- data[1:11] # features / predictors


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
round_pred = round(pred,0)
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
round_pred = round(pred,0)
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
# y2 <- data$size # target / response
# fit <- train(data.active, y2, method="knn", trControl=trainControl("cv", 5), tuneGrid=data.frame(k=3:9))

# pred <- predict(fit)
# round_pred = round(pred,0)
# abs_result =  abs(y2 - round_pred)
# success = sum(abs_result == 0)
# success_percent = round(success/length(abs_result) * 100, 2)

# print('BAND ANALYSIS (Caret Library):')
# print ( 'Success:')
# print ( success )
# print ( paste(success_percent, "%", sep="")  )

# print ( 'Fails:'  )
# fail <- length(abs_result) - success
# print ( paste(100 - success_percent, "%", sep="")  )
