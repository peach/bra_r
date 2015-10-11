library(caret)
library(sinkr)

styles <- c('2941','3086','3281','3282','3646','3954','852189','N6101','N730023','P011', 'P012','P013','P029')
style <- 'P029'

data = read.csv("measurement_vectors.csv")

cmd <- "table(data$style)" 
print(cmd)
eval(parse(text=cmd))

style <- 'P029'
data = data[data$style == style,]

data.trans <- eof(data[1:11])
data[1:11] <- data.trans$A

trainIndex <- createDataPartition(data$size, p = .8, list = FALSE, times = 1)

data.train <- data[trainIndex, 1:11]
data.test <- data[-trainIndex, 1:11]

data.train.size <- data[trainIndex, "size"]
data.test.size <- data[-trainIndex, "size"]

library(caret)
library(sinkr)

styles <- c('2941','3086','3281','3282','3646','3954','852189','N6101','N730023','P011', 'P012','P013','P029')
style <- 'P029'

data = read.csv("measurement_vectors.csv")

cmd <- "table(data$style)" 
print(cmd)
eval(parse(text=cmd))

style <- 'P029'
data = data[data$style == style,]

data.trans <- eof(data[1:11])
data[1:11] <- data.trans$A

trainIndex <- createDataPartition(data$size, p = .8, list = FALSE, times = 1)

data.train <- data[trainIndex, 1:11]
data.test <- data[-trainIndex, 1:11]

data.train.size <- data[trainIndex, "size"]
data.test.size <- data[-trainIndex, "size"]

knnFit <- knn3(data.train, as.factor(data.train.size), k = 11)

pred <- predict(knnFit, data.test, type = "class")

match = (data.test.size == pred)
success = length(match[match==TRUE])
success_percent = round(success/length(match) * 100, 2)

print('SIZE ANALYSIS KNN3:')
print ( 'Success:')
print ( success )
print ( paste(success_percent, "%", sep="")  )

print ( 'Fails:'  )
print ( paste(100 - success_percent, "%", sep="")  )
