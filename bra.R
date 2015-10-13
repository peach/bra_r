library(caret)
library(sinkr)

data = read.csv("measurement_vectors.csv")

#set.seed(808)

print('## table(data$style) ##')
print(table(data$style))
data$size = sapply(data$size, function(x) make.names(x) )

styles <- c('2941','3086','3281','3282','3646','3954','852189','N6101','N730023','P011', 'P012','P013','P029')

style <- 'P029'
data = unique(data[data$style == style,])

filled_empty <- eof(data[1:11])
data[1:11] <- filled_empty$A
data.active = data[1:11]

source("bra-lib.R")

###########  SIZE ANALYSIS #############
y2 <- data$size # target / response
ctrl <- trainControl(method="repeatedcv", number=10, repeats=3,  classProbs = TRUE, summaryFunction = customSummary) 
fit <- train(data.active, as.factor(y2), method='knn',trControl=ctrl,  tuneGrid=data.frame(k=11))  

results <- nameResults(fit$results)
print('####### Result: ####### ')
print(colMeans(results)[1:11])
