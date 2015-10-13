library(caret)
library(sinkr)

source("bra-lib.R")

data = read.csv("measurement_vectors.csv")

#set.seed(808)

print('## table(data$style) ##')
print(table(data$style))
data$size = sapply(data$size, function(x) make.names(x) )

styles <- c('2941','3086','3281','3282','3646','3954','852189','N6101','N730023','P011', 'P012','P013','P029')

filled_empty <- eof(data[1:11])
data[1:11] <- filled_empty$A

data = remove_outliers(data,c(1:10))

style <- 'P029'
print(dim(data))
data = unique(data[data$style == style,])
print(dim(data))

data.active <- data[1:11]



###########  SIZE ANALYSIS #############
y2 <- data$size # target / response

ctrl <- trainControl(method="repeatedcv", number=10, repeats=3,  classProbs = TRUE, summaryFunction = customSummary) 
fit <- train(data.active, as.factor(y2), method='knn',trControl=ctrl,  tuneGrid=data.frame(k=11))  

result <- fit$results
names(result) <- c('k',"Accuracy", 'Kappa','ConfidenceThreshold', 'Umbral', 'MaxOutput','Counts','Prob','Unconfident', 'FalsePositive','FalseNegative', "AccuracySD", 'KappaSD','CountsSD','ProbSD', 'UnconfidentSD', 'FalsePositiveSD','FalseNegativeSD' )
print('####### Result: ####### ')
print(t(result[1:11]))
