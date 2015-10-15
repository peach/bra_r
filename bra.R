library(caret)
library(sinkr)

source("bra-lib.R")

org_data = read.csv("measurement_vectors.csv" )

#set.seed(808)

data <- org_data

print('## table(data$style) ##')

print(table(data$style))
data$size = sapply(data$size, function(x) make.names(x) )

styles <- unique(data$size)

filled_empty <- eof(data[1:11])
data[1:11] <- filled_empty$A

style <- 'P029'
data = data[data$style == style,]

data.active <- data[1:11]

###########  SIZE ANALYSIS #############
y2 <- data$size # target / response

ctrl <- trainControl(method="cv", number=5, repeats=1,  classProbs = TRUE, summaryFunction = customSummary) 

print(length(y2))
fit <- train(data.active, as.factor(y2), method='knn',trControl=ctrl,  tuneGrid=data.frame(k=11))  

result <- fit$results
names(result) <- c('k',"Accuracy", 'Kappa','ConfidenceThreshold', 'Umbral', 'MaxOutput','Counts','Prob','Unconfident', 'FalsePositive','FalseNegative', "AccuracySD", 'KappaSD','CountsSD','ProbSD', 'UnconfidentSD', 'FalsePositiveSD','FalseNegativeSD' )
print('####### Result: ####### ')
print(t(result[1:11]))
