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

# data <- data[!data %in% boxplot(data)$out]

m.dist <- mahalanobis(data.active, colMeans(data.active), cov(data.active))
print('bfore')
print(dim(data))

x <- data[1:11]
percentage.to.remove <- 5 # Remove 5% of points
number.to.remove     <- trunc(nrow(x) * percentage.to.remove / 100)
m.dist               <- mahalanobis(x, colMeans(x), cov(x))
m.dist.order         <- order(m.dist, decreasing=TRUE)
rows.to.keep.index   <- m.dist.order[(number.to.remove+1):nrow(x)]
data                <- data[rows.to.keep.index,]

data.active = data[1:11]

print('after')
print(dim(data))

source("bra-lib.R")

###########  SIZE ANALYSIS #############
y2 <- data$size # target / response
ctrl <- trainControl(method="repeatedcv", number=10, repeats=3,  classProbs = TRUE, summaryFunction = customSummary) 
fit <- train(data.active, as.factor(y2), method='knn',trControl=ctrl,  tuneGrid=data.frame(k=11))  

result <- fit$results
names(result) <- c('k',"Accuracy", 'Kappa','ConfidenceThreshold', 'Umbral', 'MaxOutput','Counts','Prob','Unconfident', 'FalsePositive','FalseNegative', "AccuracySD", 'KappaSD','CountsSD','ProbSD', 'UnconfidentSD', 'FalsePositiveSD','FalseNegativeSD' )
print('####### Result: ####### ')
print(t(result[1:11]))
