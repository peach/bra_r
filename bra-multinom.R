library(caret)
library(sinkr)

source("bra-lib.R")

umbral <- 0.10
max_output <- 4
confidence_threshold = .83

styles <- c('2941','3086','3281','3282','3646','3954','852189','N6101','N730023','P011', 'P012','P013','P029')
style <- 'P029'

data = read.csv("measurement_vectors.csv")

cmd <- "table(data$style)" 
print(cmd)
eval(parse(text=cmd))

style <- 'P029'
data = data[data$style == style,]

data$size = sapply(data$size, function(x) make.names(x))
data.trans <- eof(data[1:11])
data[1:11] <- data.trans$A

ctrl <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=customSummary)
fit <- train(data[1:11], as.factor(data$size), tuneLength=5, method="multinom", maxit=500, trControl=ctrl)

results <- nameResults(fit$results)
print(colMeans(results)[1:11])
