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

fit <- train(data[1:11], as.factor(data$size), tuneLength=5, method="multinom", trControl=trainControl("cv", 5))

print(fit$results)

print(head(fit.fitted <- fitted(fit)))
