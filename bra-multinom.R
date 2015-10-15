library(caret)
library(sinkr)

source("bra-lib.R")

umbral <- 0.10
max_output <- 4
confidence_threshold = .85
min_row_by_style_size <- 4
percent_remove_points <- .0

data.raw = read.csv("measurement_vectors.csv")

cmd <- "table(data.raw$style)" 
print(cmd)
eval(parse(text=cmd))

style <- 'P029'
data.style = data.raw[data.raw$style == style,]

data.style$size = sapply(data.style$size, function(x) make.names(x))
data.trans <- eof(data.style[1:11])
data.style[1:11] <- data.trans$A

#data.cooked = remove_outliers(data.style, c(1:11))
data.cooked = data.style

ctrl <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=customSummary)
fit <- train(data.cooked[1:11], as.factor(data.cooked$size), tuneLength=5, method="multinom", maxit=500, trControl=ctrl)

results <- nameResults(fit$results)
print(colMeans(results)[1:11])
