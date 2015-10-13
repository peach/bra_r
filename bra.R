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

recommended_size <- function(out, umbral, max_output){
  col_sizes = colnames(out)
  sizes_out <- numeric(nrow(out))
  size_count_out <- numeric(nrow(out))
  prob_out <- numeric(nrow(out))
  
  for(i in 1:nrow(out)) {
    row <- out[i,]
    sorted_values = sort.int(row, decreasing=TRUE, index.return=TRUE)
    size_out = ''
    size_count = 0
    prob = 0
    for(k in 1:max_output){
      value <- sorted_values$x[k]
      if (value < umbral) {
        break
      }
      size_out = paste(size_out, col_sizes[sorted_values$ix[k]])
      prob = prob + value
      size_count = k
    }
    sizes_out[i] = size_out
    size_count_out[i] = size_count
    prob_out[i] = prob
  }
  out = list("sizes"=sizes_out, "counts"=size_count_out, "prob"=prob_out)
  out
}

matrix_result <- function(out, winners){
  result = winners
  for( i in 1:length(winners)){
    l = unlist(strsplit(out[i], split=' '))
    sizes = l[2:length(l)]
    if(!(winners[i] %in% sizes)){
      result[i] = 'wrong'
    } 
  }
  return (result)
}

true_percent <- function(v) {
  num = length(v[v==TRUE])
  count = length(v)
  return (round((num*100)/count, 2))
}

customSummary <- function(data, lev = NULL, model = NULL){
  if(is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)
  probs <- as.matrix(data[, lev, drop = FALSE])
  winners <- as.character(data[,"obs"])
  umbral <- 0.17
  max_output <- 4
  out <- recommended_size(probs, umbral,max_output)
  out2 <- matrix_result(out$sizes, winners)
  stats <- postResample(winners, as.factor(out2))
  
  confidence_threshold = 0.8
  match <- (out2 == winners)
  
  confident <- (out$prob > confidence_threshold)
  false_positive <- (confident & !match)
  false_negative <- ((!confident) & match)
  
  stats <- c(stats, confidence_threshold, umbral, max_output, mean(out$counts), mean(out$prob), true_percent(!confident), true_percent(false_positive), true_percent(false_negative) )
  return (stats)
}

###########  SIZE ANALYSIS #############
y2 <- data$size # target / response
ctrl <- trainControl(method="repeatedcv", number=10, repeats=3,  classProbs = TRUE, summaryFunction = customSummary) 
fit <- train(data.active, as.factor(y2), method='knn',trControl=ctrl,  tuneGrid=data.frame(k=11))  

result <- fit$results
names(result) <- c('k',"Accuracy", 'Kappa','ConfidenceThreshold', 'Umbral', 'MaxOutput','Counts','Prob','Unconfident', 'FalsePositive','FalseNegative', "AccuracySD", 'KappaSD','CountsSD','ProbSD', 'UnconfidentSD', 'FalsePositiveSD','FalseNegativeSD' )
print('####### Result: ####### ')
print(t(result[1:11]))
