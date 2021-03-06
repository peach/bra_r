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

winners <- as.character(data.test.size)

knnFit <- knn3(data.train, as.factor(data.train.size), k = 11)
pred <- predict(knnFit,data.test, type = "prob")

out <- recommended_size(pred, 0.10, 4)

print('recommended_size')
print(out$sizes)

out2 <- matrix_result(out$sizes, winners)

print('matrix_result')
print(out2)

print('evaluation')
print(postResample(data.test.size, as.factor(out2)))

print('mean sizes')
print(mean(out$counts))

true_percent <- function(v) {
  num = length(v[v==TRUE])
  count = length(v)
  return (round((num*100)/count, 2))
}

confidence_threshold = .85
match <- (out2 == winners)
confident <- (out$prob > confidence_threshold)
false_positive <- (confident & !match)
false_negative <- ((!confident) & match)
print('no confidence')
print(true_percent(!confident))
print('false positive')
print(true_percent(false_positive))
print('false negative')
print(true_percent(false_negative))
