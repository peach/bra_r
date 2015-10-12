library(caret)
library(sinkr)
library(pROC)

data = read.csv("~/Documents/dev/peach/r/measurement_vectors.csv")

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


recommended_size <- function(out, umbral, max_output){
  col_sizes = colnames(out)
  values_out <- numeric(nrow(out))
  sizes_out <- numeric(nrow(out))
  
  for(i in 1:nrow(out)) {
    row <- out[i,]
    main_values = rev(sort(row))
    
    size_out = ''
    value_out = 0
    
    for(k in 1:max_output){
      value <- main_values[k]
      if(value > umbral){
        for(j in 1:length(row)){
          if((row[j] == value) &!(length(grep(col_sizes[j], size_out)) > 0) ){
            size_out = paste(size_out, col_sizes[j]) 
            value_out = value_out + value
            break
          }
        }
      }
    }
    round_value_out <- as.character(round(value_out,2))
    sizes_out[i] = paste(round_value_out, size_out)
  }
  sizes_out
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

count <- function(x) {
  value <- length(unlist(strsplit(x, ' '))) - 2
  if(value < 0) value = 0
  return (value)
}
customSummary <- function(data, lev = NULL, model = NULL){
  if(is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)
  probs <- as.matrix(data[, lev, drop = FALSE])
  winners <- as.character(data[,"obs"])
  out <- recommended_size(probs, 0.25,4)
  
  print('winners')
  print(winners)
  print('recommended_size: ')
  print(out)
  out2 <- matrix_result(out, winners )
  print(out2)
  stats <- postResample(winners, as.factor(out2))
  
  quantity <- sapply(out, function(x) count(x) )
  probabylities <- sapply(out, function(x) (as.numeric(unlist(strsplit(x, ' ')))[1]) )
  
  stats <- c(stats, mean(quantity), mean(probabylities) )
  return (stats)
}

###########  SIZE ANALYSIS #############
y2 <- data$size # target / response
ctrl <- trainControl(method="repeatedcv", number=10, repeats=3,  classProbs = TRUE, summaryFunction = customSummary) 
fit <- train(data.active, as.factor(y2), method='knn',trControl=ctrl,  tuneGrid=data.frame(k=3:9))  
