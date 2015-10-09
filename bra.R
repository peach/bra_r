library(caret)
library(sinkr)

data = read.csv("~/Documents/dev/peach/r/measurement_vectors.csv")

set.seed(808)

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
    value_out = ''
    
    for(k in 1:max_output){
      value <- main_values[k]
      if(value > umbral){
        for(j in 1:length(row)){
          if((row[j] == value) & !(size_out %in% row[j]) ){
            size_out = paste(size_out, col_sizes[j]) 
            value_out = paste(value_out, round(value,2))
            break
          }
        }
      }
    }
    
    values_out[i] = value_out
    sizes_out[i] = as.character(size_out)
  }
  return (sizes_out)
}

matrix_result <- function(out, winners){
  result = winners
  for( i in 1:length(winners)){
    if(length(grep(winners[i], out[i])) > 0) result[i] = 'wrong' 
  }
  return (result)
}

customSummary <- function(data, lev = NULL, model = NULL){
  if(is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)
  probs <- as.matrix(data[, lev, drop = FALSE])
  winners <- data[,"obs"]
  out <- recommended_size(probs, 0.01,4)
  
  print('winners')
  print(winners)
  print('recommended_size: ')
  print(out)
  out2 <- matrix_result(out, as.character(winners) )
  print('matrix_result: ')
  print(out2)
  postResample(winners, as.factor(out2))

}

###########  SIZE ANALYSIS #############
y2 <- data$size # target / response
ctrl <- trainControl(method="repeatedcv", number=10, repeats=3,  classProbs = TRUE, summaryFunction = customSummary) 
fit <- train(data.active, as.factor(y2), method='knn',trControl=ctrl,  tuneGrid=data.frame(k=3:9))  

