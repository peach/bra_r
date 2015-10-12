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
          if((row[j] == value) &!(length(grep(col_sizes[j], size_out)) > 0) ){
            size_out = paste(size_out, col_sizes[j]) 
            value_out = paste(value_out, round(value,2))
            break
          }
        }
      }
    }
    sizes_out[i] = size_out
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

winners <- as.character(data.test.size)

knnFit <- knn3(data.train, as.factor(data.train.size), k = 11)
pred <- predict(knnFit,data.test, type = "prob")

out <- recommended_size(pred, 0.01,4)

print('recommended_size')
print(out)

out2 <- matrix_result(out, winners )

print('matrix_result')
print(out2)

print('evaluation')
print(postResample(data.test.size, as.factor(out2)))
