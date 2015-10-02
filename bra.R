library(class)
library(gmodels)

data = read.csv("~/Documents/dev/peach/r/measurement_vectors.csv")

print('## table(data$style) ##')
print(table(data$style))

print( 'round(prop.table(table(data$style)) * 100, digits = 1)' )
print( round(prop.table(table(data$style)) * 100, digits = 1)  )

sizes = c('34-9','38-15','34-8','34-7','38-8','34-6','32-9','32-5','38-10','36-9', 
          '32-7','30-6','42-14','40-16','40-13','40-12', '32-10', '40-10','36-10',
          '36-8','38-9','34-10','38-12','40-14','36-7','30-7','32-8','44-15','38-14', 
          '32-11','34-11','38-13','38-11','40-11','32-6','36-14','40-15','36-13', 
          '36-11','42-12','34-12','42-13','42-15','36-12','44-13','44-18','42-16',
          '44-16','42-11','46-15','34-13','46-16','44-14', '46-14','30-8','32-12',
          '44-17','30-5') 

styles <- c('2941','3086','3281','3282','3646','3954','852189','N6101','N730023','P011', 'P012','P013','P029')

result_matrix = matrix(0, nrow=length(sizes),ncol = length(sizes) )
rownames(result_matrix) <- sizes
colnames(result_matrix) <- sizes

style <- 'P029'
data = unique(data[data$style == style,])


print( summary(data[c("style", "size", 'style.size')]) )
print("summary(data[c('style', 'size', 'style.size')])" )


for (i in 1:5 ) {
  ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.80, 0.20))
  data.training <- data[ind==1, 1:11]
  data.test <- data[ind==2, 1:11]

  data.trainLabels <- data[ind==1, 13]
  data.testLabels <- data[ind==2, 13]

  data_pred <- knn(train = data.training, test = data.test, cl = data.trainLabels, k=20)

  y <- CrossTable(x = data.testLabels, y = data_pred, prop.chisq=FALSE)

  print ( x <- y[1]$t  )

  for(i in 1:nrow(x))
  {
    for(j in 1:ncol(x))
    {
      # print(paste(rownames(x)[i],colnames(x)[j]))
      col<-colnames(x)[j]
      row<-rownames(x)[i]
      value<-x[row,col]
      result_matrix[col,row] <- result_matrix[col,row] + value
    }
  }

} #end for

print ( 'Total predictions: ' )
print ( total <- sum(result_matrix) )

print ( 'Success:')
print ( success <- sum(result_matrix[colnames(result_matrix)[col(result_matrix)] == rownames(result_matrix)[row(result_matrix)]]) )

success_percent = round(success/total * 100, 2)
print ( paste(success_percent, "%", sep="")  )

print ( 'Fails:'  )
print ( fail <- total - success )
print ( paste(100 - success_percent, "%", sep="")  )

