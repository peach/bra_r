# bra_r


library(class)
library(gmodels)

data = read.csv("~/Documents/dev/peach/r/measurement_vectors.csv")

print('## table(data$style) ##')
print(table(data$style))

print( 'round(prop.table(table(data$style)) * 100, digits = 1)' )
print( round(prop.table(table(data$style)) * 100, digits = 1)  )

# Filter by style

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

# In order to assess your model's performance later, you will 
# need to divide the data set into two parts: a training set 
# and a test set. The first is used to train the system, while 
# the second is used to evaluate the learned or trained system. 
# splitting 4/5 of the original data set as the training set, 
# while the 1/5 that remains will compose the test set.

print( 'set.seed(1234)' ) 
# set.seed(1234)

# To make your training and test sets, you first set a seed. 
# This is a number of R's random number generator. The major 
# advantage of setting a seed is that you can get the same 
# sequence of random numbers whenever you supply the same 
# seed in the random number generator.

# then, you want to make sure that the data set is shuffled and 
# that you have the same ratio between species in your training 
# and test sets. You use the sample() function to take a sample 
# with a size that is set as the number of rows of the Iris data set. 
# You sample with replacement: you choose from a vector of 2 elements 
# and assign either 1 or 2 to the rows of the data set. The assignment 
# of the elements is subject to probability weights of 0.80 and 0.20.

for (i in 1:5 ) {
  
print( 'ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.80, 0.20))' )
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.80, 0.20))

print( 'data.training <- data[ind==1, 1:11]' )
data.training <- data[ind==1, 1:11]

print( 'data.test <- data[ind==2, 1:11]' )
data.test <- data[ind==2, 1:11]

print( 'data.trainLabels <- data[ind==1, 13]' )
data.trainLabels <- data[ind==1, 13]

print( 'data.testLabels <- data[ind==2, 13]' )
data.testLabels <- data[ind==2, 13]

# An easy way to do these two steps is by using the knn() function, which 
# uses the Euclidian distance measure in order to find the k-nearest neighbours 
# to your new, unknown instance. Here, the k parameter is one that you set yourself. 
# As mentioned before, new instances are classified by looking at the majority vote 
# or weighted vote. In case of classification, the data point with the highest score 
# wins the battle and the unknown instance receives the label of that winning data point. 
# If there is an equal amount of winners, the classification happens randomly.

# To build your classifier, you need to take the knn() function and simply add some 
# arguments to it, just like in this example:

print( 'data_pred <- knn(train = data.training, test = data.test, cl = data.trainLabels, k=3)' )
data_pred <- knn(train = data.training, test = data.test, cl = data.trainLabels, k=20)

print( 'CrossTable(x = data.testLabels, y = data_pred, prop.chisq=FALSE)[1]' )
y <- CrossTable(x = data.testLabels, y = data_pred, prop.chisq=FALSE)

print ( 'x <- y[1]$t'  )
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
