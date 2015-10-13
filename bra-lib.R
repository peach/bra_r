sizes = c('X3409','X3815','X3408','X3407','X3808','X3406','X3209','X3205','X3810','X3609','X4013','X4012', 'X3210', 'X4010','X3610',
          'X3608','X3809','X3410','X3812','X4014','X3607','X3208','X3814','X3211','X3411','X3813','X3811','X4011','X3206','X3613', 
          'X3611','X4212','X3412','X4213','X4215','X3612', 'X4614','X4417','X3005', 'X3006', 'X3207', 'X4214','X4016','X3007', 'X4415',
          'X3614','X3008','X3413','X4416','X4211','X4216','X4413','X4414')

# ,X4418','X4015','X4615','X4015','X4616','X3212'

umbral <- 0.15
max_output <- 4
confidence_threshold = .80

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
  out <- recommended_size(probs, umbral,max_output)
  out2 <- matrix_result(out$sizes, winners)
  stats <- postResample(winners, as.factor(out2))
  
  match <- (out2 == winners)
  
  confident <- (out$prob > confidence_threshold)
  false_positive <- (confident & !match)
  false_negative <- ((!confident) & match)
  
  stats <- c(stats, confidence_threshold, umbral, max_output, mean(out$counts), mean(out$prob), true_percent(!confident), true_percent(false_positive), true_percent(false_negative) )
  return (stats)
}

nameResults <- function(results) {
  names(results) <- c('k',"Accuracy", 'Kappa','ConfidenceThreshold', 'Umbral', 'MaxOutput','Counts','Prob','Unconfident', 'FalsePositive','FalseNegative', "AccuracySD", 'KappaSD','CountsSD','ProbSD', 'UnconfidentSD', 'FalsePositiveSD','FalseNegativeSD' )
  return (results)
}

remove_outliers <- function(data,columns){
  head(data)
  keep_index = c()
  for(i in 1:length(sizes)){
    x = unique(data[data$size == sizes[i],][columns])
    percentage.to.remove <- 2 # Remove 3% of points
    number.to.remove     <- trunc(nrow(x) * percentage.to.remove / 100)
    centroid             <- colMeans(x)
    m.dist               <- mahalanobis(x, center= centroid, cov=cov(x),tol=1e-20)
    m.dist.order         <- order(m.dist, decreasing=TRUE)
    rows.to.keep.index   <- m.dist.order[(number.to.remove+1):nrow(x)]
    keep_index <- c(keep_index, rows.to.keep.index)
  }
  data                  <- data[ keep_index,]
  
  data$index <- as.numeric(row.names(data))
  data <- data[order(data$index), ]
  return (data)
}


