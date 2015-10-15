umbral <- 0.15
max_output <- 4
min_row_by_style_size <- 8
confidence_threshold = .90
percent_remove_points <- .1

recommended_size <- function(out, umbral, max_output){
  col_sizes = colnames(out)
  sizes_out <- numeric(nrow(out))
  size_count_out <- numeric(nrow(out))
  prob_out <- numeric(nrow(out))
  prob_out_values <- numeric(nrow(out))
  
  for(i in 1:nrow(out)) {
    row <- out[i,]
    sorted_values = sort.int(row, decreasing=TRUE, index.return=TRUE)
    size_out = ''
    prob_values = ''
    
    size_count = 0
    prob = 0
    for(k in 1:max_output){
      value <- sorted_values$x[k]
      if (value < umbral) {
        break
      }
      size_out = paste(size_out, col_sizes[sorted_values$ix[k]])
      prob_values = paste(prob_values, round(value,2))
      prob = prob + value
      size_count = k
    }
    sizes_out[i] = size_out
    size_count_out[i] = size_count
    prob_out[i] = prob
    prob_out_values[i] = prob_values 
  }
  out = list("sizes"=sizes_out, "counts"=size_count_out, "prob"=prob_out , 'prob_values'=prob_out_values)
  out
}

matrix_result <- function(out, winners){
  result = as.character(winners)
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

result_csv <- function(index,out, winner_predicted){
  result <- org_data[index,]
  result <- cbind(result,confidence_threshold,stringsAsFactors=FALSE)
  result <- cbind(result,umbral,stringsAsFactors=FALSE)
  result <- cbind(result,max_output,stringsAsFactors=FALSE)
  result <- cbind(result,percent_remove_points,stringsAsFactors=FALSE)
  sizes <- out$sizes
  result <- cbind(result,sizes,stringsAsFactors=FALSE)
  prob_values <- out$prob_values
  result <- cbind(result,prob_values,stringsAsFactors=FALSE)
  prob <- out$prob
  result <- cbind(result,prob,stringsAsFactors=FALSE)
  prediction <- winner_predicted
  result <- cbind(result,prediction,stringsAsFactors=FALSE)
  
  ru <- nrow( result[result$prob <= confidence_threshold] )
  if(ru > 0 ) result$prob[result$prob <= confidence_threshold] <-  1
  rc <- nrow( result[result$prob > confidence_threshold] )
  if(rc > 0 ) result$prob[result$prob > confidence_threshold] <-  0
  
  rk <- nrow( result[result$prediction == 'wrong'] )
  if(rk > 0 ) result$prediction[result$prediction == 'wrong'] <- '0'
  rl <- nrow( result[result$prediction != 'wrong'] )
  if(rl > 0 ) result$prediction[result$prediction != 'wrong'] <- '1'
  
  print(head(result))
  
  return (result)
}

customSummary <- function(d, lev = NULL, model = NULL){
  if(is.character(d$obs)) d$obs <- factor(d$obs, levels = lev)
  
  probs <- as.matrix(d[, lev, drop = FALSE])
  winners <- as.character(d[,"obs"])
  out <- recommended_size(probs, umbral,max_output)
  
  out2 <- matrix_result(out$sizes, winners)
  result_csv(d$rowIndex,out, out2)
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
  keep_index = c()
  style_size <- unique(data$style.size)
  rows = setNames(data.frame(matrix(nrow=0, ncol=ncol(data))), names(data))
  for(i in 1:length(style_size)){
    x = data[data$style.size == style_size[i],]
    if(nrow(x) > min_row_by_style_size){
      x.active = x[columns]
      number.to.remove     <- trunc(nrow(x.active) * percent_remove_points)
      centroid             <- colMeans(x.active)
      m.dist               <- mahalanobis(x.active, center= centroid, cov=cov(x.active),tol=1e-20)
      m.dist.order         <- order(m.dist, decreasing=TRUE)
      rows.to.keep.index   <- m.dist.order[(number.to.remove+1):nrow(x.active)]
      new_rows = x[rows.to.keep.index,]
      rows = rbind(rows, new_rows)
    }
  }
  return (rows)
}


