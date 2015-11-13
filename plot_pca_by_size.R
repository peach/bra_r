library(lattice)
library(ggplot2)
library(sinkr)

b = read.csv("measurement_vectors.csv")

filled_empty <- eof(b[1:11])
b[1:11] <- filled_empty$A

sizes = c('4012','3811','3812','3813','4011','4013','4211','4212','4213')

colors = c('red','orange','yellow','green','brown','purple','pink','maroon','navy','black')

for(i in 1:length(sizes)) {
  size = sizes[i]
  bi = unique(b[b$size == size,])
  xi <- bi[1:11]
  pci <- prcomp(xi)
  yi <- pci$x[,1:2]
  plot(yi,col=colors[i] )
  par(new=TRUE)
}

par(new=FALSE)

for(i in 1:length(sizes)) {
  size = sizes[i]
  bi = unique(b[b$size == size,])
  xi <- bi[1:11]
  pci <- prcomp(xi)
  yi <- pci$x[,1:2]
  plot(yi,col=colors[i] )
  text(yi,labels=bi$winner_bra_id)
  par(new=TRUE)
}
