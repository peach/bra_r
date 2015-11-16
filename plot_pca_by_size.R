library(lattice)
library(ggplot2)
library(sinkr)

b = read.csv("measurement_vectors.csv")

filled_empty <- eof(b[1:11])
b[1:11] <- filled_empty$A

pc <- prcomp(b[1:11])
b <- cbind(b,pc$x[,1:2])

sizes = c('3006','3007','3008','3205','3206','3207','3208','3209','3210','3211','3212','3406','3407','3408','3409','3410','3411','3412','3413','3607','3608','3609','3610','3611','3612','3613','3614','3808','3809','3810','3811','3812','3813','3814','3815','4010','4011','4012','4013','4014','4015','4016','4211','4212','4213','4214','4215','4216','4413','4414','4415','4416','4417','4418','4614','4615','4616')
 
sizes = c('4012','3610','4414')

colors = c('red','yellow','green','black','brown','purple','pink','maroon','navy','orange')
par(new=FALSE)

for(i in 1:length(sizes)) {
  size = sizes[i]
  bi = unique(b[b$size == size,])
  yi <- bi[,15:16]
  plot(yi,col=colors[i] )
  par(new=TRUE)
}

par(new=FALSE)

for(i in 1:length(sizes)) {
  size = sizes[i]
  bi = unique(b[b$size == size,])
  yi <- bi[,15:16]
  plot(yi,col=colors[i])
  text(yi,labels=bi$winner_bra_id)
  par(new=TRUE)
}