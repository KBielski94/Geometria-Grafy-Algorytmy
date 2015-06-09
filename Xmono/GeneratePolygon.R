
main <- function(filename,howPoints){
  dat <- matrix(stats::rnorm(howPoints), ncol = 2)
  ch <- chull(dat)
  coords <- dat[c(ch, ch[1]), ]
  temp <- apply(coords[2:nrow(coords),],2,rev)
  data <- rbind(coords[1,],temp)
  data <- unique(data)
  write.table(data,file=filename,sep=",",col.names=FALSE,row.names=FALSE)
}

for (i in 1:5){
  filename = paste("examples/example",i,".txt")
  main(filename,200)
}

