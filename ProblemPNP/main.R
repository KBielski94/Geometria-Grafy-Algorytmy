#EuclideanDistance

PrepareData <- function(filename){
  temp <- read.csv2(filename,sep=",",header=FALSE)
  temp <- as.matrix(temp)
  temp <- apply(temp,2,as.numeric)
  return (temp)
}

ComputeDistanceByResult <- function(result){
  if(!all(is.infinite(result))){
    distance <- EuclideanDistance(result[1,],result[2,])
  }
  else{
    distance <- result
  }
  return (distance)
}

EuclideanDistance <- function(rowOne,rowTwo){
  x <- as.numeric(rowOne)
  y <- as.numeric(rowTwo)
  return(sqrt(sum((x-y)^2)))
}

SortByColumn <- function(matrix,column){
  if(nrow(matrix) > 1){
    matrix <- matrix[order(matrix[,column]), ]
    return (matrix)
  }
  else{
    return (matrix)
  }
}

JoinData <- function(S1,S2,point,delta){
  if(nrow(S1) > 0 && nrow(S2) > 0){
    m <- nrow(S2)
    j <- 1
    jprim <- 1
    tempS2 <- S2[j,2]
    vect <- delta
    for(i in 1:nrow(S1)){
      tempS1 <- S1[i,2]
      while((abs(tempS1 - tempS2)) <= delta && j <= nrow(S2)){
        x <- S1[i,]
        y <- S2[j,]
        result <- EuclideanDistance(x,y)
        if(result < vect){
          vect <- result
          point <- rbind(x,y)
        }
        j <- j + 1
        if(j <= nrow(S2)){
          tempS2 <- S2[j,2]
        }
      }
      if(i < nrow(S1)){
        bound <- S1[i+1] - delta
      }
      else{
        bound <- S1[i] - delta
      }
      while(S2[jprim,2] < bound && jprim < nrow(S2)){
        jprim <- jprim + 1
      }
      j <- jprim
    }
    return (point)
  }
  else{
    return (point)
  }
}

AppointS <- function(centerLine,data,delta){
  temp <- data[abs(centerLine - data[,1]) <= delta, ]
  temp <- matrix(temp,ncol = 2)
  return (temp)
}

CPAIR2 <- function(dataX,dataY){
  if(nrow(dataX) == 2){
    x <- dataX[1,]
    y <- dataX[2,]
    return (dataX)
  }
  else if(nrow(dataX) == 1){
    return (Inf)
  }
  else{
    center <- floor(nrow(dataX) / 2.0)
    centerLine <- dataX[center,1]
    
    dataX.splitOne <- matrix(dataX[1:center,],ncol = 2)
    dataX.splitTwo <- matrix(dataX[(center + 1):nrow(dataX),], ncol = 2)
    
    dataY.splitOne <- matrix(dataY[dataY[,1] <= centerLine, ], ncol = 2)
    dataY.splitTwo <- matrix(dataY[dataY[,1] >  centerLine, ], ncol = 2)
    
    #Divide
    result1 <- CPAIR2(dataX.splitOne, dataY.splitOne)
    result2 <- CPAIR2(dataX.splitTwo, dataY.splitTwo)
    
    distance1 <- ComputeDistanceByResult(result1)
    distance2 <- ComputeDistanceByResult(result2)

    #Find delta and point with delta distance
    if(distance1 < distance2){
      point <- result1
      delta <- distance1
    }
    else{
      point <- result2
      delta <- distance2
    }
    
    S1 <- AppointS(centerLine,dataY.splitOne,delta)
    S2 <- AppointS(centerLine,dataY.splitTwo,delta)
    
    #Join
    result <- JoinData(S1,S2,point,delta)
    return (result)
  }
}

PrintResult <- function(data,result){
  print(paste("Points with minimum distance is ",result[1,1],result[1,2],"and",result[2,1],result[2,2]))
  print(paste("Minimum distance is ", round(EuclideanDistance(result[1,],result[2,]),4)))
  x11()
  plot(data)
  points(result[1,1],result[1,2],pch = 16)
  points(result[2,1],result[2,2],pch = 16)
  segments(result[1,1],result[1,2],result[2,1],result[2,2],lty = 1,lwd = 3,col = "black")
  message("Prees Return to quit")
  invisible(readLines("stdin",n = 1))
}

main <- function(filename){
  data <- PrepareData(filename)
  #Sort data by coordinates
  dataX <- SortByColumn(data,1)
  dataY <- SortByColumn(data,2)
  #Compute result
  result <- CPAIR2(dataX,dataY)
  PrintResult(data,result)
}

args = commandArgs(trailingOnly = TRUE)
if(is.na(args[1]) == TRUE ){
  stop("Correct usage: Rscript main.R <input file> <plik ze wzorcem> ")
}

main(as.character(args[1]))
