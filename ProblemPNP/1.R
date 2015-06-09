#EuclideanDistance

PrepareData <- function(filename){
  temp <- read.csv2(filename,sep=",",header=FALSE)
  temp <- as.matrix(temp)
  temp <- apply(temp,2,as.numeric)
  return (temp)
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

JoinData <- function(S1,S2,delta){
  if(nrow(S1) > 0 && nrow(S2) > 0){
    m <- nrow(S2)
    j <- 1
    jprim <- 1
    tempS2 <- S2[j,2]
    vect <- c(delta)
    for(i in 1:nrow(S1)){
      tempS1 <- S1[i,2]
      while((abs(tempS1 - tempS2)) <= delta && j <= nrow(S2)){
        x <- S1[i,]
        y <- S2[j,]
        result <- EuclideanDistance(x,y)
        vect <- c(vect,result)
        j <- j + 1
        if(j <= nrow(S2)){
          tempS2 <- S2[j,2]
        }
      }
      bound <- S2[jprim,2] + 2 * delta
      while(S2[jprim,2] <= bound && jprim < nrow(S2)){
        jprim <- jprim + 1
      }
      j <- jprim
    }
    return (min(vect))
  }
  else{
    return (delta)
  }
}

AppointS <- function(centerLine,data,delta){
  temp <- data[abs(centerLine - data[,1]) <= delta, ]
  temp <- matrix(temp,ncol = 2)
  #temp <- SortByColumn(temp,2)
  return (temp)
}

CPAIR2 <- function(dataX,dataY){
  if(nrow(dataX) == 2){
    x <- dataX[1,]
    y <- dataX[2,]
    return(EuclideanDistance(x,y))
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

    delta <- min(result1,result2)

    S1 <- AppointS(centerLine,dataY.splitOne,delta)
    S2 <- AppointS(centerLine,dataY.splitTwo,delta)
    
    #Join
    result <- JoinData(S1,S2,delta)
    return (result)
  }
}



data <- PrepareData("1.txt")
data <- m
#Sort data by coordinates
dataX <- SortByColumn(data,1)
dataY <- SortByColumn(data,2)

start <- proc.time()

#Compute result
a <- CPAIR2(dataX,dataY)
print(a)

stopP <- proc.time() - start
print(paste("CZAS: ",round(stopP[1],2),round(nrow(data) * log(nrow(data))/stopP[1])))

