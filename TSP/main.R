GetData <- function(filename){
  temp <- read.csv2(filename,sep=",",header=FALSE)
  temp <- as.matrix(temp)
  temp <- apply(temp,2,as.numeric)
  colnames(temp) <- c("X","Y")
  return (temp)
}

EuclideanDistance <- function(a,b){
  x <- as.numeric(a)
  y <- as.numeric(b)
  return(sqrt(sum((x-y)^2)))
}

CreateAdjMatrix <- function(data){
  n <- nrow(data)
  tempMatrix <- matrix(rep(0,n),nrow = n,ncol = n)
  return (tempMatrix)
}

FillAdjMatrix <- function(matrix,data){
  n <- nrow(matrix)
  for(i in 1:n){
    for(j in 1:n){
      if(i != j){
        matrix[i,j] <- EuclideanDistance(data[i,],data[j,])
      }
      else{
        matrix[i,j] <- 0
      }
    }
  }
  return (matrix)
}

CreateDynTable <- function(size){
  n <- 2^size-1
  tempMatrix <- matrix(rep(0,size*n),nrow = size,ncol = n)
  return (tempMatrix)
}

SizeOfOnes <- function(number){
  k <- 0
  while(number != 0){
    if( number %% 2 == 1){
      k <- k + 1
    }
    number <- floor(number / 2)
  }
  return(k)
}

Fun <- function(adjMatrix,dp){
  n <- nrow(adjMatrix)
  dp[1,1] <- 0
  for (s in 2:n){
    for (S in 1:2^n-1){
      if(SizeOfOnes(S) == s && bitwAnd(S,1) != 0){
        dp[1,S] <- Inf
        #j \in S j != 1
        for(j in 2:n){
          minimum <- Inf
          shiftJ <- bitwShiftL(1,j-1)
          if(bitwAnd(shiftJ,S) != 0){
            xor <- bitwXor(S,shiftJ) # S \ {j}
            for(i in 1:n){
              shiftI <- bitwShiftL(1,i-1)
              if(i != j && bitwAnd(S,shiftI) != 0){ # i \in S i != j
                resultTemp <- dp[i,xor] + adjMatrix[i,j]
                if(minimum > resultTemp){
                  minimum <- resultTemp
                }
              }
            }
          }
          dp[j,S] <- minimum
        }
      }
    }
  }
  return(dp)
}

ShowPath <- function(adjMatrix,dp,row,S){
  listTemp <- list()
  n <- nrow(adjMatrix)
  if(SizeOfOnes(S) == 1){
    for(i in 2:n){
      shiftI <- bitwShiftL(1,i-1)
      if(bitwAnd(S,shiftI) != 0){
        return(i)
      }
    }
  }
  else{
    xorTemp <- 0
    vertexI <- 0
    minimum <- Inf
      shiftJ <- bitwShiftL(1,row-1)
      # j \in S
      if(bitwAnd(shiftJ,S) != 0){
        xor <- bitwXor(S,shiftJ) # S \ {j}
        for(i in 1:n){
          shiftI <- bitwShiftL(1,i-1)
          if(i != row && bitwAnd(S,shiftI) != 0){ # i \in S i != j
            resultTemp <- dp[i,xor] + adjMatrix[i,row]
            if(minimum > resultTemp){
              minimum <- resultTemp
              vertexI <- i
              xorTemp <- xor
            }
          }
        }
      }
    listTemp <- c(listTemp,row)
    listTemp <- c(listTemp,ShowPath(adjMatrix,dp,vertexI,xorTemp))
  }
  return (listTemp)
}

PlotResult <- function(data,path,minimum){
  plot(data,lwd = 5)
  text(data,labels=c(1:nrow(data)),cex=2,pos=3)
  n <- nrow(data)
  for(i in 1:n){
    for(j in 1:n){
      if(i != j){
        segments(data[i,1],data[i,2],data[j,1],data[j,2])
      }
    }
  }
  m <- length(path)
  last <- 1
  a <- paste("1 -> ")
  for(i in 2:(m-1)){
    a <- paste(a,paste(path[[i]], "-> "))
    segments(data[last,1],data[last,2],data[path[[i]],1],data[path[[i]],2],lwd=5)
    last <- path[[i]]
  }
  segments(data[1,1],data[1,2],data[last,1],data[last,2],lwd=5)
  a <- paste(a,paste("1"))
  a <- paste(a,"Length of cycle: ",round(minimum,3),sep="\n")
  title(a)
}

ShowResult <- function(data,adjMatrix,dp){
  n <- nrow(adjMatrix)
  S <- ncol(dp)
  minimum <- Inf
  row <- 1
  listPath <- list(1)
  for(i in 1:n){
    resultTemp <- dp[i,S] + adjMatrix[i,1]
    if(minimum > resultTemp){
      minimum <- resultTemp
      row <- i
    }
  }
  a <- ShowPath(adjMatrix,dp,row,S)
  resultPath <- c(1,a,1)
  PlotResult(data,resultPath,minimum)
}



main <- function(filename){
  data <- GetData(filename)
  x11()
  adjMatrix <- CreateAdjMatrix(data)
  adjMatrix <- FillAdjMatrix(adjMatrix,data)
  dynMatrix <- CreateDynTable(nrow(adjMatrix))
  dynMatrix <- Fun(adjMatrix,dynMatrix)
  ShowResult(data,adjMatrix,dynMatrix)
  message("Prees Return to quit")
  invisible(readLines("stdin",n = 1))
}

args = commandArgs(trailingOnly = TRUE)
if(is.na(args[1]) == TRUE){
  stop("Correct usage: Rscript main.R <filename> ")
}

if(as.character(args[1]) == "demo"){
  for (i in 1:5){
    filename = paste("examples/test",i,".csv",sep="")
    main(filename)
  }
}
if(as.character(args[1]) != "demo"){
  main(as.character(args[1]))
}



