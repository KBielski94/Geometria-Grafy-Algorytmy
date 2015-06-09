library(methods)

setwd("~/Dropbox/Studia/Geometria,Grafy,Algorytmy/Xmono")
source('~/Dropbox/Studia/Geometria,Grafy,Algorytmy/Xmono/stackClass.R')

#Stack function
StackEmpty <- function(stack){
  if(stack@state == 0)
    return (TRUE)
  else
    return (FALSE)
}

Push <- function(stack,element){
  stack@state <- stack@state + 1
  stack@list[[stack@state]] <- NULL
  stack@list[[stack@state]] <- element
  return (stack)
}

Pop <- function(stack){
  if(StackEmpty(stack)){
    warning("Stack empty!")
  }
  else{
    stack@state <- stack@state - 1
    return (stack)
  }
    
}

PopElement <- function(stack){
  element <- stack@list[[stack@state + 1]]
  return (element)
}

PrepareData <- function(filename){
  temp <- read.csv2(filename,sep=",",header=FALSE)
  temp <- as.matrix(temp)
  temp <- apply(temp,2,as.numeric)
  temp <- cbind(temp,0)
  colnames(temp) <- c("X","Y","Chain")
  return (temp)
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

OrientationPoint <- function(points){
  temp <- points[,1:2]
  temp <- cbind(temp,1)
  det <- determinant(temp,logarithm = FALSE)$sign
  if(det > 0)
    return (1)
  else if(det == 0)
    return (0)
  else 
    return (-1)
}

CreateChain <- function(data){
  data[1,3] <- -1
  axis <- data[1,1]
  for(i in 2:nrow(data)){
    if(axis < data[i,1]){
      data[i,3] = 1
    }
    else{
      data[1,3] = 1
      data[i,3] = -1
    }
    axis <- data[i,1]
  }
  return (data)
}

SweepTriangulation <- function(data){
  stack <- new("Stack",state = 0)
  stack <- Push(stack,data[1,])
  stack <- Push(stack,data[2,])
  n <- nrow(data)
  for (i in 3:(n-1)){
    #print(paste("DATA=",data[i,]))
    tempV <- stack@list[[stack@state]]
    if(data[i,3] != tempV[3]){
      #print(stack)
      while(stack@state != 1){
        stack <- Pop(stack)
        tempW <- PopElement(stack)
        #print(paste("TempW=",tempW))
        segments(data[i,1],data[i,2],tempW[1],tempW[2])
      }
      stack <- Pop(stack) 
      stack <- Push(stack,data[i,])
      stack <- Push(stack,tempW)

    }
    else{
      stack <- Pop(stack)
      tempW <- PopElement(stack)
      while(TRUE){
        stack <- Pop(stack)
        tempU <- PopElement(stack)
        points <- rbind(data[i,],tempW,tempU)
        #print(points)
        orientation <- OrientationPoint(points)
        #print(orientation)
        if(data[i,3] == 1 && orientation < 0){
          segments(data[i,1],data[i,2],tempU[1],tempU[2])
          tempW <- tempU
        }
        else if(data[i,3] == -1 && orientation > 0){
          segments(data[i,1],data[i,2],tempU[1],tempU[2])
          tempW <- tempU
        }
        else{
          break
        }
        if(StackEmpty(stack)){
          break
        }
      }
      stack <- Push(stack,tempU)
      stack <- Push(stack,data[i,])
    }
  }
  #print(stack)
  stack <- Pop(stack)
  while(stack@state != 1){
    stack <- Pop(stack)
    v <- PopElement(stack)
    segments(data[i+1,1],data[i+1,2],v[1],v[2])
  }
}

PrintPolygon <- function(data){
  plot(data[,1:2])
  for(i in 1:(nrow(data)-1)){
    segments(data[i,1],data[i,2],data[i+1,1],data[i+1,2],lty = 1,lwd = 3,col = "black")
  }
  segments(data[1,1],data[1,2],data[i+1,1],data[i+1,2],lty = 1,lwd = 3,col = "black")
}

main <- function(filename){
  data <- PrepareData(filename)
  x11()
  PrintPolygon(data)
  data <- CreateChain(data)
  data <- SortByColumn(data,1)
  SweepTriangulation(data)
  message("Prees Return to quit")
  invisible(readLines("stdin",n = 1))
}

args = commandArgs(trailingOnly = TRUE)
if(is.na(args[1]) == TRUE){
  stop("Correct usage: Rscript main.R <filename> ")
}

if(as.character(args[1]) == "demo"){
  for (i in 1:5){
    filename = paste("examples/example",i,".txt")
    main(filename)
  }
}
if(as.character(args[1]) != "demo"){
    main(as.character(args[1]))
}


