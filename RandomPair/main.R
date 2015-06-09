library(methods)

source('dataObjects.R')

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

ComputeOneIndex <- function(point,delta){
  a <- floor(point[1]/delta)
  b <- floor(point[2]/delta)
  result <- c(a,b)
  return(result)
}

AddNodeToTree <- function(Tree,node){
  if(class(Tree) == "Empty" || is.null(Tree@elem)){
    return (node)
  }
  else if(node@elem@index[1] < Tree@elem@index[1] && node@elem@index[2] < Tree@elem@index[2]){
    Tree@left <- AddNodeToTree(Tree@left,node)
  }
  else{
    Tree@right <- AddNodeToTree(Tree@right,node)
  }
  return (Tree)
}

AddPointToTree <- function(index,tree,point){
  condition <- tree@elem@index[1] == index[1] && tree@elem@index[2] == index[2]
  if(condition){
    tree@elem@listPoints <- c(tree@elem@listPoints,list(point))
    return (tree)
  }
  else{
    if(index[1] < tree@elem@index[1] && index[2] < tree@elem@index[2]){
      tree@left <- AddPointToTree(index,tree@left,point)
    }
    else{
      tree@right <- AddPointToTree(index,tree@right,point)
    }
    return (tree)
  }
}

ComputeAllIndex <- function(points,delta,index){
  tree <<- new("Node")
  for (i in 1:index){
    tempIndex <- ComputeOneIndex(points[i,],delta)
    nodeIndex <- new("Indexes",index=tempIndex,listPoints=list(points[i,]))
    node <- new("Node",elem=nodeIndex)
    if(SearchTree(tempIndex,tree)){
      tree <<- AddPointToTree(tempIndex,tree,points[i,])
    }
    else{
      tree <<- AddNodeToTree(tree,node)
    }
  }
  return(tree)
}

FindPoints <- function(point,index,tree,points){
    minResult <- Inf
    condition <- tree@elem@index[1] == index[1] && tree@elem@index[2] == index[2]
    if(condition){
      n <- length(tree@elem@listPoints)
      for(j in 1:n){
        result <- EuclideanDistance(point,tree@elem@listPoints[[j]])
        minResult <- min(minResult,result)
        return(result)
      }
    }
    else{
      conditionTwo <- index[1] < tree@elem@index[1] && index[2] < tree@elem@index[2]
      if(conditionTwo){
        result <- FindPoints(point,index,tree@left,points)
      }
      else{
        result <- FindPoints(point,index,tree@right,points)
      }
      return(result)
    }
}

ComputeIndexes <- function(tempIndex){
  oneInd <- c(tempIndex[1]-1,tempIndex[2])
  twoInd <- c(tempIndex[1],tempIndex[2]-1)
  threeInd <- c(tempIndex[1]+1,tempIndex[2])
  fourInd <- c(tempIndex[1],tempIndex[2]+1)
  fiveInd <- c(tempIndex[1]+1,tempIndex[2]+1)
  sixInd <- c(tempIndex[1]-1,tempIndex[2]-1)
  sevenInd <- c(tempIndex[1]+1,tempIndex[2]-1)
  eightInd <- c(tempIndex[1]-1,tempIndex[2]+1)
  nineInd <- c(tempIndex[1],tempIndex[2])
  result <- list(oneInd,twoInd,threeInd,fourInd,fiveInd,sixInd,sevenInd,eightInd,nineInd)
  return(result)
}

MinimumWithIndex <- function(point,tempIndex,tree,points){
  resultList <- ComputeIndexes(tempIndex)
  result <- Inf
  for(i in 1:9){
    temp <- resultList[[i]]
    if(SearchTree(temp,tree)){
      result <- min(result,FindPoints(point,temp,tree,points))
    }
  }
  return (result)
}

FindCorrectPoint <- function(point,delta,points,index){
  for (i in 1:index){
    if(EuclideanDistance(point,points[i,]) == delta){
      return(points[i,])
    }
  }
}

SearchTree <- function(index,tree){
  if(class(tree) == "Empty" || is.null(tree@elem)){
    return (FALSE)
  }
  condition <- tree@elem@index[1] == index[1] && tree@elem@index[2] == index[2]
  if(condition){
    return (TRUE)
  }
  if(index[1] < tree@elem@index[1] && index[2] < tree@elem@index[2]){
    return(SearchTree(index,tree@left))
  }
  else{
    return(SearchTree(index,tree@right))
  }
}

RandomPair <- function(data){
  n <- nrow(data) 
  delta <- EuclideanDistance(data[1,],data[2,])
  resultPoints <- list(data[1,],data[2,])
  tree <- ComputeAllIndex(data,delta,2)
  for(i in 3:n){
   tempIndex <- ComputeOneIndex(data[i,],delta)
   h <- MinimumWithIndex(data[i,],tempIndex,tree,data)
    newDelta <- min(delta,h)
    if(newDelta < delta) {
      newPoint <- FindCorrectPoint(data[i,],newDelta,data,i)
      resultPoints <- list(data[i,],newPoint)
    }
    if(newDelta == delta){
      if(SearchTree(tempIndex,tree)){
        tree <- AddPointToTree(tempIndex,tree,data[i,])
      }
      else{
        element <- new("Indexes",index=tempIndex,listPoints=list(data[i,]))
        node <- new("Node",elem=element)
        tree <- AddNodeToTree(tree,node)
      }
    }
    else{
      rm(tree)
      delta <- newDelta
      tree <- ComputeAllIndex(data,delta,i)
    }
  }
  return (resultPoints)
}

PrintResult <- function(data,result){
  a <- paste("Minimum distance is ", round(EuclideanDistance(result[[1]],result[[2]]),4))
  x11()
  plot(data)
  points(result[[1]][1],result[[1]][2],pch = 16)
  points(result[[2]][1],result[[2]][2],pch = 16)
  segments(result[[1]][1],result[[1]][2],result[[2]][1],result[[2]][2],lty = 1,lwd = 3,col = "black")
  title(a)
  message("Prees Return to quit")
  invisible(readLines("stdin",n = 1))
}

PermutationData <- function(data){
  n <- nrow(data)
  newData <- matrix(ncol=2)
  randomIndex <- sample(1:n,size=n)
  for(i in 1:n){
    newData <- rbind(newData,data[randomIndex[i],])
  }
  return (newData[2:(n+1),])
}

main <- function(filename){
  data <- PrepareData(filename)
  data2 <- PermutationData(data)
  plot(data2)
  result <- RandomPair(data2)
  PrintResult(data,result)
}

args = commandArgs(trailingOnly = TRUE)
if(is.na(args[1]) == TRUE){
  stop("Correct usage: Rscript main.R <filename> ")
}

if(as.character(args[1]) == "demo"){
  for (i in 1:5){
    filename = paste("examples/test",i,".txt",sep="")
    main(filename)
  }
}
if (as.character(args[1]) != "demo"){
  main(as.character(args[1]))
}

