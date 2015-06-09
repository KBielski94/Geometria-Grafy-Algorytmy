library(methods)

setwd("~/Dropbox/Studia/Geometria,Grafy,Algorytmy/ProblemOkienkowania")
source('~/Dropbox/Studia/Geometria,Grafy,Algorytmy/ProblemOkienkowania/classTree.R')

PrepareData <- function(filename){
  temp <- read.csv2(filename,sep=",",header=FALSE)
  temp <- as.matrix(temp)
  temp <- apply(temp,2,as.numeric)
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

BuildStowTree <- function(Py){
  if(nrow(Py) == 1){
    tempNode <- new("NodeStow",elem = Py[1,])
    return (tempNode)
  }
  else{
    i <- 1
    center <- floor(nrow(Py) / 2.0)
      splitPy <- matrix(Py[1:center,],ncol = 2)
      leftChild <- BuildStowTree(splitPy)
      i <- i + 1
      splitPyTwo <- matrix(Py[(center+1):nrow(Py),],ncol = 2)
      rightParent <- BuildStowTree(splitPyTwo)
      parent <- new("NodeStow",elem = Py[i,],left = leftChild,right = rightParent)
      return (parent)
  }
}

BuildStructure <- function(Px,Py){
  if(nrow(Px) == 1){
    stow <- new("NodeStow",elem = Px[1,])
    tempNode <- new("Node",elem = Px[1,], stow = stow)
    return(tempNode)
  }
  else{
    center <- floor(nrow(Px) / 2.0) 
    centerLine <- Px[center,1]
    
    PxOne <- matrix(Px[1:center,],ncol = 2)
    PxTwo <- matrix(Px[(center + 1):nrow(Px),], ncol = 2)
    
    PyOne <- matrix(Py[Py[,1] <= centerLine, ], ncol = 2)
    PyTwo <- matrix(Py[Py[,1] >  centerLine, ], ncol = 2)
    
    nodeLeft <- BuildStructure(PxOne,PyOne)
    nodeRight <- BuildStructure(PxTwo,PyTwo)
    tempStow <- BuildStowTree(Py)
  }
  temp <- new("Node",elem = Px[center,],left = nodeLeft,right = nodeRight, stow = tempStow)
  return (temp)
}

IsLeaf <- function(node){
    condition <- ((class(node@right) == "Empty") && (class(node@left) == "Empty"))
    if(condition == TRUE){
      return (TRUE)
    }
    else{
      return (FALSE)
    }
}

ReportSubtree <- function(node){
  if(IsLeaf(node)){
    return (node@elem)
  }
  else{
    ReportSubtree(node@left)
    ReportSubtree(node@right)
  }
}

FindSplitNode <- function(Tree,col,left,right){
  if(left > right){
    stop("Left is greater then right!")
  }
  else{
    v <- Tree
    while(!IsLeaf(v) && (right <= v@elem[col] || left > v@elem[col])){
      if(right <= v@elem[col]){
        v <- v@left
      }
      else{
        v <- v@right
      }
    }
    return (v)
  }
}

OneDRangeQuery <- function(tree,col,x1,x2){
  list <- c()
  node_median <- FindSplitNode(tree,col,x1,x2)
  if(IsLeaf(node_median)){
    if(node_median@elem[col] >= x1 && node_median@elem[col] <= x2){
      list <- rbind(list,node_median@elem)
    }
  }
  else{
    v <- node_median@left
    while(!IsLeaf(v)){
      if(x1 <= v@elem[col]){
       listTemp <- ReportSubtree(v@right)
       list <- rbind(list,listTemp)
        v <- v@left
      }
      else{
        v <- v@right
      }
    }
    if(v@elem[col] >= x1 && v@elem[col] <= x2){
      list <- rbind(list,v@elem)
    }
    
    w <- node_median@right
    while(!IsLeaf(w)){
      if(w@elem[col] <= x2){
        listTemp <- ReportSubtree(w@left)
        list <- rbind(list,listTemp)
        w <- w@right
      }
      else{
        w <- w@left
      }
    }
    if(w@elem[col] >= x1 && w@elem[col] <= x2){
      list <- rbind(list,w@elem)
    }
  }
  return (list)
}

PointInPolygon <- function(point,x1,y1,x2,y2){
  a <-  (point[1] >= x1 && point[1] <= x2)
  b <-  (point[2] >= y1 && point[2] <= y2)  
  return (a && b)
}

TwoDRangeQuery <- function (tree,x1,x2,y1,y2){
  list <- c()
  node_median <- FindSplitNode(tree,1,x1,y1)
  if(IsLeaf(node_median)){
    if(PointInPolygon(node_median@elem,x1,x2,y1,y2)){
      list <- rbind(list,node_median@elem)
    }
  }
  else{
    v <- node_median@left
    while(!IsLeaf(v)){
      if(x1 <= v@elem[1]){
        listTemp <- OneDRangeQuery(v@right@stow,2,x2,y2)
        list <- rbind(list,listTemp)
        v <- v@left
      }
      else{
        v <- v@right
      }
    }
    if(PointInPolygon(v@elem,x1,x2,y1,y2)){
      list <- rbind(list,v@elem)
    }
    w <- node_median@right
    while(!IsLeaf(w)){
      if(w@elem[1] <= y1){
        listTemp <- OneDRangeQuery(w@left@stow,2,x2,y2)
        list <- rbind(list,listTemp)
        w <- w@right
      }
      else{
        w <- w@left
      }
    }
    if(PointInPolygon(w@elem,x1,x2,y1,y2)){
      list <- rbind(list,w@elem)
    }
  }
  return (list)
}

PrintResult <- function(data,points,x1,y1,x2,y2){
  print(paste("Points in",x1,"x",y1,"and",x2,"x",y2,"is "))
  #colnames(points) <- c("x","y")
  print(points)
  x11()
  plot(data)
  points(points,pch = 16)
  rect(x1,y1,x2,y2,border = 2,lwd = 4)
  message("Prees Return to quit")
  invisible(readLines("stdin",n = 1))
}

main <- function(filename,x1,y1,x2,y2){
  data <- PrepareData(filename)
  #Sort data by coordinates
  Px <- SortByColumn(data,1)
  Py <- SortByColumn(data,2)
  #Compute result
  treeStructure <- BuildStructure(Px,Py)
  result <- TwoDRangeQuery(treeStructure,x1,y1,x2,y2)
  PrintResult(data,result,x1,y1,x2,y2)
}

args = commandArgs(trailingOnly = TRUE)
if(is.na(args[1]) == TRUE || is.na(args[2]) == TRUE || is.na(args[3]) == TRUE || is.na(args[4]) == TRUE || is.na(args[5]) == TRUE){
  stop("Correct usage: Rscript main.R <input file> <x1> <y1> <x2> <y2> ")
}

main(as.character(args[1]),as.numeric(args[2]),as.numeric(args[3]),as.numeric(args[4]),as.numeric(args[5]))
