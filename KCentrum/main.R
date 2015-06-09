

GetFirstRow <- function(filename){
  row <- read.table(filename,nrows=1,header=FALSE,sep=" ")
  return (row)
}

GetAdjMatrix <- function(filename){
  tempMatrix <- read.table(filename,skip=1,header=FALSE)
  tempMatrix <- as.matrix(tempMatrix)
  colnames(tempMatrix) <- 1:nrow(tempMatrix)
  rownames(tempMatrix) <- 1:nrow(tempMatrix)
  return(tempMatrix)
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

GetEdges <- function(matrix){
  list <- c()
  n <- nrow(matrix)
  for (i in 1:(n-1)){
    for(j in (i+1):n){
      if(matrix[i,j] != 0)
        list <- rbind(list,c(i,j,matrix[i,j]))
    }
  }
  return (list)
}

CreateIGraph <- function(n,i,edges){
  row <- rep(0,n)
  matrix <- matrix(row,ncol=n,nrow=n)
  for(i in 1:i){
    r <- edges[i,1]
    c <- edges[i,2]
    matrix[r,c] <- 1
    matrix[c,r] <- 1
  }
  return (matrix)
}

DeleteVertex <- function(matrix,vertex){
  n <- nrow(matrix)
  tempMatrix <- matrix
  for(i in ((vertex):n)){
    if(matrix[vertex,i] != 0){
      element <- (as.integer(rownames(matrix)[i]))
      k <- length(rownames(matrix))
      for(j in (vertex:k)){
        tempElement <- (as.integer(rownames(tempMatrix)[j]))
        if(element == tempElement){
          tempMatrix <- as.matrix(tempMatrix[-j,-j])
          break
        }
      }
    }
  }
  if(nrow(tempMatrix) == 1){
    return (NULL)
  }
  else if(nrow(tempMatrix) == 2){
    element <- as.integer(rownames(tempMatrix)[2])
    tempMatrix <- as.matrix(tempMatrix[-vertex,-vertex])
    rownames(tempMatrix) <- element
    colnames(tempMatrix) <- element
    return (tempMatrix)
  }
  else{
    tempMatrix <- as.matrix(tempMatrix[-vertex,-vertex])
    return (tempMatrix)
  }
}

FindIndependentSet <- function(matrix){
  list <- c()
  while(!is.null(matrix) && nrow(matrix) != 1 ){
    element <- as.integer(rownames(matrix)[1])
    list <- c(list,element)
    matrix <- DeleteVertex(matrix,1)
  }
  if(is.null(matrix)){
    return (list)
  }
  else if(nrow(matrix) == 1){
    element <- as.integer(rownames(matrix)[1])
    list <- c(list,element)
  }
  return (list)
}

CreateSquareMatrix <- function(matrix){
  n <- nrow(matrix)
  tempMatrix <- matrix %*% matrix
  tempMatrix + matrix
  diag(tempMatrix) <- 0
  rownames(tempMatrix) <- 1:n
  colnames(tempMatrix) <- 1:n
  return (tempMatrix)
}

Kcenter <- function(adjMatrix,edges,k){
  n <- nrow(adjMatrix)
  nEdge <- nrow(edges)
  index <- Inf
  for(i in 1:nEdge){
    newMatrix <- CreateIGraph(n,i,edges)
    squareMatrix <- CreateSquareMatrix(newMatrix) 
    independentSet <- FindIndependentSet(squareMatrix)
    if(length(independentSet) <= k && index > i){
      index <- i
      result <- independentSet
    }
  }
  return (result)
}

PlotResult <- function(matrix,independentSet){
  require(igraph)
  graph <- graph.adjacency(matrix,mode="undirected", weighted=TRUE)
  V(graph)[independentSet]$color <- "green"  
  n <- length(independentSet)
  mess <- paste(n,"- center of graph is ",independentSet,sep="")
 
  plot(graph, edge.label=round(E(graph)$weight,3),main=mess)
}

main <- function(filename){
  row <- GetFirstRow(filename)
  k <- row[1,1]
  n <- row[1,2]
  adjMatrix <- GetAdjMatrix(filename)
  edges <- GetEdges(adjMatrix)
  edges <- SortByColumn(edges,3)
  result <- Kcenter(adjMatrix,edges,k)
  x11()
  PlotResult(adjMatrix,result)
  message("Prees Return to quit")
  invisible(readLines("stdin",n = 1))
}

args = commandArgs(trailingOnly = TRUE)
if(is.na(args[1]) == TRUE){
  stop("Correct usage: Rscript main.R <filename> ")
}

if(as.character(args[1]) == "demo"){
  for (i in 1:7){
    filename = paste("examples/test",i,".txt",sep="")
    main(filename)
  }
}
if(as.character(args[1]) != "demo"){
  main(as.character(args[1]))
}


