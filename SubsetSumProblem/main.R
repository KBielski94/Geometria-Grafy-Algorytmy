
Trim <- function(list,delta){
  m <- length(list)
  listPrim <- c(list[1])
  last <- list[1]
  for(i in 2:m){
    if(list[i] > (last*(1+delta))){
      listPrim <- c(listPrim,list[i])
      last <- list[i]
    }
  }
  return (listPrim)
}

AddElement <- function(list,element){
  n <- length(list)
  if(n == 0){
    return(c(0,element))
  }
  else{
    for(i in 1:n){
      list[i] <- list[i] + element
    }
    return (list)
  }
}

MergeList <- function(listOne,listTwo){
  listTemp <- c(listOne,listTwo)
  listTemp <- sort(listTemp)
  listTemp <- unique(listTemp)
  return (listTemp)
}

DeleteValues <- function(list,t){
  list <- list[list <= t]
  return (list)
}

ApproxSubsetSum <- function(S,t,epsilon){
  n <- length(S)
  list <- c(0)
  for(i in 1:n){
    listAddTemp <- AddElement(list,S[i])
    list <- MergeList(list,listAddTemp)
    list <- Trim(list,epsilon/(2*n))
    list <- DeleteValues(list,t)
  }
  return (max(list))
}

ExactSubsetSum <- function(S,t){
  n <- length(S)
  list <- c(0)
  for (i in 1:n){
    list <- MergeList(list,AddElement(list,S[i]))
    list <- DeleteValues(list,t)
  }
  return (max(list))
}

Interval <- function(S,a,b){
  eps <- (b-a)/b
  result <- ApproxSubsetSum(S,b,eps)
  if(result >= a){
    print(result)
  }
  else{
    print(paste("Subset sum in interval[",a,",",b,"] doesn't exist",sep=""))
  }
}

main <- function(filename){
  list <- read.table(filename,nrows=1,header=FALSE,sep=" ")
  list <- sort(as.numeric(list[1,]))
  t <- read.table(filename,skip=1,header=FALSE)
  t <- t[1,1]
  eps <- read.table(filename,skip=2,header=FALSE)
  eps <- eps[1,1]
  result <- ApproxSubsetSum(list,t,eps)
  optimum <- ExactSubsetSum(list,t)
  print("===================")
  print(list)
  print(paste("t=",t,"epsilon=",eps))
  print(paste("Sum of the subset: ",result))
  print(paste("Optimum solution: ",optimum))
  print("===================")
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
if(as.character(args[1]) != "demo"){
  main(as.character(args[1]))
}

#a <- c(104,102,201,101)
#a <- c(1,2,3,4)
#Interval(a,505,650)


