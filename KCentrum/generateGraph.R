

generateGraph <- function(numberVertex,filename){
  require(igraph)
  graph <- graph.union(graph.ring(numberVertex-1),
                       graph.star(numberVertex, center=numberVertex, mode="undirected"))
  E(graph)$weight <- sample(1:length(E(graph))+5,length(E(graph)),replace=TRUE)
  adjMatrix <- as.matrix(get.adjacency(graph,attr='weight'))
  write.table(adjMatrix,filename,col.names=FALSE,row.names=FALSE)
}


generateGraph(6,"test7.txt")
