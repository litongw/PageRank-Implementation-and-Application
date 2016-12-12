
# Implementing PageRank
setwd("~/R files")
library(igraph)
library(Matrix)

getAdjMatrix<-function(name){
  graph<-graph.data.frame(d=read.table(name),directed = T)
  cat("The number of Vertices are",vcount(graph))
  cat("\nThe number of edges are",ecount(graph))
  AdjacencyMatrix<-get.adjacency(graph,sparse = TRUE)
  return(AdjacencyMatrix)
}
getTransitionMatrix<-function(A){
  A<-t(A)
  SummaryMatrix<- within(summary(A), x <- 1/(colSums(A)[j]))
  T<-sparseMatrix(i=SummaryMatrix$i,j=SummaryMatrix$j,dims=c(A@Dim[1],A@Dim[2]),x=(A@x)*(SummaryMatrix$x))
  z<-colSums(T)
  z[z > 0]<-(0.2/ncol(A))
  z[z==0]<-(1/ncol(A))
  return(list(T=T,z=z))
}

PageRank<-function(T,z,niter){
  e=matrix(1,nrow=nrow(T),ncol=1)
  xold = matrix(1/nrow(T),nrow=nrow(T),ncol=1)
  for(i in 1:niter){
    xnew = (0.8*T) %*% xold + e %*% (z %*% xold)
    xold = xnew;
  }
  return(xnew)
}
# run PageRank on graph data
AdjacentMatrix<-getAdjMatrix("graph.txt")
TransistionMatrixList<-getTransitionMatrix(AdjacentMatrix)
Page_Rank<-PageRank(TransistionMatrixList$T,TransistionMatrixList$z,40) #iteration=40
id=read.table("graph.txt")[,1]
Page_Rank<-as.data.frame(as.matrix((cbind(id[!duplicated(id)],Page_Rank))))
Page_Rank<-Page_Rank[order(Page_Rank$V2,decreasing = T),]
colnames(Page_Rank)<-c("id","score")
top_5<-Page_Rank[1:5,]
bottom_5<-Page_Rank[96:100,]
print(top_5)
print(bottom_5)

