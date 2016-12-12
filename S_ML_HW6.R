# STAT 5241 Statistical Machine Learning 
# Assignment 6
# Litong Wen  UNI:lw2627
# Problem 1. Implementing PageRank
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
AdjacentMatrix<-getAdjMatrix("graph.txt")
TransistionMatrixList<-getTransitionMatrix(AdjacentMatrix)
Page_Rank<-PageRank(TransistionMatrixList$T,TransistionMatrixList$z,40)
id=read.table("graph.txt")[,1]
Page_Rank<-as.data.frame(as.matrix((cbind(id[!duplicated(id)],Page_Rank))))
Page_Rank<-Page_Rank[order(Page_Rank$V2,decreasing = T),]
colnames(Page_Rank)<-c("id","score")
top_5<-Page_Rank[1:5,]
bottom_5<-Page_Rank[96:100,]
print(top_5)
print(bottom_5)
# Problem 2.
# 1. plot graph of p for theta=1 in [0,4]
curve(dexp, xlim=c(0,4),main="Graph of exponential distribution PDF with theta=1")
# 2. 
curve(dexp,xlim = c(0,4),main="Graph of exponential distribution PDF with theta=1")
lines(c(1,1),c(0,dexp(1)))
lines(c(2,2),c(0,dexp(2)))
lines(c(4,4),c(0,dexp(4)))
# 3.
curve(dexp(x,rate=2),xlim=c(0,4),lty=2,ylab = "density")
curve(dexp(x),add=T)
legend("topright",legend=c("rate=1","rate=2"),lty = c(1,2)) # decrease the likelihood of each sample in this toy data set
# Q2c
# Generate n = 256 exponentially distributed samples with parameter θ = 1
dat<-rexp(n=256, rate=1)
# setup hyperparameters of the prior
alpha_0<-2
beta_0<-0.2
n<-c(1:256)
theta_list<-seq(from=0,to=4,length=1000)
alpha_list<-rep(NA,256)
beta_list<-rep(NA,256)

for(i in 1:256){
  alpha_list[i]<-alpha_0+n[i]
  beta_list[i]<-beta_0+sum(dat[1:i])
}

gamma_1<-dgamma(theta_list,shape=alpha_list[4],rate=beta_list[4])
plot(theta_list,gamma_1,ylim = c(0,7),type="l", lty=1,lwd=1.5,col="red",xlab="theta",ylab=" ",main = "posterior distribution with n=4,8,16,256")
gamma_2<-dgamma(theta_list,shape=alpha_list[8],rate=beta_list[8])
lines(theta_list,gamma_2,lty=2,lwd=1.5,col="blue")
gamma_3<-dgamma(theta_list,shape=alpha_list[16],rate=beta_list[16])
lines(theta_list,gamma_3,lty=3,lwd=1.5,col="green")
gamma_4<-dgamma(theta_list,shape=alpha_list[256],rate=beta_list[256])
lines(theta_list,gamma_4,lty=4,lwd=1.5,col="black")
legend("topright",c("n=4","n=8","n=16","n=256"),lwd=1.5,col=c("red","blue","green","black"),lty=c(1:4))
