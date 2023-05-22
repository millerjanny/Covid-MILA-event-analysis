
#Part of the script to generate the derived variables from the market connectivity networks 
#is based on one of the use cases of the FINTECH-h2020 project: https://fintech-ho2020.eu/


rm(list = ls())
setwd("C:IPSA")

# Pre-load the packages
# Clean the environment 
graphics.off()
#options(warn=-1)
rm(list = ls(all = TRUE))
## Default repo
local({r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org" 
options(repos=r)
})
# Necessary libraries 
# Install and load packages
libraries = c("PerformanceAnalytics",
              "xts",
              "quantmod",
              "vegan",
              "ape",
              "dendextend",
              "NetworkToolbox",
              "igraph",
              "MTS",
              "Matrix",
              "matrixcalc",
              "fPortfolio",
              #"IntroCompFinR",
              "quadprog",
              "pracma",
              "glasso"
)
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

prezzi<-read.table("ts_eventos.csv", header=TRUE, sep=";", dec=".")
ZOO <- zoo(prezzi[,-1], order.by=as.Date(as.character(prezzi$FECHA), format='%d/%m/%Y'))
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
dim(returnstd)

W<-list()
for(t in 0: 836){
  W[[(t+1)]]=returnstd[(t+10):(261+t),]
}

C <- list()
Dist <- list()
for(t in 1: length(W)){
  #Wt[[(t)]]=returnstd[(t):(tw+t),]
  C[[(t)]] =cor(W[[(t)]])
  Dist[[t]]<-sqrt(2-2*C[[t]])
  Dist[[t]]<-as.matrix(Dist[[t]])
  Dist[[t]][is.nan(Dist[[t]])]<-0
  colnames(Dist[[(t)]])<-colnames(returnstd)
  rownames(Dist[[(t)]])<-colnames(returnstd)
}

ciao<-list()
nodes2 <- read.csv("nodes3.csv",sep = ";", header=T, as.is=T)

for(t in 1: length(W)){
  ciao[[t]]<-as.numeric(unlist(Dist[[t]]))
  ciao[[t]]<-matrix(ciao[[t]],28,28)
  colnames(ciao[[t]])<-nodes2$id
  rownames(ciao[[t]])<-nodes2$id
}

A<-list()
network<-list()
Edgelist<-list()
weight<-list()
links2<-list()

for(t in 1: length(W)){
  network[[t]]=graph_from_adjacency_matrix(ciao[[t]],weighted=T, mode="undirected", diag=F)
  Edgelist[[t]]<-get.edgelist(network[[t]])
  weight[[t]]<-E(network[[t]])$weight
  A[[t]]<-cbind(Edgelist[[t]],weight[[t]])
  A[[t]]<-as.matrix(A[[t]])
  links2[[t]]<-as.data.frame(A[[t]])
  colnames(links2[[t]])<-c("from","to","weight")
}

par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(network[[1]], layout=layout.sphere, main="sphere")
plot(network[[1]], layout=layout.circle, main="circle")
plot(network[[1]], layout=layout.random, main="random")
plot(network[[1]], layout=layout.fruchterman.reingold, main="fruchterman.reingold")

weightmst<-list()
net<-list()
mst<-list()
deg<-list()
root<-list()
deg_vert<-list()
centralization<-list()
def_matrix<-list()
red<-list()
res<-list()
for(t in  1: length(W)){
  net[[t]]<- graph_from_data_frame(d=links2[[t]], vertices=nodes2, directed=F)
  mst[[t]] <- minimum.spanning.tree(net[[t]])
  weightmst[[t]]<-max(E(mst[[t]])$weight)
  wei<-unlist(weightmst[[t]])
  deg[[t]]<-degree(mst[[t]])
  centralization[[t]]<-centr_eigen(mst[[t]])$centralization
  centr<-as.matrix(unlist(centralization))
  deg_vert[[t]]<- -(deg[[t]])
  root[[t]]<-names(deg[[t]])[deg[[t]]== max(deg[[t]])]
  def_matrix[[t]]<-ciao[[t]]-(as_adjacency_matrix(mst[[t]])*ciao[[t]])
  def_matrix[[t]][def_matrix[[t]] == 0] <- 5
  def_m<-as.matrix(unlist(def_matrix[[t]]))
  de<-vec(def_m)
  red[[t]]<-sum(de < wei )/sum(de > wei )
  a<-subset(de,de<wei)
  b<-subset(de,de>wei)
  res[[t]]<-sum(b^-1)/sum(a^-1)
}


#Tres eventos 11/03/2020, 18/03/2020 y 24/12/2020

par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(mst[[1]], layout=layout.sphere, main="sphere",vertex.label=nodes2$id)
plot(mst[[600]], layout=layout.circle, main="circle",vertex.label=nodes2$id)
plot(mst[[600]], layout=layout.random, main="random",vertex.label=nodes2$id)
plot(mst[[600]], layout=layout.fruchterman.reingold, main="fruchterman.reingold",vertex.label=nodes2$id)

par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(mst[[460]], layout=layout.sphere, main="sphere",vertex.label=nodes2$id)
plot(mst[[460]], layout=layout.circle, main="circle",vertex.label=nodes2$id)
plot(mst[[460]], layout=layout.random, main="random",vertex.label=nodes2$id)
plot(mst[[460]], layout=layout.fruchterman.reingold, main="fruchterman.reingold",vertex.label=nodes2$id)


par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(mst[[465]], layout=layout.sphere, main="sphere",vertex.label=nodes2$id)
plot(mst[[465]], layout=layout.circle, main="circle",vertex.label=nodes2$id)
plot(mst[[465]], layout=layout.random, main="random",vertex.label=nodes2$id)
plot(mst[[465]], layout=layout.fruchterman.reingold, main="fruchterman.reingold",vertex.label=nodes2$id)

par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(mst[[666]], layout=layout.sphere, main="sphere",vertex.label=nodes2$id)
plot(mst[[666]], layout=layout.circle, main="circle",vertex.label=nodes2$id)
plot(mst[[666]], layout=layout.random, main="random",vertex.label=nodes2$id)
plot(mst[[666]], layout=layout.fruchterman.reingold, main="fruchterman.reingold",vertex.label=nodes2$id)

EIGEN_cent<-list()
eigencent<-list()

for(t in 1: length(W)){
  EIGEN_cent[[t]]<-eigen_centrality(mst[[t]], directed = FALSE, scale = TRUE,options = arpack_defaults)
  eigencent[[t]]<-EIGEN_cent[[t]]$vector
  round(eigencent[[t]],3)
  eigencent[[t]]<-as.matrix(eigencent[[t]])
  eigencent[[t]]<- -(eigencent[[t]])
  #eigen_mat[,t]<-eigencent[[t]][,1]
}

eigen_mat<-matrix(, nrow = 28, ncol = length(W))
rownames(eigen_mat)<-nodes2$id


for(t in 1: length(W)){
  eigen_mat[,t]<--eigencent[[t]][,1]
}

eigen_mat<-t(eigen_mat)


EIGEN_cent<-eigen_centrality(mst[[600]], directed = FALSE, scale = TRUE,options = arpack_defaults)$vector

plot.igraph(mst[[600]],vertex.size=EIGEN_cent*30,layout=layout.fruchterman.reingold,
            vertex.label=nodes2$id,
            vertex.frame.color="gray", vertex.label.color="black", edge.color="gray",
            vertex.label.cex=0.5,
            edge.width=as.numeric(E(mst[[600]])$weight)*2.5,
            vertex.label.dist=1,
            frame='False',
            edge.lty=c("solid"),
            margin=-0.65,
            vertex.color="orange",
            vertex.label.dist=2)

dev.off()
EIGEN_cent<-eigen_centrality(mst[[460]], directed = FALSE, scale = TRUE,options = arpack_defaults)$vector
plot.igraph(mst[[460]],vertex.size=EIGEN_cent*30,layout=layout.fruchterman.reingold,
            vertex.label=nodes2$id,
            vertex.frame.color="gray", vertex.label.color="black", edge.color="gray",
            vertex.label.cex=0.5,
            edge.width=as.numeric(E(mst[[460]])$weight)*2.5,
            vertex.label.dist=1,
            frame='False',
            edge.lty=c("solid"),
            margin=-0.6,
            vertex.color="orange",
            vertex.label.dist=1)

EIGEN_cent<-eigen_centrality(mst[[465]], directed = FALSE, scale = TRUE,options = arpack_defaults)$vector
plot.igraph(mst[[465]],vertex.size=EIGEN_cent*30,layout=layout.fruchterman.reingold,
            vertex.label=nodes2$id,
            vertex.frame.color="gray", vertex.label.color="black", edge.color="gray",
            vertex.label.cex=0.5,
            edge.width=as.numeric(E(mst[[465]])$weight)*2.5,
            vertex.label.dist=1,
            frame='False',
            edge.lty=c("solid"),
            margin=-0.6,
            vertex.color="orange",
            vertex.label.dist=1)

EIGEN_cent<-eigen_centrality(mst[[666]], directed = FALSE, scale = TRUE,options = arpack_defaults)$vector
plot.igraph(mst[[666]],vertex.size=EIGEN_cent*30,layout=layout.fruchterman.reingold,
            vertex.label=nodes2$id,
            vertex.frame.color="gray", vertex.label.color="black", edge.color="gray",
            vertex.label.cex=0.5,
            edge.width=as.numeric(E(mst[[666]])$weight)*2.5,
            vertex.label.dist=1,
            frame='False',
            edge.lty=c("solid"),
            margin=-0.6,
            vertex.color="orange",
            vertex.label.dist=1)


write.csv(eigen_mat,'C:\\pruebaeigen_ipsa.csv', row.names = TRUE)
write.csv(centr,'C:pruebacent_ipsa.csv', row.names = TRUE)