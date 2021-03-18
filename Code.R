setwd("D:/R_Code/") 
library(igraph); 
library(data.table);
library(Matrix); 
library(lubridate);
library(dplyr);
library(RColorBrewer); 
library(sand);
library(igraphdata);
library(sna); 
library(rio)
library(readxl)


X = as.data.frame(read_excel("0809Lottery.xlsx"));   
X[,2]=substr(X[,2],6,10)
X[,2] = paste(X[,1],X[,2],sep="-")
X

dim(X);   

max(unique(X$Perid))

head(X,3) 
##################數據轉換 X--->X1/X2--->XX #########################

dim(X)[1]
X1 = X[,c(1:3,11)] ; X2 = X[,4:10]
XX = NULL
for(i in 1:dim(X)[1]){
  for(j in 1:dim(X2)[2]){
    XX = rbind(XX,unlist(c(X1[i,1],as.character(X1[i,2]),X1[i,3:4],colnames(X2)[j],X2[i,j])))
  }
}
XX[,1] = as.integer(XX[,1])
XX[,3] = as.integer(XX[,3])
XX[,4] = as.integer(XX[,4])
XX[,6] = as.integer(XX[,6])
XX = as.data.frame(XX)
XX


colnames(XX) = c("Year","Date","Perid","Total","Seq","Number")
XX$Number[which(XX$Seq=='One')]
range(X$Year)
XX
export(XX,"Lottery_XX.csv")



table(XX$Number)
XX[which(XX$Number == 1),]
XX$month = substr(XX$Date,6,7)
TT = table(XX$Number,XX$month)
TT[1,]
TT
addmargins(TT)

rowSums(TT)
sort(rowSums(TT))
colSums(TT)
TT[which(rownames(TT) %in% c(1,2)),]
#######數字之間的關聯##############
D = cor(t(TT))
D

Upper = cor(t(TT))
min(D)
which(D==min(D), arr.ind=T)

round(D>0.8) 
plot(D)
TT[c(6,19),]

D0 = cor(t(TT));   dim(D0);   D0[1:2,1:8] 
D = D0;   diag(D) = 0;   D[1:2,1:8]

D

color = c(brewer.pal(10, "Paired"))

#####=====================Graph 的處理============================================###
DD = D * round((D>0.6),0);
DDD = DD[colSums(abs(DD))>0, colSums(abs(DD))>0]; 

g5 = graph.adjacency(abs(DDD)>0,mode = "upper")
##== (0) 節點度(degree): 與 v 相連的 邊的數量========
A = get.adjacency(g5, sparse=FALSE); 
g = network::as.network.matrix(A); 
sort(degree(abs(DDD)>0),decreasing=T)
sna::gplot.target(g, degree(g), main="Degree", circ.lab=FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=c("blue",rep("red",32),"yellow"), edge.col="darkgray")

EG = get.edgelist(g5)
EG
Elabel = NULL;
for (k in 1:dim(EG)[1]) { 
  column = which(  colnames(DDD) == EG[k,1] )
  row = which(  rownames(DDD) == EG[k,2] )
  Elabel[k] = DDD[column,row] 
  
}


DDD[31,]
dim(DDD)
DDD[35,]
plot(g5)
E(g5)$weight = Elabel
g5D = decompose.graph(g5)
unlist( lapply(g5D, FUN=function(x) length(V(x))) )

plot(g5D[[1]], vertex.size = 10) 

g1 = g5D[[1]]
V(g1)
E(g1)[1]
E(g1)$weight
vcount(g1)
ecount(g1)

##== (0) 節點度(degree): 與 v 相連的 邊的數量========
A = get.adjacency(g1, sparse=FALSE);
g = network::as.network.matrix(A);
de = degree(g)/2

##== (1) 接近中心度(closeness centrality): 節點到其他所有節點距離之和 的倒數
cl = round(closeness(g),3)*1000
ev = evcent(g)
sna::gplot.target(g, de, main="Degree", circ.lab =FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=c("blue",rep("red",32),"yellow"), edge.col="darkgray")
#== (2) 節點強度(strength) : 與 v 相連的 邊的權重之和
sg = graph.strength(g1)

##=================================================
color = c(brewer.pal(10, "Paired"))
plot(g1, vertex.size = 10)
V(g1)$size = round(strength(g1)*10,1)
l <- cbind(cl,de)
plot(g1, layout = l, edge.curved=0.2 ,xlab = "接近中心度",ylab = "節點度",edge.color=color[abs(round(ev,4)*10)+1],edge.width = 2,edge.label=abs(round(ev,3)*10))
 axis(side = 1, lwd = 1) 
 axis(side = 2, lwd = 1) 

 
# ======================數據預測========================
# 7,8,10,22,41,6,40
 
