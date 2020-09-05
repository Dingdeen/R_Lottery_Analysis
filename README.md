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
# install.packages("igraph")
#install.packages("data.table")
#install.packages("Matrix")
#install.packages("lubridate")
# install.packages("dtplyr")
# install.packages("data.frame")
X = as.data.frame(read_excel("0809Lottery.xlsx"));   
X[,2]=substr(X[,2],6,10)
X[,2] = paste(X[,1],X[,2],sep="-")
X
# install.packages("readxl")
# Z <- read_excel("2019Lottery_2.xlsx")

dim(X);   
# dim(Z);

max(unique(X$Perid))


head(X,3) 
# head(Z,3)
##################數據轉換 X--->X1/X2--->XX #########################3
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
# range(as.Date(XX$Date,format="%m/%d"),na.rm=T)
XX
# export(XX,"Lottery_XX.csv")
#   Year  Date Perid Total     Seq Number month
# 1   2019 01/01     1  1591     One     18    01
# 2   2019 01/01     1  1591     Two     23    01
# 3   2019 01/01     1  1591   Three     24    01
# 4   2019 01/01     1  1591    Four     25    01
# 5   2019 01/01     1  1591    Five     39    01
# 6   2019 01/01     1  1591     Six     42    01
# 7   2019 01/01     1  1591 Special     48    01
# 8   2019 01/04     2  1592     One      6    01
# 9   2019 01/04     2  1592     Two      7    01
# 10  2019 01/04     2  1592   Three      8    01
# 11  2019 01/04     2  1592    Four     20    01
# 12  2019 01/04     2  1592    Five     47    01
# 13  2019 01/04     2  1592     Six     48    01
# 14  2019 01/04     2  1592 Special     30    01
####################################################
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
#######數字之間的關聯##################
D = cor(t(TT)) #---> 數字之間的關聯性
D

Upper = cor(t(TT))
min(D)
# [1] -0.7707611
which(D==min(D), arr.ind=T)
# row col
# 26  19   6
# 14   6  19
round(D>0.8) 
plot(D)
TT[c(6,19),]
#     01 02 03 04 05 06 07 08 09 10 11 12
# 14  3  2  2  3  1  0  2  1  1  1  0  1
# 26  0  0  1  1  0  3  1  2  2  2  4  2
D0 = cor(t(TT));   dim(D0);   D0[1:2,1:8] 
D = D0;   diag(D) = 0;   D[1:2,1:8]
# install.packages("corrplot")
# library(corrplot);
# dev.new() ; corrplot(corr=cor(t(TT)),type = c("upper"))
D
# export(D,"D.csv")
#          1           10          11          12          13           14          15          16          17          18           19            2          20          21
# 1   0.00000000 -0.254740987  0.04068069 -0.05480302  0.12893084  0.170957904 -0.17852874  0.58159843 -0.07240296 -0.44632184  0.464193330 -0.359961157 -0.05251855 -0.20286404
# 10 -0.25474099  0.000000000 -0.53141223 -0.30681099  0.26593021 -0.125121238  0.18411492 -0.38630975 -0.56748031  0.04602873 -0.157190984  0.073199175  0.32497117  0.10703854
# 11  0.04068069 -0.531412229  0.00000000  0.39970403 -0.51861886  0.206471915  0.36458617 -0.30196356  0.41072005 -0.07595545  0.542366353  0.086279596 -0.25819889 -0.17663230
# 12 -0.05480302 -0.306810993  0.39970403  0.00000000  0.32245741  0.010836965  0.49115249  0.21308068  0.31301072 -0.41660256 -0.068072904  0.069738967 -0.17200523  0.10197870
# 13  0.12893084  0.265930212 -0.51861886  0.32245741  0.00000000 -0.275812709 -0.05252257  0.23200592  0.04260143 -0.22978626 -0.224209825  0.104407859  0.20601048  0.40250612
# 14  0.17095790 -0.125121238  0.20647192  0.01083697 -0.27581271  0.000000000 -0.42010540 -0.06822482  0.06013244 -0.21623072 -0.009590148  0.203515036  0.46041162 -0.22856298
# 15 -0.17852874  0.184114924  0.36458617  0.49115249 -0.05252257 -0.420105403  0.00000000 -0.11043153 -0.19466571  0.02500000  0.124184084 -0.045436947 -0.35300904  0.04228131
# 16  0.58159843 -0.386309748 -0.30196356  0.21308068  0.23200592 -0.068224825 -0.11043153  0.00000000 -0.03582872 -0.27607882 -0.188565271 -0.514311315 -0.25988885 -0.08171083TT.directed = graph.adjacency(D>0.6 && D != 1)



color = c(brewer.pal(10, "Paired"))
# DrawTheGraph <- function(relation,D) {
#   DD = D * round((D>relation),0);   
#   DDD = DD[colSums(abs(DD))>0, colSums(abs(DD))>0];   
#   # DDD[1:2,1:8]
#   #    11 14 15 18 22         26 27 29
#   # 11  0  0  0  0  0  0.0000000  0  0
#   # 14  0  0  0  0  0 -0.7707611  0  0
#   GDDD = graph.adjacency(abs(DDD)>0)
#   EG = get.edgelist(GDDD)
#   Elabel = NULL;  
#   for (k in 1:dim(EG)[1]) { Elabel[k] = DDD[EG[k,1],EG[k,2]] }
#   # plot(GDDD, edge.label=round(Elabel,2), 
#   #      edge.curved=0.7, edge.arrow.size=0.1)
#   
#   
#   test.layout <- layout_(GDDD,with_dh(weight.edge.lengths = edge_density(GDDD)/100))
#   
#   dev.new() ; plot(GDDD,edge.arrow.size=0.5,edge.color=color[abs(round(Elabel,1)*10)],edge.label=round(Elabel,2) , edge.curved=1 ,  layout = test.layout  ) #所有號碼的有向圖
#   
#   
#   DD = D * round(D< relation*(-1),0);   
#   DDD = DD[colSums(abs(DD))>0, colSums(abs(DD))>0];   
#   # DDD[1:2,1:8]
#   #    11 14 15 18 22         26 27 29
#   # 11  0  0  0  0  0  0.0000000  0  0
#   # 14  0  0  0  0  0 -0.7707611  0  0
#   GDDD = graph.adjacency(abs(DDD)>0)
#   EG = get.edgelist(GDDD)
#   Elabel = NULL;  
#   for (k in 1:dim(EG)[1]) { Elabel[k] = DDD[EG[k,1],EG[k,2]] }
#   # plot(GDDD, edge.label=round(Elabel,2), 
#   #      edge.curved=0.7, edge.arrow.size=0.1)
#   
#   
#   test.layout <- layout_(GDDD,with_dh(weight.edge.lengths = edge_density(GDDD)/100))
#   
#   dev.new() ; plot(GDDD,edge.arrow.size=0.5,edge.color=color[abs(round(Elabel,1)*10)],edge.label=round(Elabel,2) , edge.curved=1 ,  layout = test.layout  ) #所有號碼的有向圖
# }
# 
# DrawTheGraph(0.7,D) 


#####=====================Graph 的處理============================================###
DD = D * round((D>0.6),0);
DDD = DD[colSums(abs(DD))>0, colSums(abs(DD))>0]; 

g5 = graph.adjacency(abs(DDD)>0,mode = "upper")
##== (0) 節點度(degree): 與 v 相連的 邊的數量========
A = get.adjacency(g5, sparse=FALSE); 
g = network::as.network.matrix(A); 
sort(degree(abs(DDD)>0),decreasing=T)
sna::gplot.target(g, degree(g), main="Degree", circ.lab=FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=c("blue",rep("red",32),"yellow"), edge.col="darkgray")
##=================================================
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
# [1] 24  2  3  2

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

# hist(de2, breaks=1:vcount(g1)-1, main="Histogram of node degree")


 
 # ======================數據預測========================
# 7,8,10,22,41,6,40
 
 
