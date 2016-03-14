##R code for Task2
library(ggplot2)
library(plyr)
BookInfo = read.csv("file:///Users/pengxuechan/Desktop/book-info.csv", header=TRUE);
BookPurchase = read.csv ("file:///Users/pengxuechan/Desktop/book-copurchase.csv", header=TRUE);

BookPurch = BookPurchase[,-1];
BookPurchMatrix = as.matrix(BookPurch)
BookPurchMatrix[1:3,1:5]
BookPurch.mult = BookPurchMatrix %*% BookPurchMatrix
BookPurch.mult[1:3, 1:5]

BooktoBook.dist = dist(BookPurch.mult);

classes = BookInfo$Class;
labels = BookInfo$name;

BooktoBook.mds = cmdscale(BooktoBook.dist);
mds.dim1 = BooktoBook.mds[,1];
mds.dim2 = BooktoBook.mds[,2];

bookClasses = mapvalues(BookInfo$Class,from=c(3,1),to=c("Conservative","Liberal"));

datak2 = data.frame(x=BooktoBook.mds[,1],y=BooktoBook.mds[,2],name=labels,bookClasses=bookClasses,classes=factor(classes));

p = ggplot(aes(x=x,y=y,shape=bookClasses,color=classes), data=datak2) +
  geom_point(size=6,alpha=0.5) +
  geom_text(aes(x=x,y=y,shape=bookClasses,color=classes,label=labels), size=4)+
  ggtitle("MDS for Book to Book");
print(p);

BooktoBookKmeas = kmeans((BookPurch), centers = 2, nstart = 10);

BooktoBookKmeas

clu=BooktoBookKmeas;
clu=BooktoBookKmeas$cluster;

## ggplot 

datak2 = data.frame(x=BooktoBook.mds[,1],y=BooktoBook.mds[,2],name=labels,bookClasses=bookClasses,clu=factor(clu));

pK = ggplot(aes(x=x,y=y,shape=bookClasses,color=clu), data=datak2) +
  geom_point(size=6,alpha=0.5) +
  geom_text(aes(x=x,y=y,shape=bookClasses,color=clu,label=labels), size=4)+
  ggtitle("MDS for k-means k=2");
print(pK);
##single link
hc.dist = dist(BookPurch);
hcSingle= hclust(hc.dist, method = 'single');
plot(hcSingle, main = "Dendrogram for single-link")
hcSingle2 = cutree(hcSingle, k=2)
print(hcSingle2)

datak2 = data.frame(x=BooktoBook.mds[,1],y=BooktoBook.mds[,2],name=labels,bookClasses=bookClasses,hcSingle2=factor(hcSingle2));

phs = ggplot(aes(x=x,y=y,shape=bookClasses,color=hcSingle2), data=datak2) +
  geom_point(size=6,alpha=0.5) +
  geom_text(aes(x=x,y=y,shape=bookClasses,color=hcSingle2,label=labels), size=4)+
  ggtitle("MDS for h-clustering single-linked");
print(phs);

##Complete link
hcComplete= hclust(hc.dist, method = 'complete');
plot(hcComplete, main="Dendrogram for complete-link")
hcComplete2 = cutree(hcComplete, k=2)
print(hcComplete2)

datak2 = data.frame(x=BooktoBook.mds[,1],y=BooktoBook.mds[,2],name=labels,bookClasses=bookClasses,hcComplete2=factor(hcComplete2));
phC = ggplot(aes(x=x,y=y,shape=bookClasses,color=hcComplete2), data=datak2) +
  geom_point(size=6,alpha=0.5) +
  geom_text(aes(x=x,y=y,shape=bookClasses,color=hcComplete2,label=labels), size=4)+
  ggtitle("MDS for h-clustering complete-linked");
print(phC);

##Average link
hcAverage = hclust(hc.dist, method = 'average');
plot(hcComplete, main="Dendrogram for average-link")
hcAverage2 = cutree(hcAverage, k=2)
print(hcAverage2)

datak2 = data.frame(x=BooktoBook.mds[,1],y=BooktoBook.mds[,2],name=labels,bookClasses=bookClasses,hcAverage2=factor(hcAverage2));
phA = ggplot(aes(x=x,y=y,shape=bookClasses,color=hcAverage2), data=datak2) +
  geom_point(size=6,alpha=0.5) +
  geom_text(aes(x=x,y=y,shape=bookClasses,color=hcAverage2,label=labels), size=4)+
  ggtitle("MDS for h-clustering average-linked");
print(phA);

## for purity and entropy
cluster.purity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

cluster.entropy <- function(clusters,classes) {
  en <- function(x) {
    s = sum(x)
    sum(sapply(x/s, function(p) {if (p) -p*log2(p) else 0} ) )
  }
  M = table(classes, clusters)
  m = apply(M, 2, en)
  c = colSums(M) / sum(M)
  sum(m*c)
}

print(cluster.purity(clu,bookClasses))
print(cluster.entropy(clu,bookClasses))
print(cluster.purity(hcSingle2,bookClasses))
print(cluster.entropy(hcSingle2,bookClasses))
print(cluster.purity(hcAverage2,bookClasses))
print(cluster.entropy(hcAverage2,bookClasses))
print(cluster.purity(hcComplete2,bookClasses))
print(cluster.entropy(hcComplete2,bookClasses))
