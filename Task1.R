##R code for Task-1
##load data;
Turkiye=read.csv("file:///Users/pengxuechan/Desktop/evaluation.csv", header=TRUE);
## center the data according to the mean
dataT=Turkiye[,-c(1,2,3,4,5)]; 
dataT[1:3,]
evaluation = dataT;
evaluation = scale(evaluation, scale = TRUE,center = TRUE);
evaluation = t(evaluation);
labels = rownames (evaluation)
labels
evaluation = as.data.frame(evaluation);
pComponents = prcomp (evaluation, scale = FALSE, center = FALSE);
projections = predict (pComponents);
head(projections);
plot(pComponents, main="PCA evaluation ScreenPlot");

##the loading
plot(pComponents$rotation[,1],type = 'l', main="The loading for PCA")

component.var = pComponents$sdev ^2;
component.var 
pve = component.var/sum(component.var);
pve 

##Scatter Plot for Questions 
plot(projections[,1:2], type = "n", main="Scatter Plot for Questions");
text(x=projections[,1], y=projections[,2], labels=labels)

##Compute the distance 
Questions.dist = dist(evaluation)

Questions.mds = cmdscale(Questions.dist);
 
plot(Questions.mds, type = 'n', main = "MDS for Questions")
text(Questions.mds,labels = labels, col = rainbow(28))

## k-means
mds.dim1 = Questions.mds[,1];
mds.dim2 = Questions.mds[,2];
dataTrans = t(dataT); 

grpTurkiye3 = kmeans((evaluation), centers = 3, nstart = 10);

plot(mds.dim1, mds.dim2, type = 'n',xlab = "MDS-1", ylab = "MDS-2",
     main = "MDS for K-means Clustering k=3", col = grpTurkiye3$cluster)
text(Questions.mds, labels = labels, col = rainbow(5)[grpTurkiye3$cluster])

grpTurkiye6 = kmeans((evaluation), center = 6, nstart = 10);
plot(mds.dim1, mds.dim2, type = 'n', xlab = "MDS-1", ylab = "MDS-2",
     main = "MDS for K-means Clustering k=6", col = grpTurkiye6$cluster)
text(Questions.mds, labels = labels, col = rainbow(6)[grpTurkiye6$cluster])

## produce MDS for h-clustering
##single link
hcSingle = hclust(Questions.dist, method = 'single');
plot(hcSingle, main = "Dendrogram for single-link")
hcSingle3 = cutree(hcSingle, k=3)
hcSingle6 = cutree(hcSingle, k=6)
print(hcSingle3)
print(hcSingle6)

plot(mds.dim1, mds.dim2, type = 'n',xlab = "MDS-1", ylab = "MDS-2",
     main = "MDS for H-clustering single-link k=3", col = hcSingle3)
text(Questions.mds, labels = labels, col = rainbow(5)[hcSingle3])

plot(mds.dim1, mds.dim2, type = 'n',xlab = "MDS-1", ylab = "MDS-2",
     main = "MDS for H-clustering single-link k=6", col = hcSingle6)
text(Questions.mds, labels = labels, col = rainbow(6)[hcSingle6])

##complete link
hcComplete = hclust(Questions.dist, method = 'complete');
plot(hcComplete, main="Dendrogram for complete-link")
hcComplete3 = cutree(hcComplete, k=3)
hcComplete6 = cutree(hcComplete, k=6)
print(hcComplete3)
print(hcComplete6)

plot(mds.dim1, mds.dim2, type = 'n',xlab = "MDS-1", ylab = "MDS-2",
     main = "MDS for H-clustering complete-link k=3", col = hcComplete3)
text(Questions.mds, labels = labels, col = rainbow(5)[hcComplete3])

plot(mds.dim1, mds.dim2, type = 'n',xlab = "MDS-1", ylab = "MDS-2",
     main = "MDS for H-clustering complete-link k=6", col = hcComplete6)
text(Questions.mds, labels = labels, col = rainbow(6)[hcComplete6])

##average-link
hcAverage = hclust(Questions.dist, method = 'average');
plot(hcAverage, main="Dendrogram for average-link")
hcAverage3 = cutree(hcAverage, k=3)
hcAverage6 = cutree(hcAverage, k=6)
print(hcAverage3)
print(hcAverage6)

plot(mds.dim1, mds.dim2, type = 'n',xlab = "MDS-1", ylab = "MDS-2",
     main = "MDS for H-clustering average-link k=3", col = hcAverage3)
text(Questions.mds, labels = labels, col = rainbow(5)[hcAverage3])

plot(mds.dim1, mds.dim2, type = 'n',xlab = "MDS-1", ylab = "MDS-2",
     main = "MDS for H-clustering average-link k=6", col = hcAverage6)
text(Questions.mds, labels = labels, col = rainbow(6)[hcAverage6])

