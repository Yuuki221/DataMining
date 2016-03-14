Reuters = read.csv("file:///Users/pengxuechan/Desktop/reuters21578.csv", header = TRUE);
library(plyr);
library(ggplot2);
library(tm);
library(lsa);
## select top 4 popular topics 
select.topics = sort(table(Reuters$topic), decreasing = T)[1:4];
select.topics = names(select.topics);
doc.idx = which(Reuters$topic %in% select.topics);
dataset = Reuters[doc.idx,];
## create corpus
corpus = Corpus(VectorSource(dataset$content));
## preprocessing the document 
## transform it all to lower cases letters 
corpus = tm_map(corpus,tolower);
inspect(corpus[1:3]);
corpus = tm_map(corpus, PlainTextDocument);
inspect(corpus[1:3]);
## Remove Punctuation in the document
corpus = tm_map(corpus, removePunctuation);
inspect(corpus[1:3]);
## Remove Numbers in the document
corpus = tm_map(corpus, removeNumbers);
inspect(corpus[1:3]);
## Remove stop words
corpus = tm_map(corpus, function(x) removeWords(x, stopwords("english")));
inspect (corpus[1:3]);
## the stemming process
corpus = tm_map(corpus, stemDocument, language = "english");
inspect(corpus[1:3]);
## Remove white spaces in the documents
corpus = tm_map(corpus, stripWhitespace);
## Create term-document Matrix
TDM0 = TermDocumentMatrix(corpus);
## Select term with 4 frequency or higher
select.freq4 = findFreqTerms(TDM0, 4);
TDM1 = TDM0[select.freq4,];
##Plot MDS plot 
dist.mat = dist(t(as.matrix(TDM1)));
doc.mds = cmdscale(dist.mat, k = 2);
data = data.frame(x = doc.mds[,1], y = doc.mds[,2], topic = dataset$topic, id = row.names(dataset));
ggplot(data, aes(x = x, y = y, color = topic)) + geom_point();

## using tf_idf and plot MDS 
td.mat.w = lw_tf(as.matrix(TDM0))*gw_idf(as.matrix(TDM0));
k = 4;
S = svd(as.matrix(td.mat.w), nu = k, nv = k);
u = S$u;
s = S$d;
v = S$v;
td.mat.svd = S$u %*% diag(S$d[1:k]) %*% t(S$v);
dist.mat = dist(t(td.mat.svd));
doc.mds = cmdscale(dist.mat, k=2);
data = data.frame(x = doc.mds[,1], y = doc.mds[,2], topic = dataset$topic, id = row.names(dataset));
ggplot(data, aes(x = x, y = y, color = topic)) + geom_point();
## use NMF and 
library(NMF);
set.seed(12345);
res = nmf(as.matrix(TDM1), 3, "lee");
V.hat = fitted(res);
dim(V.hat);
dist.mat = dist(t(V.hat));
doc.mds = cmdscale(dist.mat, k=2);
data = data.frame(x = doc.mds[,1], y = doc.mds[,2], topic = dataset$topic, id = row.names(dataset));
ggplot(data, aes(x = x, y = y, color = topic)) + geom_point();

k=3;
S = svd(as.matrix(td.mat.w), nu = k, nv = k);
u = S$u; s = S$d; v = S$v;
td.mat.svd = S$u %*% diag(S$d[1:k]) %*% t(S$v);
dist.mat = dist(t(td.mat.svd));
doc.mds = cmdscale(dist.mat, k = 2);
data = data.frame(x = doc.mds[,1], y = doc.mds[,2], topic = dataset$topic, id = row.names(dataset));
ggplot(data, aes(x = x, y = y, color = topic)) + geom_point();


k = 3;
S = svd(as.matrix(td.mat.w), nu = k, nv = k);
u = S$u; s = S$d; v = S$v;
td.mat.svd = S$u %*% diag(S$d[1:k]) %*% t(S$v);
dist.mat = dist(t(td.mat.svd));
doc.mds = cmdscale(dist.mat, k = 2);
data = data.frame(x = doc.mds[,1], y = doc.mds[,2], topic = df$topic, id = row.names(df));
ggplot(data, aes(x = x, y = y, color = topic)) + geom_point() + geom_text(aes(x = x, y = y - 0.2, label = id));
las.space = lsa(td.mat.w, dims = 3);
dist.mat = dist(t(as.textmatrix(lsa.space)));
dist.mat;
doc.mds = cmdscale(dist.mat, k=2);
data = data.frame(x = doc.mds[,1], y = doc.mds[,2], topic = df$topic, id = row.names(df));
ggplot(data, aes(x = x, y = y, color = topic)) + geom_point() + geom_text(aes(x = x, y = y - 0.2, label = id))


