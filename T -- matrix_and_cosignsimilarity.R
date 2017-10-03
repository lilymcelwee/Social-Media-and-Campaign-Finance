### DATA
## FILTERING
# Cosine Similarity

## First load the file from the export folder for the TM tool:
# Set working directory:
getwd()
setwd('/Users/Xyz27/Dropbox/Thesis/DATA/TMOutput/output_csv')

# Libraries:
library(reshape2)
library(gtools)
library(plyr)
library(lsa)

# Load the topics in documents file:
tid<-read.csv("TopicsInDocs-11.csv", header=T)
tid$filename<-NULL
View(tid)
# Set working directory back to the normal data folder
getwd()
setwd('/Users/Xyz27/Dropbox/Thesis/DATA')

## Convert to a clean matrix
dt<-as.list(rep("",21))
recast<-as.list(rep("",21))
count=2
for (i in 1:10){
  count2=count+1
  print(count)
  dt[[i]]<-tid[,c(1,count:count2)]
  count=count+2
  names(dt[[i]])<-c("docid", "topic", "share")
  recast[[i]]<-reshape(dt[[i]], idvar="docid", timevar="topic", direction="wide")
  recast[[i]]$share.NA<-NULL
  print(names(recast[[i]]))
  recast[[i]]<-recast[[i]][c("docid", "share.1","share.2","share.3","share.4","share.5","share.6","share.7","share.8","share.9","share.10")]
}
warnings()
## Bind the results together:
bind<-smartbind(recast[[1]],recast[[2]], recast[[3]],recast[[4]], recast[[5]],recast[[6]], 
                recast[[7]],recast[[8]], recast[[9]],recast[[10]])

## Sum by document
groupColumns=c("docid")
dataColumns=c("share.1","share.2","share.3","share.4","share.5","share.6","share.7","share.8","share.9","share.10")
res=ddply(bind,groupColumns,function(x) colSums(x[dataColumns], na.rm = TRUE))
# Rename the columns to topics:
names(res)<-c("docid", "topic1", "topic2", "topic3","topic4","topic5","topic6","topic7","topic8","topic9","topic10")

## Reshaped topic df:
# Remove the final row of the matrix, because it involves the document 'NA':
res<-res
write.csv(res, 'topic_df.csv') 
View(res)
# Topic DF is a dataframe of the share breakdown with topics as headers and documents as row names

## cosine similarity
# Test:
top12<-cosine(res$topic8, res$topic10)
top12
# Convert 'res' to matrix form:
res_matrix<-as.matrix(res)
# Calculate cosine similarity matrix:
cs_matrix<-cosine(res_matrix,y=NULL)
# Remove the document column:
cs_matrix<-cs_matrix[-c(1), ]
cs_matrix<-cs_matrix[,-c(1)]

# Export matrix of cos-similarty as csv:
write.csv(cs_matrix, 'cs_matrix.csv')

##### CREATE GEPHI MATRIX
# Melt the matrix:
melt<-melt(cs_matrix)
melt$Type<-"Undirected"
names(melt)<-c("Source", "Target", "Weight", "Type")
write.csv(melt, 'cs_melt.csv', row.names=FALSE)
write.table(melt, file = "cs_melt.txt",row.names=FALSE, quote=FALSE, na="",col.names=FALSE, sep=",")

# CSMelt is the GEPHI-ready version of the topic cosine similarity matrix.
# Histogram of Cosine-Similarity Values (<1)
m1<-melt[melt$Weight<1,]
summary(m1$Weight)
m <- ggplot(m1, aes(x = Weight))
m + geom_histogram(bins=15) + ylab("Count") + xlab("Cosine Similarity")+ 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+
  theme(axis.title = element_text()) +  labs(x = 'Cosine Similarity', y = 'Count')

# Identify threshold
# Subset by threshold
# Count number of unique pairs above/below thresholds
threshold = 0.230
rel_cs<-melt[melt$Weight>threshold & melt$Weight!=1,]
# Remove the duplicate rows:
rel_cs<-rel_cs[!duplicated(rel_cs[,3]),]
print(length(unique(melt[melt$Weight!=1,]$Weight))) # 45 pairs total
print(length(unique(rel_cs$Weight))) # 30 unique pairs at/above the threshold

##### Export the data with the new values, after applying the CS threshold
# Export the edges
write.csv(rel_cs, 'cs_edges.csv')

# Export the nodes
cs_labels<-data.frame()
Id<-as.vector(unique(rel_cs$Source))
Label<-as.vector(unique(rel_cs$Source))
cs_labels<-data.frame(Id, Label)

write.table(cs_labels, "cs_labels.csv", row.names=FALSE)
write.table(cs_labels, file = "cs_labels.txt",row.names=FALSE, quote=FALSE, na="",col.names=FALSE, sep=",")
