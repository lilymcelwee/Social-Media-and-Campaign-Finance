### Data
## Filtering
# Topic Assignment

## Set working directory
getwd()
setwd('/Users/Xyz27/Dropbox/Thesis/DATA')

# Load the topic DF
topic_df<-read.csv("topic_df.csv")
print(length(topic_df$docid))
topic_df$X<-NULL

## USE THE THRESHOLD HERE AND CLASSIFY THE DOCUMENTS
# How many non-zero terms naturally exist in matrix?
library(Matrix)
topic_matrix<-as.matrix(topic_df)
nnzero(topic_matrix, na.counted = FALSE) # 108,586

# If value is above 0.4, keep, otherwise move to zero
topic_matrix[topic_matrix<0.25]<-0
View(topic_matrix)

# Now check how many non-zero values there are:
nnzero(topic_matrix, na.counted = FALSE) # 52,707

## Convert to df:
library(reshape2)
topic_df<-as.data.frame(topic_matrix)

## CLASSIFY DOCUMENTS
## After I have applied the thresholds, I run the max and sum functions again to classify documents. 
#library(magrittr)
library(dplyr)
# Find the max topic-share in a document
topic_df[, "max"] <- apply(topic_df[, 3:ncol(topic_df)], 1, max)
# Find the sum of shares for a document
topic_df[,'sum']<-rowSums(topic_df[3:grep("max", colnames(topic_df))-1])
# Find the topic name capturing the largest share:
topic_df[,'maxname']<-colnames(topic_df[, 3:grep("max", colnames(topic_df))-1])[apply(topic_df[, 3:grep("max", colnames(topic_df))-1],1,which.max)]
# Rename the assigned topic to 'none' if the sum (total share) is zero 
topic_df[,'maxname']<-ifelse(topic_df$sum!=0, topic_df$maxname, "none")
# If there is more than one topic to which the document belongs, 1; if not, 0
topic_df[,'morethanone']<-ifelse(topic_df$max!=topic_df$sum,1,0)

## Keep the necessary classification columns:
names(topic_df)
class_df<-topic_df[,c("docid","maxname", "morethanone")]
View(class_df)
length(class_df[class_df$maxname=="none",]$docid)
length(class_df$docid)
# Export this file:
write.csv(class_df,"classification")

# OG Row file
origrows<-read.csv('origrows(noempty).csv')
origrows$X<-NULL

# Merge with the classification file:
class<-cbind(class_df, origrows)
# Cand_df:
cand_df<-read.csv("cand_full.csv")
print(length(cand_df$abb))
cand_df$X<-NULL
cand_df$original_row<-row.names(cand_df)
## THE 'newdata' file is necessary to MERGE with once have the documents -- because that will allow me to understand what the indexes are of the documents that ARE classified, based on the original file's indices.
# Additionally, this step combines the FULL file with the ones that are classified in the other steps.
b<-merge(cand_df, class, by=c("original_row"), all=T)
b$text.y<-NULL
# Find out which are empty:
b$empty<-ifelse(is.na(b$empty), "empty", "full")
b$original_row<-as.numeric(b$original_row)
View(b)

## Classify the empty ones as 'none' - these are for the documents without any text
b$maxname<-ifelse(b$empty=="empty", "none", b$maxname)
print(length(b$original_row))

# Export to csv:
names(b)[4]<-"text"
names(b)
b$original_row<-NULL
write.csv(b, 'cand_full_class.csv', row.names=FALSE)
