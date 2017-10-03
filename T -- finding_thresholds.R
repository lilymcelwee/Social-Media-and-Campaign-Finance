### Data
## Filtering
# Topic Assignment

## Set working directory
getwd()
setwd('/Users/Xyz27/Dropbox/Thesis/DATA')

## Load the topic_df file:
# Load the topic DF
topic_df<-read.csv("topic_df.csv")
print(length(topic_df$docid))
topic_df$X<-NULL

## Find the max, in order to plot the major topic (highest share)
library(dplyr)
# Find the max topic-share in a document
topic_df[, "max"] <- apply(topic_df[, 2:ncol(topic_df)], 1, max)
# Find the sum of shares for a document
topic_df[,'sum']<-rowSums(topic_df[3:grep("max", colnames(topic_df))-1])
# Find the topic name capturing the largest share:
topic_df[,'maxname']<-colnames(topic_df[, 3:grep("max", colnames(topic_df))-1])[apply(topic_df[, 3:grep("max", colnames(topic_df))-1],1,which.max)]
# Rename the assigned topic to 'none' if the sum (total share) is zero
topic_df[,'maxname']<-ifelse(topic_df$sum!=0, topic_df$maxname, "none")
# If there is more than one topic to which the document belongs, 1; if not, 0
topic_df[,'morethanone']<-ifelse(topic_df$max!=topic_df$sum,1,0)
# Check if any 0.05:
#topic_df[which(topic_df$ == "blank"), 2:40] <- NA
ncol(topic_df)
topic_df[,'split']<-NULL

## Find out how many are not classified:
length(topic_df[topic_df$maxname=="none",]$docid) # 0, before application of threshold


#### PLOTTING SHARES -- MAJOR TOPICS AND ALL TOPICS
# Load the ggplot library:
library(ggplot2)

## PLOT  -- MAJOR TOPICS
# Create dataframe of major topics
major<-as.data.frame(topic_df$max)
names(major)<-c("majortopic")

## Plotting major topics 
# Density function
major_point<-ggplot(data=major, aes(majortopic)) + geom_density()+#+ geom_histogram(bins=50,colour = "dark green") + 
  theme_wsj(base_size=8, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+ xlab("Major Topic Share") + 
  theme(axis.title = element_text()) +  labs(x = 'Major Topic Share', y = 'Count') + 
  scale_x_continuous(breaks = round(seq(min(major$majortopic), max(major$majortopic), by = 0.1),1))
major_point
# Histogram
major_point<-ggplot(data=major, aes(majortopic)) + geom_histogram(binwidth = 0.05)+#+ geom_histogram(bins=50,colour = "dark green") + 
  theme_wsj(base_size=8, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+ xlab("Major Topic Share") + ylab("Count")+ ggtitle("Distribution of Shares Captured by the Highest-Share Topic") + scale_x_continuous(breaks = round(seq(min(major$majortopic), max(major$majortopic), by = 0.1),1))
major_point+theme(text=element_text(size=10, family="Verdana"))
# Plotting without 1:
below1<-as.data.frame(major[major$majortopic<1,])
names(below1)<-"majortopic"
class(below1)
major_point<-ggplot(data=below1, aes(majortopic)) + geom_histogram(binwidth = 0.03)+#+ geom_histogram(bins=50,colour = "dark green") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+ xlab("Major Topic Share") + ylab("Count")+
  theme(axis.title = element_text()) +  labs(x = 'Major Topic Share', y = 'Count') + 
  scale_x_continuous(breaks = round(seq(min(major$majortopic), max(major$majortopic), by = 0.1),1))
major_point

## PLOT -- ALL TOPICS
## Plotting all topics -- rewrite this!
library(zoo)
# Remove the summary columns, so only the shares are plotted
all_topics<-topic_df[, !(colnames(topic_df) %in% c("max", "sum", "maxname", "morethanone"))]
all_topics$docid<-NULL
View(all_topics)
# Create one column of all values:
onecolumn<-stack(all_topics)

# Remove all the zero values, which indicate topics that do not describe a document:
onecolumn<-onecolumn[onecolumn$values!=0,]
# Histogram
topics<-ggplot(data=onecolumn, aes(values)) + geom_histogram(binwidth=0.01)+#+ geom_histogram(bins=50,colour = "dark green") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + xlab("All Topic Share") + 
  theme(axis.title = element_text()) +  labs(x = 'Topic Shares', y = 'Count')
topics
# Removing values <1:
below1<-as.data.frame(onecolumn[onecolumn$values<1,])
names(below1)<-"values"
class(below1)
topics<-ggplot(data=below1, aes(values)) + geom_histogram(binwidth=0.01)+#+ geom_histogram(bins=50,colour = "dark green") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + xlab("All Topic Share") + 
  theme(axis.title = element_text()) +  labs(x = 'Topic Shares', y = 'Count')
topics
