### DATA
## COLLECTION
# Wikipedia Page Views

# Set working directory
getwd()
setwd("/Users/Xyz27/Dropbox/THESIS/Code")

## Load packages:
remove.packages(pageviews)
install.packages(pageviews)
library(ddply)
library(plyr)
library(pageviews)

## Load the data:
info<-read.csv('senate_searchinfo.csv')

# Search term column:
sub<-info[info$Wikipedia.!="" & info$Wikipedia.!="n",]
wiki<-unlist(list(as.character(sub$Wikipedia.)))

## WIKIPEDIA
# Load packages
# Test 1:
art1<-article_pageviews(project = "en.wikipedia",
                  article = "Thomas Dixon (politician)", platform = "all",
                  user_type = "all", start = "2016010100", end = "2016110800", reformat = TRUE)
art1
art2<-article_pageviews(project = "en.wikipedia",
                        article = "Tom Cruise", platform = "all",
                        user_type = "all", start = "2016010100", end = "2016110800", reformat = TRUE)
comb<-rbind(art1, art2)

## For loop to get Wikipedia page views:
wiki_df<-data.frame(matrix(ncol=8,nrow=0))
c<-c("project", "language", "article", "access", "agent", "granularity", "date", "views")
colnames(wiki_df)<-c

for (i in 1:length(wiki)){
    try(views<-article_pageviews(project = "en.wikipedia",
                             article = as.character(wiki[i]), platform = "all",
                             user_type = "all", start = "2016090100", end = "2016110800", reformat = TRUE))
    print(head(views))
    wiki_df<-rbind(wiki_df, views)
}

# There are 68 articles with Wikipedia pages
print(length(unique(wiki_df$article)))

## Categorize into periods:
wiki_df[format(wiki_df$date, '%Y-%m-%d') >= '2016-09-26' & format(wiki_df$date, '%Y-%m-%d') < '2016-10-03',][,"period"]<-1
library(car)
# 09/26/2016-10/3/2016
#10/4/2016-10/10/2016
#10/11/2016-10/17/2016
#10/18/2016-10/24/2016
#10/25/2016-10/31/2016
#11/1/2016-11/8/2016
wiki_df$period <- ifelse(format(wiki_df$date, '%Y-%m-%d') >= '2016-09-26' & format(wiki_df$date, '%Y-%m-%d') < '2016-10-03', 1,
                                 ifelse(format(wiki_df$date, '%Y-%m-%d') >= '2016-10-04' & format(wiki_df$date, '%Y-%m-%d') < '2016-10-10', 2,
                                        ifelse(format(wiki_df$date, '%Y-%m-%d') >= '2016-10-11' & format(wiki_df$date, '%Y-%m-%d') < '2016-10-17', 3,
                                               ifelse(format(wiki_df$date, '%Y-%m-%d') >= '2016-10-18' & format(wiki_df$date, '%Y-%m-%d') < '2016-10-24', 4,
                                                      ifelse(format(wiki_df$date, '%Y-%m-%d') >= '2016-10-25' & format(wiki_df$date, '%Y-%m-%d') < '2016-10-31', 5,
                                                             ifelse(format(wiki_df$date, '%Y-%m-%d') >= '2016-11-01' & format(wiki_df$date, '%Y-%m-%d') < '2016-11-08', 6, "NA")))))) # all other values map to NA

# Remove the dates that are not in the periods set above:
wiki_df<-wiki_df[wiki_df$period!="NA",]

## Export the full wikipedia dataset:
write.csv(wiki_df, 'wikipedia_full.csv')

## IMPORT FILE
#setwd("/Users/Xyz27/Dropbox/THESIS/CODE")
wiki_df<-read.csv('wikipedia_full.csv')
wiki_df$X<-NULL

## Now summarize, so that have 1 count of full views per candidate per period:
wikir<-ddply(wiki_df, .(article, period), numcolwise(sum), .drop=FALSE)

# Replace the '-' characters with spaces:
wikir$article <- gsub('_', ' ', wikir$article)

# Rename columns:
names(wikir)<-c("wikipedia_article", "period", "wiki_views")

# Export the Wikipedia data:
setwd("/Users/Xyz27/Dropbox/THESIS/DATA")
write.csv(wikir, 'wiki_period.csv')

# Plot of per-candidate distribution: WP Views
# Lin
hist<-ggplot(data=wikir, aes(wiki_views)) + geom_histogram(bins=50,colour = "blue", fill="blue") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = 'Wikipedia Page Views', y = 'Count')
hist
# Log
hist<-ggplot(data=wikir, aes(log(wiki_views))) + geom_histogram(bins=50,colour = "blue", fill="blue") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() +
  theme(axis.title = element_text()) +  labs(x = 'Wikipedia Page Views (Log)', y = 'Count')
hist
## Per period - LOG
g= list()
par(mfrow = c(2, 3))

# Per Period:
p<-list()
for (i in 1:6){
  period<-wikir[wikir$period==i,]
  p[[i]]<-ggplot(data=period, aes(log(wiki_views))) + geom_histogram(bins=50,colour = "blue", fill="blue") + xlim(0,12)+ylim(0,16)+
    theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
    theme(axis.title = element_text()) +  labs(x = paste("Period",i), y = 'Count')
  do.call(grid.arrange,p)
}



