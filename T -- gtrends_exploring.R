### DATA
## COLLECTION
# Google Search Trends

# Set working directory
getwd()
setwd("/Users/Xyz27/Dropbox/Thesis/Code")

## Simply load the library, according to the new instructions for the development version of the package
library(gtrendsR)

## Get the list of search terms:
setwd("/Users/Xyz27/Dropbox/Thesis/DATA")
search<-read.csv("senate_searchinfo.csv")
# Create vectors of search terms:
name_vector<-as.vector(search$name)

## FULL DF:
# Create dataframe to include the contents of the gtrendsR function results
search_df<-data.frame(matrix(ncol=6,nrow=0))
c<-c("date", "hits", "keyword", "geo", "gprop", "category")
colnames(search_df)<-c

# For loop to get each search results at a time:
for (i in name_vector){
  print("break")
  print(i)
  res<-gtrends(i, geo = c("US"), time = "2016-09-25 2016-11-09")
  try(trend<-as.data.frame(res$interest_over_time))
  search_df<-rbind(search_df, trend)
}

## Average Monthly
# First, get one-year worth of data
year_df<-data.frame(matrix(ncol=6,nrow=0))
c<-c("date", "hits", "keyword", "geo", "gprop", "category")
colnames(year_df)<-c
# For loop to get each search result at a time: last month
for (i in name_vector){
  print("break")
  print(i)
  res<-gtrends(i, geo = c("US"), time = "today 12-m")
  try(trend<-as.data.frame(res$interest_over_time))
  year_df<-rbind(year_df, trend)
}

# Add this observation:
res_jh<-gtrends(c("John Heiderscheit"), geo = c("US"), time = "today 12-m")
trend_jh<-as.data.frame(res_jh$interest_over_time)
year_df<-rbind(year_df, trend_jh)

# Sum the views to get monthly views per candidate
yearly_sum<-ddply(year_df,~keyword,summarise,sum_year_gst=sum(hits))
names(yearly_sum)<-c("name", "sum_year_gst")
# Divide each value by 12 in order to create a monthly average
yearly_sum$ave_monthly_gst<-yearly_sum$sum_year_gst/12
## FOR SCALING: Have to access data from July 3rd in order to scale the datapoints:
adwords<-read.csv('adwords.csv')
# Merge with adwords data, which is monthly search volume
yearly<-merge(yearly_sum, adwords, by=c("name"))
# Create ratio
yearly$ratio<-yearly$adwords/yearly$ave_monthly_gst

## Now, use these ratios to scale the daily data for each candidate
ratio<-yearly[,c("name", "ratio")]
names(ratio)<-c("keyword", "ratio")
scaled_daily<-merge(year_df, ratio, by=c("keyword"))
# Create column of scaled searches:
scaled_daily$scaled_gst<-scaled_daily$ratio*(scaled_daily$hits)

# Create period columm:
# DATES:
# 09/26/2016-10/3/2016
#10/4/2016-10/10/2016
#10/11/2016-10/17/2016
#10/18/2016-10/24/2016
#10/25/2016-10/31/2016
#11/1/2016-11/8/2016
scaled_daily$period<-ifelse(scaled_daily$date<="2016-10-03 BST", 1, ifelse(
  scaled_daily$date>"2016-10-03 BST" & scaled_daily$date<="2016-10-10 BST", 2, ifelse(
    scaled_daily$date>"2016-10-10 BST" & scaled_daily$date<="2016-10-17 BST", 3, ifelse(
      scaled_daily$date>"2016-10-17 BST" & scaled_daily$date<="2016-10-24 BST", 4, ifelse(
        scaled_daily$date>"2016-10-24 BST" & scaled_daily$date<="2016-10-31 BST", 5, ifelse(
          scaled_daily$date>"2016-10-31 BST" & scaled_daily$date<="2016-11-08 BST", 6, 0))))))

# Remove those falling outside of dates:
scaled_daily<-scaled_daily[scaled_daily$period!=0,]

## Aggregate by candidate-period:
gst<-ddply(scaled_daily,~keyword+period,summarise,scaled_weekly_gst=sum(scaled_gst))
names(gst)<-c("keyword", "period", "scaled_gst")

## Export data:
write.csv(gst, 'gst_scaled.csv')

# Read file:
gst<-read.csv("gst_scaled.csv")
gst$X<-NULL

##### DESCRIPTIVE STATISTICS #####
summary(gst)
# Normal scale
hist<-ggplot(data=gst, aes(scaled_gst)) + geom_histogram(bins=50,colour = "yellow", fill="yellow") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = "Weekly Google Search Trends", y = 'Count')
hist
# Log scale
hist<-ggplot(data=gst, aes(log(scaled_gst))) + geom_histogram(bins=50,colour = "yellow", fill="yellow") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = "Weekly Google Search Trends (Log)", y = 'Count')
hist
## Per period - LOG
par(mfrow = c(2, 3))
p<-list()
for (i in 1:6){
  period<-gst[gst$period==i,]
  p[[i]]<-ggplot(data=period, aes(log(scaled_gst))) + geom_histogram(bins=50,colour = "yellow", fill="yellow") + xlim(0,16) + ylim(0,10) +
    theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
    theme(axis.title = element_text()) +  labs(x = paste("Period",i), y = 'Count')
  ggtitle("Google Search Volume (108 Candidates, Log)")
  do.call(grid.arrange,p)
}

do.call(grid.arrange,p)

# Per Period:
p<-list()
for (i in 1:6){
  period<-wikir[wikir$period==i,]
  p[[i]]<-ggplot(data=period, aes(log(wiki_views))) + geom_histogram(bins=50,colour = "blue", fill="blue") + xlim(0,12)+ylim(0,16)+
    theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
    theme(axis.title = element_text()) +  labs(x = paste("Period",i), y = 'Count')
  do.call(grid.arrange,p)
}


