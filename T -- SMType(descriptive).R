### Results
## Descriptive Statistics
# Social Media - Volume

library(Rmisc)

# Set the working directory:
getwd()
setwd('/Users/Xyz27/Dropbox/Thesis/DATA')

# Social media presence
handles<-read.csv('handle_table.csv')
print(length(unique(handles$name)))

## Missing social media data altogether:
names<-read.csv('google_adwordsdata.csv')
print(length(names$name))
diff<-setdiff(names$name,unique(handles$name))
diff

# Load the full file:
new_df<-read.csv('cand_full_cleancombine(withempty).csv')
print(length(unique(new_df$name)))
print(length(new_df$name))

# Check to make sure no individuals with social media data are misisng from DF
print(length(unique(new_df$name)))#,unique(handles$name)))
diff<-setdiff(unique(handles$name),unique(new_df$name))
diff # NONE

############# DESCRIPTIVE ANALYSIS
# Find the number of people with different levels of SM presence on each platform
# TWITTER:
handle_t<-handles[handles$handle_type=="twitter1",]
#View(handle_t)
print(unique(handle_t$name))
handle_t2<-handles[handles$handle_type=="twitter2",]
print(unique(handle_t2$name))
# FACEBOOK
handle_fb<-handles[handles$handle_type=="facebook1",]
#View(handle_fb)
print(unique(handle_fb$name))
handle_fb2<-handles[handles$handle_type=="facebook2",]
print(unique(handle_fb2$name))

## Which candidates did/did notpost at all in a given period?
# If a candidate does not have any observations for a given period, it means they did not post in the period.
new_df<-new_df[!duplicated(new_df), ]
# Find out which candidates did not post in a given period
table(new_df$name, new_df$period)
print(length(new_df$name))
new_df$X<-NULL
#View(new_df)
# Campaign-related messages:
campaign<-new_df[,c("name", "period", "campaign_related")]
ave_cr_summary<-summarySE(campaign, measurevar="campaign_related", groupvars=c("period"))
ave_cr_summary$post<-ave_cr_summary$campaign_related
ave_cr_summary$campaign_related<-NULL
ave_cr_summary$Type<-"campaign"

## Issue-Related:
issue<-new_df[,c("name", "period", "issue_related")]
ave_ir_summary<-summarySE(issue, measurevar="issue_related", groupvars=c("period"))
ave_ir_summary$post<-ave_ir_summary$issue_related
ave_ir_summary$issue_related<-NULL
ave_ir_summary$Type<-"issue"

## Now add full post-volume in before plotting:
posts<-new_df[,c("name", "period", "post_volume")]
ave_post_summary<-summarySE(posts, measurevar="post_volume", groupvars=c("period"))
ave_post_summary$post<-ave_post_summary$post_volume
ave_post_summary$post_volume<-NULL
ave_post_summary$Type<-"full"

## Bind the types:
full_summary<-rbind(ave_cr_summary, ave_ir_summary, ave_post_summary)
#View(full_summary)

## Error Bars
# Plot the two types and full volume:
ggplot(data=full_summary, aes(period, post)) + 
         geom_line(aes(color = Type, group = Type)) +
         geom_errorbar(aes(ymin = post - se, ymax = post + se, color = Type)) + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = 'Period', y = 'Posts (Ave)') + guides(size = guide_legend(override.aes = list(alpha=2)))#, size = guide_legend(size=10))#+ guides(shape=guide_legend(override.aes=list(size=10)))

ggplot(data=full_summary, aes(period, post)) + 
  geom_line(aes(color = Type, group = Type)) +
  geom_errorbar(aes(ymin = post - se, ymax = post + se, color = Type)) + 
  #theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+
  scale_colour_wsj() + scale_x_continuous(breaks=c(1,2,3,4,5,6,seq(1,6,by=1)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.background=element_rect(fill = "#f8f2e4", colour = "#f8f2e4"), axis.title = element_text(), 
        legend.background = element_rect("#f8f2e4"), legend.key = element_rect(fill = "#f8f2e4"), 
        panel.background = element_rect(fill = "#f8f2e4", colour = "#f8f2e4"), legend.text=element_text(size=12), legend.title = element_text(size=12),
        axis.text=element_text(size=12), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x = 'Period', y = 'Posts (Ave)', legend.key.size=14, base_size=14)#, size = guide_legend(size=10))#+ guides(shape=guide_legend(override.aes=list(size=10)))



## Plot Error Bars: Campaign-Related Summary
ggplot(data=ave_cr_summary, aes(x=period, y=post)) + geom_line(colour="purple") + 
  geom_errorbar(aes(ymin=post-se, ymax=post+se), width=.1, colour="purple") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = 'Period', y = 'Campaign-Related Messages (Ave)') 

#View(campaign)
###### SOCIAL MEDIA TYPE - Histograms
## Analyze the average number of campaign-related posts per candidate:
# Plot of per-candidate distribution: CASH
# Lin
hist<-ggplot(data=campaign, aes(post)) +  geom_histogram(bins=50,colour = "purple", fill="purple") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() +  
  theme(axis.title = element_text()) +  labs(x = 'Campaign-Related Posts (Average)', y = 'Count')
hist
# Log
hist<-ggplot(data=campaign, aes(log(post))) + geom_histogram(bins=25,colour = "purple", fill="purple") +  
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = 'Campaign-Related Posts (Average, Log)', y = 'Count')
hist

## Analyze the average number of issue-related posts per candidate:
# Lin
hist<-ggplot(data=issue, aes(post)) +  geom_histogram(bins=50,colour = "deeppink3", fill="deeppink3") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() +  
  theme(axis.title = element_text()) +  labs(x = 'Issue-Related Posts (Average)', y = 'Count')
hist
# Log
hist<-ggplot(data=issue, aes(log(post))) + geom_histogram(bins=25,colour = "deeppink3", fill="deeppink3") +  
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = 'Issue-Related Posts (Average, Log)', y = 'Count')
hist



# Plot average CS for each period
library(ggplot2)
library(Rmisc)
cs<-new_df[,c("name", "period", "campaign_share")]
ave_cs_summary<-summarySE(cs, measurevar="campaign_share", groupvars=c("period"))
#View(ave_cs_summary)
eb <- aes(ymax = mean + sd, ymin = mean - sd)
ggplot(data=ave_cs_summary, aes(x=period, y=campaign_share)) + geom_line(colour="purple") + 
  geom_errorbar(aes(ymin=campaign_share-se, ymax=campaign_share+se), width=.1, colour="purple") +  
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() +  
  theme(axis.title = element_text()) +  labs(x = 'Period', y = 'Campaign-Related Messages (% Total)')#, group = name, colour = name)) + geom_line() #+
  #geom_point(size=4, shape=21, fill="white")
View(ave_cs_summary)

## Create plot for each perid, with bar chart representing number of posts per topic
# Reshape dataframe, from wide to long:
library(reshape2)
period_topics<-new_df[,c("name", "period", "topic1", "topic2", "topic3", "topic4", "topic5", "topic6", "topic7", "topic8", "topic9", "topic10", "none")]
long <- melt(period_topics, id.vars = c("name", "period"))
names(long)<-c("name", "period", "topic", "value")

ave_topic_per_period<-aggregate(value ~ period+topic, data=long, mean)
#View(ave_topic_per_period)
names(ave_topic_per_period)
one<-ave_topic_per_period[ave_topic_per_period$period==1,]
## Plot posting averages per topic in each period
library(gridExtra)
range(ave_topic_per_period$value)
par(mfrow = c(2, 3))
p<-list()
View(ave_topic_per_period)
for (i in 1:6){
  period<-ave_topic_per_period[ave_topic_per_period$period==i,]
  period<-period[,c("topic", "value")]
  p[[i]]<-ggplot(data=period, aes(value)) + geom_histogram(bins=100,colour = "black", fill="black") + xlim(0,10)+ylim(0,max(period$value))+#scale_x_discrete(breaks=period$topic, labels=period$topic)+
    theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
    theme(axis.title = element_text()) +  labs(x = paste("Topics, in Period", i), y = 'Average number of posts')
  #plot(period$topic, period$value, ylab=c("Average number of posts"), xlab=c("Topics"), main="Posting Average Per Topic")
  #ggplot(period, aes(topic, value)) + geom_point() #ggplot((as.factor(period$topic), xlab=paste("Period", i), main="Average Posts Per Topic (108 candidates)")
  do.call(grid.arrange,p)
}

# CALCULATE THE NUMBER OF POSTS PER PERIOD, in ONE GRAPH
total_sum<-aggregate(value ~ period, data=long, FUN="sum")
total_sum$period<-c(1:6)
total_sum$topic<-"all"
sum_topic_per_period<-aggregate(value ~ topic + period, data=long, FUN="sum")
names(sum_topic_per_period)<-c("Topic", "Period", "Value")
# Bind together:
period_sums<-rbind(sum_topic_per_period, total_sum)
#dev.off()
# Existing chart -- the popularity of each topic overall
#ggplot(sum_topic_per_period, aes(x=Topic, y=Value)) +
 # geom_line() + 
  #geom_text(aes(label=Value),hjust=0, vjust=0)+ theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+
  #theme(axis.title = element_text()) +  labs(x = paste("Topics"), y = 'Posts (Ave)')
# New chart - popularity of each topic over the six periods, in one graph
library(directlabels)
ggplot(sum_topic_per_period, aes(x=Period, y=Value, group=Topic, colour=Topic)) +
  #geom_dl(aes(label=Topic),method="last.qp") + 
  geom_line(alpha=0.7,aes(color=as.factor(sum_topic_per_period$Topic))) +
  geom_text(data=subset(sum_topic_per_period, Period != "1" && Period != "6"), aes(label=Value),hjust=0, vjust=0, size=5)+ 
  theme_wsj(base_size=14, base_family="Verdana", title_family="Verdana")#scale_colour_wsj()+
  #theme(axis.title = element_text()) +  labs(x = paste("Topics"), y = 'Posts (Ave)')

View(sum_topic_per_period)
