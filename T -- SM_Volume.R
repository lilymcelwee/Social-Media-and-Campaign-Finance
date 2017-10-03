### Data
## Description
# Social Media Volume

## Set working directory:
getwd()
setwd('/Users/Xyz27/Dropbox/Thesis/DATA')

## Load the cand_full file:
cand_full<-read.csv("cand_full_class.csv")
cand_full$X<-NULL
## Summarize by volume:
sm_volume<-aggregate(text ~ name+period+platform, data = cand_full, FUN=length)# count)
names(sm_volume)<-c("name", "period", "platform", "post_volume")

## Save the social media volume table:
write.csv(sm_volume, 'smvolume.csv')

## Load the SM volume data:
setwd('/Users/Xyz27/Dropbox/Thesis/DATA')
smv<-read.csv("smvolume.csv")
smv$X<-NULL

## DISTRIBUTION OF VOLUME DATA - OVERALL
summary(smv$post_volume)
# Normal
hist<-ggplot(data=smv, aes(post_volume)) + geom_histogram(bins=80,colour = "red", fill="red") +  
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = 'Post Volume (Per Period)', y = 'Count')
hist
# Log
hist<-ggplot(data=smv, aes(log(post_volume))) + geom_histogram(bins=15,colour = "red", fill="red") +  
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = 'Post Volume (Per Period)', y = 'Count')
hist

## DISTRIBUTION OF VOLUME DATA - TWITTER
t_v<-smv[smv$platform=="twitter",]
summary(t_v$post_volume)
#View(t_v)
hist<-ggplot(data=t_v, aes(post_volume)) + geom_histogram(bins=80,colour = "light blue", fill="light blue") +  
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = 'Post Volume (Per Period)', y = 'Count')
hist
# Log
hist<-ggplot(data=t_v, aes(log(post_volume))) + geom_histogram(bins=15,colour = "light blue", fill="light blue") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = 'Post Volume (Per Period)', y = 'Count')
hist

## DISTRIBUTION OF VOLUME DATA - FACEBOOK
f_v<-smv[smv$platform=="facebook",]
summary(f_v$post_volume)
#View(f_v)
hist<-ggplot(data=f_v, aes(post_volume)) + geom_histogram(bins=30,colour = "dark blue", fill="dark blue") +  
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()  + 
  theme(axis.title = element_text()) +  labs(x = 'Post Volume (Per Period)', y = 'Count')
hist
# Log
hist<-ggplot(data=f_v, aes(log(post_volume))) + geom_histogram(bins=5,colour = "dark blue", fill="dark blue") +  
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() +
  theme(axis.title = element_text()) +  labs(x = 'Post Volume (Per Period)', y = 'Count')
hist
