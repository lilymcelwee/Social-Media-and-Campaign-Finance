### Results
## Regression Analysis
# Log-Log Models
### Conclusion
## Reflections on the Data
# FEC Unitemized & Itemized Correlation


# Set the working directory:
getwd()
setwd('/Users/Xyz27/Dropbox/Thesis/DATA')
library(lm.beta)

## Load relevant file:
new_df_lag<-read.csv('full_lag.csv')
new_df_lag$X<-NULL
#View(new_df_lag)
names_df<-as.data.frame(names(new_df_lag))
#View(names_df)
# Log transformation
new_df_lag$log_sum_lag<-log(new_df_lag$donations_sum_lag+1)
new_df_lag$log_donations_sum_lag<-NULL
new_df_lag$log_count_lag<-log(new_df_lag$donations_count_lag+1)
new_df_lag$log_post_volume<-log(new_df_lag$post_volume+1)
new_df_lag$log_campaign<-log(new_df_lag$campaign_related+1)
new_df_lag$log_issue<-log(new_df_lag$issue_related+1)
# Controls
new_df_lag$log_gst<-log(new_df_lag$scaled_gst+1)
new_df_lag$log_wp<-log(new_df_lag$wiki_views+1)
new_df_lag$log_population<-log(new_df_lag$population+1)


## Full lag - update:
write.csv(new_df_lag, 'full_lag_df.csv')

class(new_df_lag$log_sum_lag)
mean(!is.na(new_df_lag$log_sum_lag))
mean(!is.na(new_df_lag$log_count_lag))
mean(!is.na(new_df_lag$log_post_volume))
mean(!is.na(new_df_lag$log_campaign))
mean(!is.na(new_df_lag$log_issue))
mean(!is.na(new_df_lag$log_issue))


summary(new_df_lag$log_sum_lag)
summary(new_df_lag$log_count_lag)
summary(new_df_lag$log_post_volume)
summary(new_df_lag$log_campaign)
summary(new_df_lag$log_issue)
#View(new_df_lag)

# Plot to check linearity: Plotting the linear and log/log scatterplots of each IV versus DV show the benefits of log-transformation, 
# as the lin-lin plots do not show a linear relationship. 
dev.off()
par(mfrow=c(2,2))
library("gridExtra")
library(cowplot)
##### VOLUME
# Sum, Vol
#plot(new_df_lag$donations_sum_lag, new_df_lag$post_volume)
#plot(new_df_lag$log_sum_lag, new_df_lag$log_post_volume)
hist1<-ggplot(data=new_df_lag, aes(x=post_volume, y=donations_sum_lag)) + geom_point() + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(y = 'Donations ($5000s)', x = 'Post Volume (Per Period)')
hist2<-ggplot(data=new_df_lag, aes(x=log_post_volume, y=log_sum_lag)) + geom_point() + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(y = 'Donations ($5000s, Log)', x = 'Post Volume (Per Period, Log)')
# Count, Vol
#plot(new_df_lag$donations_count_lag, new_df_lag$post_volume)
#plot(new_df_lag$log_count_lag, new_df_lag$log_post_volume)
hist3<-ggplot(data=new_df_lag, aes(x=post_volume, y=donations_count_lag)) + geom_point() + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(y = 'Donations (#)', x = 'Post Volume (Per Period)')
hist4<-ggplot(data=new_df_lag, aes(x=log_post_volume, y=log_count_lag)) + geom_point() + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(y = 'Donations (#, Log)', x = 'Post Volume (Per Period, Log)')
plot_grid(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2)
##### TYPE (C)
# Sum, Type (C)
#plot(new_df_lag$donations_sum_lag, new_df_lag$campaign_related)
#plot(new_df_lag$log_sum_lag, new_df_lag$log_campaign)
hist1<-ggplot(data=new_df_lag, aes(x=campaign_related, y=donations_sum_lag)) + geom_point() + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(y = 'Donations ($5000s)', x = 'Campaign Posts (Per Period)')
hist2<-ggplot(data=new_df_lag, aes(x=log_campaign, y=log_sum_lag)) + geom_point() + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(y = 'Donations ($5000s, Log)', x = 'Campaign Posts (Per Period, Log)')
# Count, Type (C)
#plot(new_df_lag$donations_count_lag, new_df_lag$campaign_related)
#plot(new_df_lag$log_count_lag, new_df_lag$log_campaign)
hist3<-ggplot(data=new_df_lag, aes(x=campaign_related, y=donations_count_lag)) + geom_point() + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(y = 'Donations (#)', x = 'Campaign Posts (Per Period)')
hist4<-ggplot(data=new_df_lag, aes(x=log_campaign, y=log_count_lag)) + geom_point() + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(y = 'Donations (#, Log)', x = 'Campaign Posts (Per Period, Log)')
plot_grid(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2)
##### TYPE (I)
# Sum, Type (I)
#plot(new_df_lag$donations_sum_lag, new_df_lag$issue_related)
#plot(new_df_lag$log_sum_lag, new_df_lag$log_issue)
hist1<-ggplot(data=new_df_lag, aes(x=issue_related, y=donations_sum_lag)) + geom_point() + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(y = 'Donations ($5000s)', x = 'Issue Posts (Per Period)')
hist2<-ggplot(data=new_df_lag, aes(x=log_issue, y=log_sum_lag)) + geom_point() + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(y = 'Donations ($5000s, Log)', x = 'Issue Posts (Per Period, Log)')
# Count, Type (I)
#plot(new_df_lag$donations_count_lag, new_df_lag$issue_related)
#plot(new_df_lag$log_count_lag, new_df_lag$log_issue)
hist3<-ggplot(data=new_df_lag, aes(x=issue_related, y=donations_count_lag)) + geom_point() + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(y = 'Donations (#)', x = 'Issue Posts (Per Period)')
hist4<-ggplot(data=new_df_lag, aes(x=log_issue, y=log_count_lag)) + geom_point() + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(y = 'Donations (#, Log)', x = 'Issue Posts (Per Period, Log)')
plot_grid(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2)

dev.off()

# Create categorical variable about high/medium/low share of campaign-related posts:
#new_df_lag$campaign_level<-ifelse(new_df_lag$campaign_share<=0.33, "low", ifelse(new_df_lag$campaign_share>0.33&new_df_lag$campaign_share<=0.67,"medium", "high"))

# Number of observations:
print(length(is.na(new_df_lag$donations_count_lag))) # There are 647 observations here. The missing observation is Wendy Long in Period 6 - this is because the shift function
# removed this last obseravtion. It's not an issue, since we only examine the first five periods of social media data (and the corresponding lagged periods of donation data).



################# Volume #################
########## Total ($) ########
## Model 1
# IV = Social Media Volume
# DV = Donation Totals
# Lag: None
# WFE = No
# Controls = None
#lmv_1<-lm(scaled_total ~ post_volume, data=new_df_lag)
#summary(lmv_1)

## Model 2
# IV = Social Media Volume
# DV = Donation Totals
# Lag: None
# WFE = No
# Controls = Incumbency, Population
#lmv_2<-lm(scaled_total ~ post_volume+incumbency+population, data=new_df_lag)
#summary(lmv_2)

## Model 3
# IV = Social Media Volume
# DV = Donation Totals
# Lag: None
# WFE = No
# Controls = GST, WP, Incumbency, Population
#View(new_df_lag)
#lmv_3<-lm(scaled_total ~ post_volume + incumbency + population + scaled_gst + wiki_views, data=new_df_lag)
#summary(lmv_3) 

## Model 1
# IV = Social Media Volume
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = None
#names(new_df_lag)
lmv_1<-lm(log_sum_lag ~ log_post_volume, data=new_df_lag)
summary(lmv_1)
#standardize(lmv_1)
#lmv_1.beta<-lm.beta(lmv_1)
#summary(lmv_1.beta)
# Which were used?
new_df_lag$used <- !seq_len(nrow(new_df_lag))%in%na.action(lmv_1)
new_df_lag$used<-NULL

## Model 2
# IV = Social Media Volume
# DV = Donation Totals
# Lag: 1-week
# WFE: No
#View(new_df_lag)
# Controls = Incumbency, Population
lmv_2<-lm(log_sum_lag ~ log_post_volume+incumbency+log_population, data=new_df_lag)
summary(lmv_2)
#standardize(lmv_2)
#lmv_2.beta<-lm.beta(lmv_2)
#summary(lmv_2.beta)
#coef(lmv_2.beta, standardized=TRUE)
## Model 3
# IV = Social Media Volume
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = GST, WP, Incumbency, Population
lmv_3<-lm(log_sum_lag ~ log_post_volume+incumbency+log_population+log_gst+log_wp, data=new_df_lag)
summary(lmv_3)
#standardize(lmv_3)
#lmv_3.beta<-lm.beta(lmv_3)
#summary(lmv_3.beta)

## DIFF BASELINE
lmv_11<-lm(log_sum_lag ~ incumbency+population, data=new_df_lag)
summary(lmv_11)
lmv_12<-lm(log_sum_lag ~ incumbency+population+scaled_gst+wiki_views, data=new_df_lag)
summary(lmv_12)
lmv_13<-lm(log_sum_lag ~ incumbency+population+scaled_gst+wiki_views+log_post_volume+log_post_volume*incumbency+log_post_volume*population, data=new_df_lag)
summary(lmv_13)

########## COUNT ########
## Model 4
# IV = Social Media Volume
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = None
names(new_df_lag)
lmv_4<-lm(log_count_lag ~ log_post_volume, data=new_df_lag)
#lmv_4.beta<-lm.beta(lmv_4)
#summary(lmv_4.beta)
#summary(lmv_4)

## Model 5
# IV = Social Media Volume
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = Incumbency, Population
lmv_5<-lm(log_count_lag ~ log_post_volume+incumbency+log_population, data=new_df_lag)
summary(lmv_5)
#lmv_5.beta<-lm.beta(lmv_5)
#summary(lmv_5.beta)
## Model 6
# IV = Social Media Volume
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = GST, WP, Incumbency, Population
lmv_6<-lm(log_count_lag ~ log_post_volume+incumbency+log_population+log_gst+log_wp, data=new_df_lag)
summary(lmv_6)
#lmv_6.beta<-lm.beta(lmv_6)
#summary(lmv_6.beta)

## DIFF BASELINE
lmv_21<-lm(log_count_lag ~ incumbency+population, data=new_df_lag)
summary(lmv_21)
lmv_22<-lm(log_count_lag ~ incumbency+population+scaled_gst+wiki_views, data=new_df_lag)
summary(lmv_22)
lmv_23<-lm(log_count_lag ~ incumbency+population+scaled_gst+wiki_views+log_post_volume, data=new_df_lag)
summary(lmv_23)

######################### TYPE ###################
############### TOTAL ($) #############
range(new_df_lag$campaign_share)
# Multiple the campaign_share variable by 100 in order to make it more easily interpretable.
#Multiply by 100 and then a 1 unit change means a 1 percentage point change.
#new_df_lag$campaign_percentage<-new_df_lag$campaign_share*100
#new_df_lag$campaign_percentage<-NULL

## Model 7
# IV = Campaign-Related Posts
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = None
names(new_df_lag)
lmv_7<-lm(log_sum_lag ~ log_campaign + log_post_volume, data=new_df_lag)
summary(lmv_7)
#lmv_7.beta<-lm.beta(lmv_7)
#summary(lmv_7.beta)

## Model 8
# IV = Campaign-Related Posts
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = Incumbency, Population
lmv_8<-lm(log_sum_lag ~ log_campaign+log_post_volume+incumbency+log_population, data=new_df_lag)
summary(lmv_8)
#lmv_8.beta<-lm.beta(lmv_8)
#summary(lmv_8.beta)
## Model 9
# IV = Campaign-Related Posts
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = GST, WP, Incumbency, Population
lmv_9<-lm(log_sum_lag ~ log_campaign+log_post_volume+incumbency+log_population+log_gst+log_wp, data=new_df_lag)
summary(lmv_9)
#lmv_9.beta<-lm.beta(lmv_9)
#summary(lmv_9.beta)

## DIFF BASELINE
lmv_31<-lm(log_sum_lag ~ incumbency+population+post_volume, data=new_df_lag)
summary(lmv_31)
lmv_32<-lm(log_sum_lag ~ incumbency+population+scaled_gst+wiki_views+post_volume, data=new_df_lag)
summary(lmv_32)
lmv_33<-lm(log_sum_lag ~ incumbency+population+scaled_gst+wiki_views+log_post_volume+log_campaign, data=new_df_lag)
summary(lmv_33)


############### COUNT (#) #############
## Model 10
# IV = Campaign-Related Posts
# DV = Donation Count
# Lag: 1-week
# WFE: No
# Controls = Total Posts
names(new_df_lag)
lmv_10<-lm(log_count_lag ~ log_campaign + log_post_volume, data=new_df_lag)
summary(lmv_10)
#lmv_10.beta<-lm.beta(lmv_10)
#summary(lmv_10.beta)

## Model 11
# IV = Campaign-Related Posts
# DV = Donation Count
# Lag: 1-week
# WFE: No
# Controls = None
#names(new_df_lag)
lmv_11<-lm(log_count_lag ~ log_campaign + log_post_volume + incumbency + log_population, data=new_df_lag)
summary(lmv_11)
#lmv_11.beta<-lm.beta(lmv_11)
#summary(lmv_11.beta)

## Model 12
# IV = Campaign-Related Posts
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = GST, WP, Incumbency, Population
lmv_12<-lm(log_count_lag ~ log_campaign+log_post_volume+incumbency+log_population+log_gst+log_wp, data=new_df_lag)
summary(lmv_12)
#lmv_12.beta<-lm.beta(lmv_12)
#summary(lmv_12.beta)

## DIFF BASELINE
lmv_41<-lm(log_count_lag ~ incumbency+population+post_volume, data=new_df_lag)
summary(lmv_41)
lmv_42<-lm(log_count_lag ~ incumbency+population+scaled_gst+wiki_views+post_volume, data=new_df_lag)
summary(lmv_42)
lmv_43<-lm(log_count_lag ~ incumbency+population+scaled_gst+wiki_views+log_post_volume+log_campaign, data=new_df_lag)
summary(lmv_43)

########## TYPE - ISSUE
## Model 7
# IV = Campaign-Related Posts
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = None
names(new_df_lag)
lmv_13<-lm(log_sum_lag ~ log_issue + log_post_volume, data=new_df_lag)
summary(lmv_13)
#lmv_13.beta<-lm.beta(lmv_13)
#summary(lmv_13.beta)

## Model 8
# IV = Campaign-Related Posts
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = Incumbency, Population
lmv_14<-lm(log_sum_lag ~ log_issue+log_post_volume+incumbency+log_population, data=new_df_lag)
summary(lmv_14)
#lmv_14.beta<-lm.beta(lmv_14)
#summary(lmv_14.beta)
## Model 9
# IV = Campaign-Related Posts
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = GST, WP, Incumbency, Population
lmv_15<-lm(log_sum_lag ~ log_issue+log_post_volume+incumbency+log_population+log_gst+log_wp, data=new_df_lag)
summary(lmv_15)
#lmv_15.beta<-lm.beta(lmv_15)
#summary(lmv_15.beta)


############### COUNT (#) #############
## Model 10
# IV = Campaign-Related Posts
# DV = Donation Count
# Lag: 1-week
# WFE: No
# Controls = Total Posts
names(new_df_lag)
lmv_16<-lm(log_count_lag ~ log_issue + log_post_volume, data=new_df_lag)
summary(lmv_16)

## Model 11
# IV = Campaign-Related Posts
# DV = Donation Count
# Lag: 1-week
# WFE: No
# Controls = None
names(new_df_lag)
lmv_17<-lm(log_count_lag ~ log_issue + log_post_volume + incumbency + log_population, data=new_df_lag)
summary(lmv_17)
#lmv_17.beta<-lm.beta(lmv_16)
#summary(lmv_17.beta)

## Model 12
# IV = Campaign-Related Posts
# DV = Donation Totals
# Lag: 1-week
# WFE: No
# Controls = GST, WP, Incumbency, Population
lmv_18<-lm(log_count_lag ~ log_issue+log_post_volume+incumbency+log_population+log_gst+log_wp, data=new_df_lag)
summary(lmv_18)
#lmv_18.beta<-lm.beta(lmv_18)
#summary(lmv_18.beta)

## Table of Models
## Print regression output table:
library(stargazer)
stargazer(lmv_1, lmv_2, lmv_3, type = "text", column.labels = c("Baseline", "Partial Controls", "Full"),covariate.labels=c("Post Volume", "Incumbency", "Population", "Google Search Trends", "Wikipedia Page Views", "Constant"), dep.var.labels="Donations (Log, $5000s)", digits=3, out="sm_models(sum_volume).txt")
stargazer(lmv_4, lmv_5, lmv_6, type = "text", column.labels = c("Baseline", "Partial Controls", "Full"),covariate.labels=c("Post Volume", "Incumbency", "Population", "Google Search Trends", "Wikipedia Page Views", "Constant"), dep.var.labels="Donations (Count)", digits=3, out="sm_models(count_volume).txt")
stargazer(lmv_7, lmv_8, lmv_9, type = "text", column.labels = c("Baseline", "Partial Controls", "Full"),covariate.labels=c("Campaign-Related Volume", "Post Volume", "Incumbency", "Population", "Google Search Trends", "Wikipedia Page Views", "Constant"), dep.var.labels="Donations (Log, $5000s)", digits=3, out="sm_models(sum_cr).txt")
stargazer(lmv_10, lmv_11, lmv_12, type = "text", column.labels = c("Baseline", "Partial Controls", "Full"), covariate.labels=c("Campaign-Related Volume", "Post Volume", "Incumbency", "Population", "Google Search Trends", "Wikipedia Page Views", "Constant"), dep.var.labels="Donations (Count)", digits=3, out="sm_models(count_cr).txt")
stargazer(lmv_13, lmv_14, lmv_15, type = "text", column.labels = c("Baseline", "Partial Controls", "Full"),covariate.labels=c("Issue-Related Volume", "Post Volume", "Incumbency", "Population", "Google Search Trends", "Wikipedia Page Views", "Constant"), dep.var.labels="Donations (Log, $5000s)", digits=3, out="sm_models(sum_ir).txt")
stargazer(lmv_16, lmv_17, lmv_18, type = "text", column.labels = c("Baseline", "Partial Controls", "Full"), covariate.labels=c("Issue-Related Volume", "Post Volume", "Incumbency", "Population", "Google Search Trends", "Wikipedia Page Views", "Constant"), dep.var.labels="Donations (Count)", digits=3, out="sm_models(count_ir).txt")
# BETA:
stargazer(lmv_1.beta, lmv_2.beta, lmv_3.beta, type = "text", column.labels = c("Baseline", "Partial Controls", "Full"),covariate.labels=c("Post Volume", "Incumbency", "Population", "Google Search Trends", "Wikipedia Page Views", "Constant"), dep.var.labels="Donations (Log, $5000s)", title="Social Media Volume & Political Donations (Sum)", digits=3, out="sm_models(sum_volume)BETA.txt")
stargazer(lmv_4.beta, lmv_5.beta, lmv_6.beta, type = "text", column.labels = c("Baseline", "Partial Controls", "Full"),covariate.labels=c("Post Volume", , "Incumbency", "Population", "Google Search Trends", "Wikipedia Page Views", "Constant"), dep.var.labels="Donations (Count)",title="Social Media Volume & Political Donations (Count)", digits=3, out="sm_models(count_volume)BETA.txt")
stargazer(lmv_7.beta, lmv_8.beta, lmv_9.beta, type = "text", column.labels = c("Baseline", "Partial Controls", "Full"),covariate.labels=c("Campaign-Related Volume",, "Post Volume", "Incumbency", "Population", "Google Search Trends", "Wikipedia Page Views", "Constant"), dep.var.labels="Donations (Log, $5000s)", title="Social Media Type & Political Donations (Sum)", digits=3, out="sm_models(sum_cr)BETA.txt")
stargazer(lmv_10.beta, lmv_11.beta, lmv_12.beta, type = "text", column.labels = c("Baseline", "Partial Controls", "Full"), covariate.labels=c("Campaign-Related Volume",, "Post Volume", "Incumbency", "Population", "Google Search Trends", "Wikipedia Page Views", "Constant"), dep.var.labels="Donations (Count)", title="Social Media Type & Political Donations (Count)", digits=3, out="sm_models(count_cr)BETA.txt")
stargazer(lmv_13.beta, lmv_14.beta, lmv_15.beta, type = "text", column.labels = c("Baseline", "Partial Controls", "Full"),covariate.labels=c("Issue-Related Volume",, "Post Volume", "Incumbency", "Population", "Google Search Trends", "Wikipedia Page Views", "Constant"), dep.var.labels="Donations (Log, $5000s)", title="Social Media Type & Political Donations (Sum)", digits=3, out="sm_models(sum_ir)BETA.txt")
stargazer(lmv_16.beta, lmv_17.beta, lmv_18.beta, coef = list(lmv_16.beta$standardized.coefficients, lmv_17.beta$standardized.coefficients, lmv_18.beta$standardized.coefficients), type = "text", column.labels = c("Baseline", "Partial Controls", "Full"), covariate.labels=c("Issue-Related Volume", "Post Volume", "Incumbency", "Population", "Google Search Trends", "Wikipedia Page Views", "Constant"), dep.var.labels="Donations (Count)", title="Social Media Type & Political Donations (Count)", digits=3, out="sm_models(count_ir)BETA.txt")

###### DIAGNOSTIC PLOTS:
## RESIDUALS
dev.off()
par(mfrow=c(2,3))
# Volume - NORMAL
r1<-ggplot(lmv_1, aes(.fitted, .resid))+geom_point()+ggtitle("Model 1")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r2<-ggplot(lmv_2, aes(.fitted, .resid))+geom_point()+ggtitle("Model 2")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r3<-ggplot(lmv_3, aes(.fitted, .resid))+geom_point()+ggtitle("Model 3")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r4<-ggplot(lmv_4, aes(.fitted, .resid))+geom_point()+ggtitle("Model 4")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r5<-ggplot(lmv_5, aes(.fitted, .resid))+geom_point()+ggtitle("Model 5")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r6<-ggplot(lmv_6, aes(.fitted, .resid))+geom_point()+ggtitle("Model 6")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
multiplot(r1,r2,r3,r4,r5,r6, cols=2)
# Volume - BETA
r1<-ggplot(lmv_1.beta, aes(.fitted, .resid))+geom_point()+ggtitle("Model 1")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r2<-ggplot(lmv_2.beta, aes(.fitted, .resid))+geom_point()+ggtitle("Model 2")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r3<-ggplot(lmv_3.beta, aes(.fitted, .resid))+geom_point()+ggtitle("Model 3")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r4<-ggplot(lmv_4.beta, aes(.fitted, .resid))+geom_point()+ggtitle("Model 4")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r5<-ggplot(lmv_5.beta, aes(.fitted, .resid))+geom_point()+ggtitle("Model 5")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r6<-ggplot(lmv_6.beta, aes(.fitted, .resid))+geom_point()+ggtitle("Model 6")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
multiplot(r1,r2,r3,r4,r5,r6, cols=2)

# Type - NORMAL
r7<-ggplot(lmv_7, aes(.fitted, .resid))+geom_point()+ggtitle("Model 7")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r8<-ggplot(lmv_8, aes(.fitted, .resid))+geom_point()+ggtitle("Model 8")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r9<-ggplot(lmv_9, aes(.fitted, .resid))+geom_point()+ggtitle("Model 9")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r10<-ggplot(lmv_10, aes(.fitted, .resid))+geom_point()+ggtitle("Model 10")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r11<-ggplot(lmv_11, aes(.fitted, .resid))+geom_point()+ggtitle("Model 11")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r12<-ggplot(lmv_12, aes(.fitted, .resid))+geom_point()+ggtitle("Model 12")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
multiplot(r7,r8,r9,r10,r11,r12, cols=2)
# Type - BETA
r7<-ggplot(lmv_7.beta, aes(.fitted, .resid))+geom_point()+ggtitle("Model 7")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r8<-ggplot(lmv_8.beta, aes(.fitted, .resid))+geom_point()+ggtitle("Model 8")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r9<-ggplot(lmv_9.beta, aes(.fitted, .resid))+geom_point()+ggtitle("Model 9")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r10<-ggplot(lmv_10.beta, aes(.fitted, .resid))+geom_point()+ggtitle("Model 10")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r11<-ggplot(lmv_11.beta, aes(.fitted, .resid))+geom_point()+ggtitle("Model 11")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r12<-ggplot(lmv_12.beta, aes(.fitted, .resid))+geom_point()+ggtitle("Model 12")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
multiplot(r7,r8,r9,r10,r11,r12, cols=2)

# Type - NORMAL
r13<-ggplot(lmv_13, aes(.fitted, .resid))+geom_point()+ggtitle("Model 13")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r14<-ggplot(lmv_14, aes(.fitted, .resid))+geom_point()+ggtitle("Model 14")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r15<-ggplot(lmv_15, aes(.fitted, .resid))+geom_point()+ggtitle("Model 15")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r16<-ggplot(lmv_16, aes(.fitted, .resid))+geom_point()+ggtitle("Model 16")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r17<-ggplot(lmv_17, aes(.fitted, .resid))+geom_point()+ggtitle("Model 17")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
r18<-ggplot(lmv_18, aes(.fitted, .resid))+geom_point()+ggtitle("Model 18")+theme_wsj(base_size=4, base_family="Verdana", title_family="Verdana")+scale_colour_wsj()+xlab("Fitted")+ylab("Residuals")
multiplot(r13, r14, r15, r16, r17, r18, cols=2)

#View(new_df_lag)

## Q-Q PLOT
dev.off()
par(mfrow=c(3,2), bg="#f8f2e4")
# Volume
qqnorm(lmv_1$res, main="Model 1")
qqline(lmv_1$res)
qqnorm(lmv_2$res, main="Model 2")
qqline(lmv_2$res)
qqnorm(lmv_3$res, main="Model 3")
qqline(lmv_3$res)
qqnorm(lmv_4$res, main="Model 4")
qqline(lmv_4$res)
qqnorm(lmv_5$res, main="Model 5")
qqline(lmv_5$res)
qqnorm(lmv_6$res, main="Model 6")
qqline(lmv_6$res)
# Volume - BETA:
dev.off()
qqnorm(lmv_1.beta$res, main="Model 1")
qqline(lmv_1.beta$res)
qqnorm(lmv_2.beta$res, main="Model 2")
qqline(lmv_2.beta$res)
qqnorm(lmv_3.beta$res, main="Model 3")
qqline(lmv_3.beta$res)
qqnorm(lmv_4.beta$res, main="Model 4")
qqline(lmv_4.beta$res)
qqnorm(lmv_5.beta$res, main="Model 5")
qqline(lmv_5.beta$res)
qqnorm(lmv_6.beta$res, main="Model 6")
qqline(lmv_6.beta$res)
# Type
par(mfrow=c(3,2), bg="#f8f2e4")
qqnorm(lmv_7$res, main="Model 7")
qqline(lmv_7$res)
qqnorm(lmv_8$res, main="Model 8")
qqline(lmv_8$res)
qqnorm(lmv_9$res, main="Model 9")
qqline(lmv_9$res)
qqnorm(lmv_10$res, main="Model 10")
qqline(lmv_10$res)
qqnorm(lmv_11$res, main="Model 11")
qqline(lmv_11$res)
qqnorm(lmv_12$res, main="Model 12")
qqline(lmv_12$res)
# Type- BETA:
dev.off()
qqnorm(lmv_7.beta$res, main="Model 7")
qqline(lmv_7.beta$res)
qqnorm(lmv_8.beta$res, main="Model 8")
qqline(lmv_8.beta$res)
qqnorm(lmv_9.beta$res, main="Model 9")
qqline(lmv_9.beta$res)
qqnorm(lmv_10.beta$res, main="Model 10")
qqline(lmv_10.beta$res)
qqnorm(lmv_11.beta$res, main="Model 11")
qqline(lmv_11.beta$res)
qqnorm(lmv_12.beta$res, main="Model 12")
qqline(lmv_12.beta$res)

# Models 13-18
par(mfrow=c(3,2), bg="#f8f2e4")
qqnorm(lmv_13$res, main="Model 13")
qqline(lmv_13$res)
qqnorm(lmv_14$res, main="Model 14")
qqline(lmv_14$res)
qqnorm(lmv_14$res, main="Model 15")
qqline(lmv_15$res)
qqnorm(lmv_15$res, main="Model 16")
qqline(lmv_16$res)
qqnorm(lmv_17$res, main="Model 17")
qqline(lmv_17$res)
qqnorm(lmv_18$res, main="Model 18")
qqline(lmv_18$res)



##### OBSERVED VS FITTED PLOT #####
### VOLUME
nona<-new_df_lag[!is.na(new_df_lag$log_sum_lag),]
## Sum
# For sum models: 1,2,3
#dev.off()
par(mfrow=c(3,1))
# 1
pred <- predict(lmv_1)
plot(nona$log_sum_lag, pred, xlim=range(c(nona$log_sum_lag,pred)), ylim=range(c(nona$log_sum_lag,pred)), xlab="observed", ylab="predicted", main="Model 1")
abline(0,1, lwd=2, col=8)
# 2
pred <- predict(lmv_2)
plot(nona$log_sum_lag, pred, xlim=range(c(nona$log_sum_lag,pred)), ylim=range(c(nona$log_sum_lag,pred)), xlab="observed", ylab="predicted", main="Model 2")
#lines(predict(lo), col='red', lwd=2)
abline(0,1, lwd=2, col=8)
# 3
pred <- predict(lmv_3)
plot(nona$log_sum_lag, pred, xlim=range(c(nona$log_sum_lag,pred)), ylim=range(c(nona$log_sum_lag,pred)), xlab="observed", ylab="predicted", main="Model 3")
abline(0,1, lwd=2, col=8)
## Count
nona<-new_df_lag[!is.na(new_df_lag$log_count_lag),]
# For count models: 4, 5, 6
# 4
pred <- predict(lmv_4)
plot(nona$log_sum_lag, pred, xlim=range(c(nona$log_sum_lag,pred)), ylim=range(c(nona$log_sum_lag,pred)), xlab="observed", ylab="predicted", main="Model 4")
abline(0,1, lwd=2, col=8)
# 5
pred <- predict(lmv_5)
plot(nona$log_sum_lag, pred, xlim=range(c(nona$log_sum_lag,pred)), ylim=range(c(nona$log_sum_lag,pred)), xlab="observed", ylab="predicted", main="Model 5")
abline(0,1, lwd=2, col=8)
# 6
pred <- predict(lmv_6)
plot(nona$log_sum_lag, pred, xlim=range(c(nona$log_sum_lag,pred)), ylim=range(c(nona$log_sum_lag,pred)), xlab="observed", ylab="predicted", main="Model 6")
abline(0,1, lwd=2, col=8)
### TYPE
## Sum
nona<-new_df_lag[!is.na(new_df_lag$log_sum_lag),]
# For sum models: 7,8,9
pred <- predict(lmv_7)
plot(nona$log_sum_lag, pred, xlim=range(c(nona$log_sum_lag,pred)), ylim=range(c(nona$log_sum_lag,pred)), xlab="observed", ylab="predicted", main="Model 7")
abline(0,1, lwd=2, col=8)
# 5
pred <- predict(lmv_8)
plot(nona$log_sum_lag, pred, xlim=range(c(nona$log_sum_lag,pred)), ylim=range(c(nona$log_sum_lag,pred)), xlab="observed", ylab="predicted", main="Model 8")
abline(0,1, lwd=2, col=8)
# 6
pred <- predict(lmv_9)
plot(nona$log_sum_lag, pred, xlim=range(c(nona$log_sum_lag,pred)), ylim=range(c(nona$log_sum_lag,pred)), xlab="observed", ylab="predicted", main="Model 9")
abline(0,1, lwd=2, col=8)
## Count
nona<-new_df_lag[!is.na(new_df_lag$log_sum_lag),]
# For count models: 10, 11, 12
pred <- predict(lmv_10)
plot(nona$log_sum_lag, pred, xlim=range(c(nona$log_sum_lag,pred)), ylim=range(c(nona$log_sum_lag,pred)), xlab="observed", ylab="predicted", main="Model 10")
abline(0,1, lwd=2, col=8)
# 5
pred <- predict(lmv_11)
plot(nona$log_sum_lag, pred, xlim=range(c(nona$log_sum_lag,pred)), ylim=range(c(nona$log_sum_lag,pred)), xlab="observed", ylab="predicted", main="Model 11")
abline(0,1, lwd=2, col=8)
# 6
pred <- predict(lmv_12)
plot(nona$log_sum_lag, pred, xlim=range(c(nona$log_sum_lag,pred)), ylim=range(c(nona$log_sum_lag,pred)), xlab="observed", ylab="predicted", main="Model 12")
abline(0,1, lwd=2, col=8)

## Correlation between unitemized and itemized sums:

# FEC: 
fec<-new_df_lag[,c("name", "state", "FECID")]
df_dups <- fec[c("name", "state", "FECID")]
fec<-fec[!duplicated(df_dups),]
fec<-fec[!is.na(fec$FECID),]
write.csv(fec, 'fec.csv')

fec_u<-read.csv('fec_unitemized.csv')
names(fec_u)
class(fec_u$itemized)
fec_u$itemized<-as.numeric(as.character(fec_u$itemized))
fec_u$unitemized<-as.numeric(as.character(fec_u$unitemized))
#View(fec_u)
cor<-cor(fec_u$itemized, fec_u$unitemized)
cor
summary(cor)

fec_reg<-lm(itemized~unitemized, data=fec_u)
summary(fec_reg)
