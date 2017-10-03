### DATA
## COLLECTION
# Donations

## Set working directory
setwd('/Users/Xyz27/Dropbox/Thesis/DATA')

## Edit workspace and load necessary packages. Source: Bertie Vidgen, devtools::install_github("bvidgen/RPackage")
rm(list=ls()) 
options(scipen=10) 
fun_check_packages <- function(x){
  for (i in seq(1,length(x))){
    if (!x[i] %in% installed.packages()){
      install.packages(x[i])}
    library(x[i],character.only=TRUE)}
}
packs = c('ggplot2', 'LSD','dplyr','tidyr','scales','knitr')
fun_check_packages(packs); rm(packs)
devtools::install_github("bvidgen/RPackage")
library("RPackage")
library(ggthemes)
library(readxl)

## Load the donation data
c<-read_excel("maplight_edit.xlsx", sheet=1)

# Rename the columns:
names(c)<-c("FECID", "maplight_name", "state", "period2", "period3", "period4","period5", "period6")
names(c)<-c("FECID", "maplight_name", "state", "period2", "count2", "period3", "count3", "period4", "count4", "period5", "count5", "period6", "count6")

## Find missing donation data altogether:
names<-read.csv('google_adwordsdata.csv')
#View(names)
diff<-setdiff(names$name,c$maplight_name)
diff

## Cash totals:
cash<-c[,c("FECID", "maplight_name", "state", "period2", "period3", "period4","period5", "period6")]
cash<-as.data.frame(cash)

## Count totals:
count<-c[,c("FECID", "maplight_name", "state", "count2", "count3", "count4","count5", "count6")]
count<-as.data.frame(count)

# Reshape the data to collapse the CASH-periods into one column:
rcash<-reshape(cash, varying=c("period2", "period3", "period4", "period5", "period6"), v.names="cash_total", timevar="period", direction="long")
rcash$period<-ifelse(rcash$period==1, 2, ifelse(rcash$period==2, 3, ifelse(rcash$period==3, 4, ifelse(rcash$period==4, 5, ifelse(rcash$period==5,6, rcash$period)))))
rcash$id<-NULL

# Reshape the data to collapse the COUNTS-periods into one column:
rcount<-reshape(count, varying=c("count2", "count3", "count4","count5", "count6"), v.names="cash_count", timevar="period", direction="long")
rcount$period<-ifelse(rcount$period==1, 2, ifelse(rcount$period==2, 3, ifelse(rcount$period==3, 4, ifelse(rcount$period==4, 5, ifelse(rcount$period==5,6, rcount$period)))))
rcount$id<-NULL

## Combine the dataframes:
r<-cbind(rcash, rcount)
names(r)
r<-r[,c("FECID", "maplight_name", "state", "period", "cash_total", "cash_count")]

## Create a column in which the donation sum is a number:
r$cash_total<-substr(r$cash_total, 2, 1000000L)
r$cash_total <- gsub(",", "", r$cash_total)
r$cash_total<-as.numeric(r$cash_total)
class(r$cash_total)
r$cash_count<-sub("\\$","",r$cash_count) # Don't need a dollar sign in front of the cash count values
r$cash_count<-as.numeric(r$cash_count)

## Create a scaled variable - total and count donations for each candidate (5000s):
r[is.na(r)] <- 0
r$scaled_total<-r$cash_total*(1/5000)
r$scaled_cash_over_count<-r$scaled_total/r$cash_count
r[is.na(r)] <- 0

## Create donation file:
write.csv(r, "donations_total.csv")

## Load the donations data:
r<-read.csv('donations_total.csv')

##### HISTOGRAMS
## Cash
hist<-ggplot(data=r, aes(scaled_total)) + ylab("Count") + geom_histogram(bins=50,colour = "dark green", fill="dark green") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = "Donations ($5000s)", y = 'Count')
hist
# Log histogram of cash:
hist<-ggplot(data=r, aes(log(scaled_total))) + ylab("Count") + geom_histogram(bins=50,colour = "dark green", fill="dark green") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = "Donations ($5000s, Log)", y = 'Count')
hist
## Count
hist<-ggplot(data=r, aes(cash_count)) + ylab("Count") + geom_histogram(bins=50,colour = "dark green", fill="dark green") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = "Donations (#)", y = 'Count')
hist
# Log histogram of cash:
hist<-ggplot(data=r, aes(log(cash_count))) + ylab("Count") + geom_histogram(bins=50,colour = "dark green", fill="dark green") + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = "Donations (#, Log)", y = 'Count')
hist

## Bin data & Plot Error Bars
names(r)
# inspect the range
range(r$cash_count)
# inspect the quantile values
quantile(r$cash_count)
#adjust the range of quantile values
quantile(r$cash_count, probs = seq(0,1,0.1))
# Use the error_bars() function from the bvidgen/RPackage package:
r_eb<-r[,c("cash_count", "scaled_cash_over_count")]
r_eb[is.na(r_eb)] <- 0
#range(r_eb$scaled_total)
#range(r_eb$scaled_cash_over_count)
error_bars.out = error_bars(r_eb) #first, create the error_bars.out object
error_bars.out$plot #inspect the plot
error_bars.out$df.summary #inspect the r.summary with all of the summary variables (this can be used for further analysis).
error_bars.out$df
# Based on inspection, it looks like the appropriate bins would be the following:
s = seq(813,8131,by=800) #we use our exploratory analysis to decide how to cut it into bins
error_bars.out.2 = error_bars(r_eb, s)
error_bars.out.2$plot 
error_bars.out.2$df.summary  
error_bars.out.2$df

error.out = error_bars(r_eb, draw = 'standard error')
error.out$plot
df.summary = error.out$df.summary
df.extended = error.out$df

#df.summary = error_bars.out.2$df.summary
# Plot - Lin
View(df.summary)
eb_linlin<-ggplot(df.summary, aes(xmean, ymean)) +
  geom_point(size = 2) + ylim(0,0.2)+
  geom_errorbar(aes(ymin = ymean_minus_se, ymax = ymean_plus_se, width=0.75)) +
  geom_smooth(color = 'red', method = 'lm', se = F) +
  geom_point(data = df.extended, aes(x,y), color = 'blue', alpha=0.01) +
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() +
  theme(axis.title = element_text()) +  labs(x = "Count", y = 'Sum ($5000s)/Count')
eb_linlin

# Plot - Log
r_eb.log2 = data.frame(logx = log10(r_eb$cash_count+1),
                       logy = log10(r_eb$scaled_cash_over_count+1))
#View(r_eb.log2)
r_eb.log2[is.na(r_eb.log2)] <- 0
# ## Method 1 -- add constant
error_bars.out.log = error_bars(r_eb.log2, draw = 'both')
error_bars.out.log$plot
df.summary = error_bars.out.log$df.summary
df.extended = error_bars.out.log$df

eb_loglog<-ggplot(df.summary, aes(xmean, ymean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ymean_minus_se, ymax = ymean_plus_se, width=0.75)) +
  #geom_smooth(color = 'red', method = 'lm', se = F) + 
  ylim(0,max(df.summary$ymean_minus_se))+
  geom_point(data = df.extended, aes(x,y), color = 'blue', alpha=0.01) +
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() +
  theme(axis.title = element_text()) +  labs(x = "Count, Log", y = 'Sum ($5000s)/Count, Log')
eb_loglog



######## Sum 
## Aggregate by candidate:
library(dplyr)
names(r)
cand<-r %>% group_by(maplight_name) %>% summarise(sum_count=sum(cash_count), sum_scaled_total=sum(scaled_total))
View(cand)
View(r)
cand$sum_total_over_count<-cand$sum_scaled_total/cand$sum_count
cand[is.na(cand)]<-0
#View(cand)
# Plot of per-candidate distribuion: CASH
hist<-ggplot(data=cand, aes(sum_scaled_total)) + geom_histogram(bins=20,colour = "dark green", fill="dark green") + ylim(0,10) + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + xlim(min(cand$sum_scaled_total),max(cand$sum_scaled_total)) + 
  theme(axis.title = element_text()) +  labs(x = "Total Donation Sum ($5000s)", y = 'Count')
hist
# Log:
hist<-ggplot(data=cand, aes(log(sum_scaled_total))) + geom_histogram(bins=10,colour = "dark green", fill="dark green") + ylim(0,10) + 
  xlim(0, max(log(cand$sum_scaled_total))) + theme_wsj(base_size=8, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = "Total Donation Sum ($5000s, Log)", y = 'Count')
hist

# Plot of per-candidate distribution: CASH
hist<-ggplot(data=cand, aes(sum_count)) + geom_histogram(bins=50,colour = "dark green", fill="dark green") + ylim(0,10) + 
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + xlim(min(cand$sum_count),max(cand$sum_count)) +  
  theme(axis.title = element_text()) +  labs(x = "Weekly Donations (#)", y = 'Count')
hist
# Log:
hist<-ggplot(data=cand, aes(log(sum_count))) + geom_histogram(bins=20,colour = "dark green", fill="dark green") + ylim(0,10) + 
  xlim(0, max(log(cand$sum_count))) + theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = "Weekly Donations (#, Log)", y = 'Count')
hist





