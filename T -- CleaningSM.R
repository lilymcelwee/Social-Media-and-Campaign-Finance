### DATA
## COLLECTION
# Donations

## Set working directory:
getwd()
setwd('/Users/Xyz27/Dropbox/Thesis/CODE')

# Libraries:
library(readxl)
library(reshape)
library(dplyr)
library(stringr)

## TWITTER - Load the Twitter dataset:
twitter_df<-read_excel("cand_twitter.xlsx")
print(length(twitter_df$twitter_handle)) #18244
twitter_df$date<-as.Date(twitter_df$date, origin = '1899-12-30') 
class(twitter_df$date)
## Cleaning
# Remove the unnecessary first (index) column:
twitter_df<-twitter_df[,-c(1)]
# Remove the time column:
twitter_df$time<-NULL
# Create handle column:
twitter_df$handle<-twitter_df$twitter_handle
twitter_df$twitter_handle<-NULL

## FACEBOOK
# Load the Facebook dataset:
facebook_df<-read_excel("cand_fb.xlsx")
# Cleaning
# Date format:
facebook_df$date<-as.Date(facebook_df$date, origin = '1899-12-30') 
# Remove index column:
facebook_df<-facebook_df[,-c(1)]
# Create handle column:
facebook_df$handle<-facebook_df$fb_handle
facebook_df$fb_handle<-NULL

## Merge the files:
full_df<-rbind(twitter_df, facebook_df)

## Check lengths and characters:
print(length(twitter_df$text)) #18244
print(length(facebook_df$text)) #2240
print(sum(nchar(facebook_df$text, type = "chars", allowNA = FALSE, keepNA = NA)))
print(length(full_df$text)) # 20484

## Write the csv to a new file:
write.csv(full_df, "text_clean.csv")

## Load it again:
full_df<-read.csv("text_clean.csv")
full_df$X<-NULL

## Set wd:
setwd('/Users/Xyz27/Dropbox/Thesis/Code')

## Load the data:
info<-read.csv('senate_searchinfo.csv')
## Examine file:
print(length(info$state)) # 108 candidates

# Create demographic dataframe:
demographic<-info[,c("state", "name", "Wikipedia.", "party", "incumbency","abb","search")]
print(length(demographic$state)) # 108 candidates

# Create platform dataframe:
# Remove some columns:
info$NA.<-NULL
info$access_handle1<-NULL
info$fbdefunct<-NULL
info$notweets<-NULL
info$access_facebook1<-NULL
info$access_facebook2<-NULL
info$fullfacebook1<-NULL
info$fullfacebook2<-NULL

# Create file:
long <- melt(info, id.vars = c("name", "state"), na.rm=TRUE)
names(long)<-c("name", "state", "handle_type", "handle")
long<-long[long$handle_type=="twitter1" | long$handle_type=="twitter2" | long$handle_type=="facebook1" | long$handle_type=="facebook2",]
print(length(long$name)) # 405
View(long)

# Merge the demographic df and the platform df:
handle_df<-merge(long, demographic, all.y=FALSE, by=c("name", "state"))#, all=FALSE)
handle_df<-handle_df[handle_df$handle!="",]
print(length(handle_df$name)) # 257

# Write to csv
write.csv(handle_df, "handle_df.csv")

## Load it again:
handle_df<-read.csv("handle_df.csv")
handle_df$X<-NULL
print(length(handle_df$name)) # 257

## Merge the handle_df and the text file:
# We need to merge the files individually (Facebook vs Twitter) because 
# some people have the same handle for each platform.
twitter_handle<-handle_df[handle_df$handle_type=="twitter1" | handle_df$handle_type=="twitter2",]
t_df<-merge(twitter_df, twitter_handle, by=c("handle"))

# Find difference with the original twitter df:
commonID<-intersect(t_df$handle, twitter_df$handle)

# We need to merge the files individually (Facebook vs Twitter) because some people have the 
# same handle for each platform.
facebook_handle<-handle_df[handle_df$handle_type=="facebook1" | handle_df$handle_type=="facebook2",]
print(length(facebook_handle$name)) # 130
# With the Facebook data, I need to extract the handle part of the url:
f_df<-merge(facebook_df, facebook_handle, by=c("handle"))
print(length(f_df$name)) # 2248

# Merge the twitter and facebook files:
full_df<-rbind(t_df, f_df)
print(length(full_df$handle)) # 20491 total posts, Facebook + Twitter
# Summary table:
han_tab<-full_df %>%
  group_by(name, period, handle_type) %>%
  count
han_tab<-as.data.frame(han_tab)
write.csv(han_tab, "handle_table.csv", col.names=TRUE)

## Clean the dataset
# Remove the unnecessary first (index) column:
full_df<-full_df[,-c(1)]

## Export the file:
write.csv(full_df, 'cand_full.csv')

## Import the file:
full_df<-read.csv("cand_full.csv")
print(unique(full_df$abb))
# Remove the unnecessary first (index) column:
full_df<-full_df[,-c(1)]

## Find the total number of words and characters per platform:
# Posts
fb_posts<-full_df[full_df$platform=="facebook",]$text
print(length(fb_posts))
sum(nchar(as.character(fb_posts)))
tw_posts<-full_df[full_df$platform=="twitter",]$text
print(length(tw_posts))
sum(nchar(as.character(tw_posts)))

## ADD STATE INFO:
## Set wd:
setwd('/Users/Xyz27/Dropbox/Thesis/DATA')

## Load the state population data:
pop<-read.csv('state_pop.csv')

## Add pop data to the main df:
main<-merge(full_df, pop, by=c("abb", "state"))
full_df<-main

## Export full dataset:
write.csv(full_df, 'cand_full.csv')
print(unique(full_df$name))

## ADD LINKS IN EACH MESSAGE as a new column:
url_pattern <- "http\\S+\\s*"
links<-str_extract_all(full_df$text, url_pattern)
links
link_df<-plyr::ldply(links, rbind)
cols<-names(link_df)
# Paste esch link into same column for a given document:
outlinks<-do.call(paste, c(link_df[cols], sep=" "))
outlink_df<-as.data.frame(outlinks)
write.csv(outlink_df, 'out.csv')
# Load the file, which has had quotes manually removed:
out<-read.csv("out_noquote.csv")
out$X<-NULL
out_links = unlist(list(as.character(out$outlinks)))
out_links<-str_replace_all(out_links, "NA", "")

# LINK DF:
full_links<-cbind(full_df, out_links)
full_links$contains_link<-ifelse(full_links$out_links=="NA", 0,1)
full_df<-full_links

## Export the file:
write.csv(full_df, 'cand_full.csv')
print(unique(full_df$name))


