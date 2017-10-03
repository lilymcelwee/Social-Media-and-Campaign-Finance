### DATA
## Filtering
# Dataset Construction

## Set working directory:
getwd()
setwd('/Users/Xyz27/Dropbox/Thesis/DATA')

## Load necessary files:
# Volume and demographics:
cand_full<-read.csv("cand_full_class.csv")
#View(cand_full)
print(length(cand_full$abb))
cand_full$X<-NULL
table(cand_full$name, cand_full$period)
View(cand_full)

## Create an empty dataframe, with a candidate-period observation for all candidate periods. Then merge this with the cand_full file, 
# in order to ensure that there are a full number of observations in that file.
searchinfo<-read.csv("senate_searchinfo.csv")
names<-as.data.frame(unique(searchinfo$name))
names(names)<-c("name")
periods<-unique(cand_full$period)

View(searchinfo)
# CREATE A DATAFRAME WITH THESE TOGETHER
full <- as.data.frame(matrix(ncol = 2))
names(full)<-c("name", "period")
for(i in 1:6){
  p<-as.data.frame(matrix(ncol = 2,nrow=length(unique(names$name))))
  names(p)<-c("name", "period")
  p$name<-names$name
  p$period<-i
  full<-rbind(full, p)
}
full<-full[-1,]
print(length(full$name))

# Merge the file with cand_full, to create a dataframe with rows for each candidate-period observation.
expand<-merge(cand_full, full, by=c("name", "period"), all.y=TRUE)

# Set cand_full to exp
cand_full<-expand

View(cand_full)
# Wikipedia:
wiki<-read.csv("wiki_period.csv")
print(length(wiki$wikipedia_article))
wiki$X<-NULL
names(wiki)
#View(wiki)

# Donations:
cont<-read.csv("donations_total.csv")
print(length(cont$FECID))
cont$X<-NULL
names(cont)
#View(cont)

## SM Volume
smvolume<-read.csv('smvolume.csv')
print(length(smvolume$name))
smvolume$X<-NULL
#View(smvolume)

## SM Type
smtype<-read.csv('smtype_totals.csv')
print(length(smtype$name))
smtype$X<-NULL

# Find out unique candidates in each file:
print(length(unique(cand_full$name))) # SM Information of some sort (either T, FB, or both) = 96 candidates
print(length(unique(wiki$wikipedia_article))) # Wikipedia = 68 candidates
print(length(unique(cont$maplight_name))) # Donations = 83 candidates
print(length(unique(smvolume$name))) # SM Volume = 96 candidates
print(length(unique(smtype$name))) # SM Type = 96 candidates
print(length(unique(gst$keyword))) # gst = 108 candidates
print(unique(cand_full$name)) # SM Information of some sort (either T, FB, or both) = 94 candidates
print(length(unique(wiki$wikipedia_article))) # Wikipedia = 68 candidates
print(length(unique(cont$maplight_name))) # Donations = 83 candidates
print(length(unique(smvolume$name))) # SM Volume = 96 candidates

## Make the names consistent across datasets:
names(cand_full)
names(wiki)
names(cont)
names(smvolume)
names(smtype)

## Editing names
# Can combine wiki and cand_full on 'Wikipedia.'
names(wiki)<-c("Wikipedia.", "period", "wiki_views") 
names(wiki)

# Can combine cont and cand_full on 'name' and 'abb' and 'period'
names(cont)<-c("FECID", "name", "abb", "period", "cash_total", "cash_count", "scaled_total")
# Can combine smvolume and cand_full on 'name' and 'period'
# Can combine smtype and cand_full on 'name' and 'period'
# Can combine d4 and gst on 'name'
names(gst)<-c("name", "scaled_gst")

## Merging the files:
table(cand_full$name, cand_full$period)
d1<-merge(cand_full, wiki, by=c("Wikipedia.", "period"), all.x=TRUE)
table(d1$name, d1$period)
d2<-merge(d1, cont, by=c("name", "abb", "period"), all.x=TRUE)
table(d2$name, d2$period)
d3<-merge(d2, smvolume, by=c('name', 'period', 'platform'), all.x=TRUE)
table(d3$name, d3$period)
d4<-merge(d3, smtype, by=c("name", "period"), all.x=TRUE)
table(d4$name, d4$period)

## Remove duplicates:
d4<-d4[!duplicated(d4),]
table(d4$name, d4$period)
#View(d4)
#am<-d4[d4$name=="Arn Menconi",]

## Export the large file:
write.csv(d4, "full_data.csv")
names(d4)
print(length(d4$name))

## Select specific columns:
# Name
# Period
# Abb
# state
# platform
# party
# incumbency
# population
# wiki_views
# FECID
# cash_total
# cash_count
# post_volume
# topics: "none"        "topic1"      "topic10"     "topic2"      "topic3"      "topic4"      "topic5"      "topic6"     "topic7"      "topic8"      "topic9"      "sum_type" 

d5<-d4[,c("name", "period", "text", "abb", "state", "party", "incumbency", "population", "wiki_views", "FECID", "cash_total", "cash_count", "scaled_total", "post_volume", "none", "topic1", "topic2", "topic3", "topic4", "topic5", "topic6", "topic7", "topic8", "topic9", "topic10", "sum_type", "maxname")]
names(d5)<-c("name", "period", "text", "abb", "state", "party", "incumbency", "population", "wiki_views", "FECID", "cash_total", "cash_count", "scaled_total", "post_volume", "none", "topic1", "topic2", "topic3", "topic4", "topic5", "topic6", "topic7", "topic8", "topic9", "topic10", "sum_type", "maxname")
d5 <- d5[order(d5$original_row),]

## Full DF:
write.csv(d5, 'cand_full_cleancombine.csv')
names(d5)

########### Add the additional observations
## ADD THE OTHERS BACK:
new_df<-d5
table(new_df$name, new_df$period)
## Correct the format of the donation and social media data for those missing it - set to zero:
# Donations
#nodonations<-new_df[is.na(new_df$cash_total) & new_df$period!=6,]
#print(unique(nodonations$name))
new_df$cash_count<-ifelse(is.na(new_df$cash_count) & new_df$period!=1,0, new_df$cash_count)
new_df$cash_total<-ifelse(is.na(new_df$cash_total)  & new_df$period!=1,0, new_df$cash_total)
new_df$scaled_total<-ifelse(is.na(new_df$scaled_total)  & new_df$period!=1,0, new_df$scaled_total)
# SM Volume
names(new_df)
new_df$topic1<-ifelse(is.na(new_df$topic1),0, new_df$topic1)
new_df$topic2<-ifelse(is.na(new_df$topic2),0, new_df$topic2)
new_df$topic3<-ifelse(is.na(new_df$topic3),0, new_df$topic3)
new_df$topic4<-ifelse(is.na(new_df$topic4),0, new_df$topic4)
new_df$topic5<-ifelse(is.na(new_df$topic5),0, new_df$topic5)
new_df$topic6<-ifelse(is.na(new_df$topic6),0, new_df$topic6)
new_df$topic7<-ifelse(is.na(new_df$topic7),0, new_df$topic7)
new_df$topic8<-ifelse(is.na(new_df$topic8),0, new_df$topic8)
new_df$topic9<-ifelse(is.na(new_df$topic9),0, new_df$topic9)
new_df$topic10<-ifelse(is.na(new_df$topic10),0, new_df$topic10)
new_df$none<-ifelse(is.na(new_df$none),0, new_df$none)
new_df$sum_type<-ifelse(is.na(new_df$sum_type),0, new_df$sum_type)
new_df$post_volume<-ifelse(is.na(new_df$post_volume),0, new_df$sum_type)

# SM Type - convert the maxname to "none"
new_df$maxname<-ifelse(is.na(new_df$maxname),"none", new_df$maxname)

## Create variable that assesses whether shares (campaign-related) or (policy/issue-related):
# Campaign-related: 1, 3, 4, 8, 9
# Policy-related: 2, 5, 6, 7, 10
new_df$issue_related<-rowSums(new_df[,c("topic2", "topic5", "topic6", "topic7", "topic10")])
new_df$campaign_related<-rowSums(new_df[,c("topic1", "topic3", "topic4", "topic8", "topic9")])
new_df$issue_share<-new_df$issue_related/new_df$post_volume
new_df$campaign_share<-new_df$campaign_related/new_df$post_volume
new_df$issue_share<-ifelse(is.na(new_df$issue_share),0,new_df$issue_share)
new_df$campaign_share<-ifelse(is.na(new_df$campaign_share),0,new_df$campaign_share)

## If wiki_views = NA, convert to zero
new_df$wiki_views<-ifelse(is.na(new_df$wiki_views), 0, new_df$wiki_views)

# Add graph - Distribution of Campaign-Related vs Issue-Related Posts as a Share of Total Posts (per candidate-period)
# (Total = 1, share each way filled)
View(new_df)
#View(new_df)
# Wikiviews:
## If wiki_views = NA, convert to zero
new_df$wiki_views<-ifelse(is.na(new_df$wiki_views), 0, new_df$wiki_views)

# Select the relevant demographic inforamtion from the saerchinfo file, to add back in
rel<-searchinfo[,c("name", "state", "abb", "party", "Wikipedia.", "incumbency", "outcome")]
names(new_df)
test<-new_df
test$incumbency<-NULL
test$population<-NULL
test$state<-NULL
test$abb<-NULL
test$party<-NULL
t<-merge(test, rel, by=c("name"), all.x=TRUE)

# Add population info:
pop<-read.csv('state_pop.csv')
main<-merge(t, pop, by=c("abb", "state"), all.x=TRUE)

# Reset file
new_df<-main
new_df$incumbency<-ifelse(new_df$incumbency=="I", 1,0)

# Just one candidate entry per period
test<-new_df
test$text<-NULL
df_dups <- test[c("name", "period")]
test<-test[!duplicated(df_dups),]
#View(test)
new_df<-test

## Add GST:
gst<-read.csv("gst_scaled.csv")
gst$X<-NULL
names(gst)<-c("name", "period", "scaled_gst")
#View(gst)

# Merge the GST file with the main file:
main<-merge(new_df, gst, by=c("name", "period"), all.x=TRUE)
#View(main)
new_df<-main

######## EXPORT
## Write the full file:
new_df$original_row<-NULL
write.csv(new_df, "cand_full_cleancombine(withempty).csv")
print(length(new_df$name))

## MERGE THE DATASETS:
smd<-read.csv("TOMERGE_SM_Donation_RegressionData.csv")
full<-read.csv("TOMERGE_full_data.csv")
full$X<-NULL
names(smd)
names(full)
