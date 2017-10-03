### Data
## Filtering
# Create Time-Lagged Dataset

# Set the working directory:
getwd()
setwd('/Users/Xyz27/Dropbox/Thesis/DATA')

# Social media presence
handles<-read.csv('handle_table.csv')
print(length(unique(handles$name)))

## Missing social media data altogether:
# FULL
names<-read.csv('google_adwordsdata.csv')
print(length(names$name))
diff<-setdiff(names$name,unique(handles$name))
diff

# Load the full file:
new_df<-read.csv('cand_full_cleancombine(withempty).csv')
print(length(unique(new_df$name)))
print(length(new_df$name))
#View(new_df)

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

##### THERE's a need to create line up the social media data in period X with the donations data in period X+1. Because we have donations data from period 2 on, we simply
# move this column forward so that the social media data in period 1, for instance, is aligned with the donation data in period 2. 
library(useful)
shift<-shift.column(new_df, columns=c("scaled_total","cash_count"), newNames=c("donations_sum_lag","donations_count_lag"), len = 1L, up = TRUE)

## How many have wikipedia articles?
wp<-shift[,c("name", "Wikipedia.")]
wp_df<-wp[!duplicated(wp), ]
#View(wp_df)
wp_df$wp<-ifelse(wp_df$Wikipedia.=="n",0,1)
print(sum(wp_df$wp))

## Export the full_lag file:
shift$original_row<-NULL
names(shift)
shift$sum_type<-NULL
write.csv(shift, 'full_lag.csv')