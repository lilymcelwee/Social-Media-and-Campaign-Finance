### Data
## Filtering
# Topic Modeling - Corpus Cleaning

# Citation: This script is based on code written by Bertie Vidgen, with extensions for additional cleaning operations. 

## Set working directory:
getwd()
setwd('/Users/Xyz27/Dropbox/Thesis/Data')

# Load packages:
library(slam); library(lsa); library(ggplot2); library(knitr); library(ggplot2); library(dplyr); library(stringr); library(tidyr); library(tibble); library(tm); library(SnowballC); library(topicmodels); library(quanteda); library(igraph)     
install.packages('tidytext')
library(tidytext)

## Load data:
df<-read.csv("cand_full.csv")
df$X<-NULL
df$text<-as.character(df$text)
df_clean<-df

# Check how many words are in the DF:
df_clean$freq<-str_count(df_clean$text,'\\w+')
sum(df_clean$freq) # With initial run, 506,984

# Remove special character:
for (i in 1:length(df_clean$text)){
  string<-strsplit(df_clean$text[i], ' ')
  for (j in string){
    string[j]<- gsub('\xe2\x80\x90', '', string[j])
  }
} 

# Remove special-character related hex terms
for (i in 1:length(df_clean$text)){
  #string<-strsplit(df_clean$text[i], ' ')
  df_clean$text[i] <- gsub('\\xe2\\x80\\x99', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xc3\\xa9', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x90', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x91', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x92', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x93', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x94', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x94', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x98', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x9b', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x9c', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x9c', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x9d', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x9e', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\x9f', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\xa6', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\xb2', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\xb3', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\xb4', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\xb5', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\xb6', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x80\\xb7', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x81\\xba', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x81\\xbb', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x81\\xbc', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x81\\xbd', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\\xe2\\x81\\xbe', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x99', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xc3\xa9', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x90', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x91', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x92', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x93', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x94', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x94', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x98', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x9b', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x9c', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x9c', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x9d', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x9e', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\x9f', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\xa6', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\xb2', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\xb3', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\xb4', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\xb5', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\xb6', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x80\xb7', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x81\xba', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x81\xbb', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x81\xbc', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x81\xbd', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('\xe2\x81\xbe', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('&amp;', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('&lt;', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('&gt;', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('xaa', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('xef', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
  df_clean$text[i] <- gsub('xahttp', '', df_clean$text[i], useBytes=TRUE, fixed=TRUE)
}

# Check how many words are in df_clean:
df_clean$freq<-str_count(df_clean$text,'\\w+')
sum(df_clean$freq) # With initial clean, 478,775

# Export the cleaned df_clean:
write.csv(df_clean, 'cand_full(nochar).csv')

# Remove the b from the first character of each string:
df_clean$text<-str_replace_all(df_clean$text, "[^[:alnum:]]", " ")
for (i in 1:length(df_clean$text)){
  if(substring(df_clean$text[i], 1, 1)=="b"){
    df_clean$text[i]<-substring(df_clean$text[i], 2)
  }
}

## Outlinks:
df_clean$domain<-gsub("https://t.co/|https://|http://|www.", "", df_clean$out_links)

# Save the file
write.csv(df_clean, 'cand_full(nospecialchar).csv')

# Reload the file:
df_clean<-read.csv('cand_full(nospecialchar).csv')
df_clean$X<-NULL

# Check characters:
char_fb<-df_clean[df_clean$platform=="facebook",]$text
print(length(char_fb))
sum(nchar(as.character(char_fb)))
char_tw<-df_clean[df_clean$platform=="twitter",]$text
print(length(char_tw))
sum(nchar(as.character(char_tw)))

# Reassign file:
df_clean_general<-df_clean

# First, we make a 'Corpus' of the text
docs = Corpus(VectorSource(df_clean_general$text))
# To make a Corpus straight out of the df_clean variable, we need to first use 'VectorSource'
# this turns the dataframe column into a 'vector source', which is basically a collection of vectors where each entry is considered a document.
# e.g.:
new = VectorSource(df_clean_general$text)
length(new) #18245 -> the same number of entries that we inputted.

##### AUTOMATIC CLEANING:
docs_clean = docs %>%
  tm_map(.,content_transformer(tolower)) %>%
  tm_map(.,removePunctuation) %>%
  tm_map(.,removeNumbers) %>%
  tm_map(.,removeWords, stopwords('english')) %>%
  tm_map(.,stripWhitespace) %>%
  tm_map(., stemDocument)

######### CHARACTER CHECK ###########
char_df<-data.frame(text = sapply(docs_clean, as.character), stringsAsFactors = FALSE)#, row.names=FALSE)
names(char_df)<-c("newtext")
cand_df<-read.csv("cand_full.csv")
clean_cand<-cbind(cand_df, char_df)
clean_cand<-clean_cand[,c("text", "platform", "newtext")]
char_fb<-clean_cand[clean_cand$platform=="facebook",]$newtext
print(length(char_fb))
sum(nchar(as.character(char_fb)))
char_tw<-clean_cand[clean_cand$platform=="twitter",]$newtext
print(length(char_tw))
sum(nchar(as.character(char_tw)))
##########################

##### MANUAL CLEANING:
## Nonwords:
nonwords <- c('amp', 'httpsxexxa', 'xexx', 'itxexx', 'xexxa','cotxcxa', 'froxexxa', 'hoosier', 'https', 'arpx', 
              'http', 't', 'co', 'st', 'pm', '\xe2\x80\x99s', 'hes', 'tri', 'rt', 'gtgt', 
              'de', 'else', 'youv', 'xf', 'xbc', 'www', 've', 'vawa', 'uiqehmezmc', 'lo', 'hadyvtxha', 
              'nhttps', 'derrcgfel', 'xc', 'xbo', 'est', 'kkfttksx', 'itta', 'brandt', 'akt', 'michel', 'smith', 
              'vasquez', 'brayton', 'boykin', 'walker', 'afn', 'fischer', 'edward', 'xs', 'xe', 'nl', 'loudobbs', 
              'htt', 'cfpb', 'lyman', 're', 'joyannreid', 'ain', 'laurenapplebaum', 'vuxhfrhf', 'ralph', 'watson',
              'davidbazzel', 'epudsmith', 'zvgxeyxi', 'katvelicia', 'cc', 'kffb','conway', 'rd', 'fnbfs', 'calhoun',
              'pryor', 'le', 'sa', 'su', 'isi', 'xa', 'shddrqzsmv', 'igmggk', 'nmebhhimi', 'ji', 'gxgyrixf','david',
              'diamond', 'ljpfney', 'schriock', 'ldifeefv', 'xbrbzea', 'xas','ll','re', 'ly', 'xfs')

# Text related to the 'no text' posts
text<-c("text", "no")

# Specific phrases:
# X terms:
xterm_vectors<-c()
for(i in letters){
  print(i)
  xterms<-paste("x", i, sep="")
  xterm_vectors<-append(xterm_vectors, xterms)
  #xterm_list[[i]]<-xterms
}
# XE terms:
xeterm_vectors<-c()
for(i in letters){
  print(i)
  xeterms<-paste("xe", i, sep="")
  xeterm_vectors<-append(xeterm_vectors, xeterms)
  #xterm_list[[i]]<-xterms
}
# XA terms:
xaterm_vectors<-c()
for(i in letters){
  print(i)
  xaterms<-paste("xa", i, sep="")
  xaterm_vectors<-append(xaterm_vectors, xaterms)
  #xterm_list[[i]]<-xterms
}
# XB terms:
xbterm_vectors<-c()
for(i in letters){
  print(i)
  xbterms<-paste("xb", i, sep="")
  xbterm_vectors<-append(xbterm_vectors, xbterms)
  #xterm_list[[i]]<-xterms
}

# Domains:
# Convert to lowercase and dataframe:
domains<-tolower(df_clean$domain)
domain_df_clean<-as.data.frame(domains)

# Split columnn by space to capture multiple links:
split_domdf_clean<-separate(data = domain_df_clean, col = domains, into = c("domain1", "domain2", "domain3"), sep = " ")
is.vector(split_domdf_clean$domain3)
domdf_clean<-stack(split_domdf_clean)#, select = c(names(domdf_clean)))
print(length(domdf_clean$values))
domains<-domdf_clean$values
domains

# Remove all non-alphanumeric characters:
domains<-str_replace_all(domains, "[^[:alnum:]]", " ")
domains
# Remove all emptry strings
domains<-domains[!is.element(domains, "")]
# REMOVE Numbers:
domains<-gsub('[0-9]+', '', domains)
# Remove those in the vector that don't have links (aka, the 'domain' part is NA)
domains<-domains[!domains %in% NA]
blankspace=""
empty=c("\n", "\n\n")
domains<-domains[!domains %in% blankspace]
domains<-domains[!domains %in% empty]

# Select only those at 10 char or less
#domains<-domains[!domains %in% NA]
domains<-domains[nchar(domains) < 11]
# Subset by two, because the length function makes it clear that there are columns with 1-2 white spaces
length_df_clean<-as.data.frame(nchar(as.character(domains_df_clean$domains)))
domains<-domains[nchar(domains) > 2]
# Dataframe of domains
domains_df_clean<-as.data.frame(domains)
names(domains_df_clean)
print(length(domains_df_clean$domains))
# Domains - split into smaller pieces, so easier to remove.
domains1<-domains[0:1000]
domains2<-domains[1000:2000]
domains3<-domains[2000:3000]
domains4<-domains[3000:4000]
domains5<-domains[4000:5000]
domains6<-domains[5000:6000]
domains7<-domains[6000:7000]
domains8<-domains[7000:8000]
domains9<-domains[8000:9000]
domains10<-domains[9000:10000]
domains11<-domains[10000:11000]
domains12<-domains[11000:12000]
domains13<-domains[12000:13000]
domains14<-domains[13000:14000] #13584

## Sen words:
# Load list of states:
states<-read.csv("states_abb.csv")
# Create list of states plus word 'sen':
abb<-tolower(states$abb)
end_sen<-paste(abb, "sen", sep="")
end_sen

# State abb + 'elex'
abb<-tolower(states$abb)
abb_elex<-paste(abb, "elex", sep="")

## Create list of states plus word 'polit' (Stemmed):
abb<-tolower(states$abb)
pol_sen<-paste(abb, "polit", sep="")
pol_sen

## Create list of states plus word 'pol' (Stemmed):
abb<-tolower(states$abb)
polit_sen<-paste(abb, "pol", sep="")
polit_sen

## Create list of states plus word 'leg' (Stemmed):
abb<-tolower(states$abb)
leg_sen<-paste(abb, "leg", sep="")
leg_sen

# Candidate twitter handles:
handle_df_clean<-read.csv("handle_table.csv")
handles<-tolower(as.vector(unique(handle_df_clean$handle)))
handles

## Custom stopwords:
customstopwords <- c('see', 'find', 'can', 'keep', 'will', 'way', 'make', 'also', 'other', 'need', 'another',
                     'sure', 'get', 'just','seem','abl','bit','alway', 'give', 'take', 'clear', 'doesnt', 'yet', 'fill', 'day', 
                     'now', 'put', 'another', 'made', 'ive', 'im', 'ill', 'dont','wont') 

## Remove the names of candidates:
# We've already loaded the file of candidate demographic information as 'df_clean', so I use this to get candidate names.
candnames<-unique(df_clean$name)
candnames<-as.vector(candnames)
candnames
cand_df_clean<-as.data.frame(candnames)
View(cand_df_clean)
# Getting first and last names in separate vectors:
library(stringr)
cand_df_clean_split<-str_split_fixed(cand_df_clean$candnames, " ", 2)
cand_df_clean_split<-as.data.frame(cand_df_clean_split)
first<-as.data.frame(cand_df_clean_split$V1)
names(first)<-c("names")
second<-as.data.frame(cand_df_clean_split$V2)
names(second)<-c("names")
first$names<-tolower(first$names)
second$names<-tolower(second$names)
firstname_vector<-as.vector(first$names)
secondname_vector<-as.vector(second$names)
# Stem these vectors:
firststem<-stemDocument(firstname_vector)
secondstem<-stemDocument(secondname_vector)
## Last name + "sen":
last_sen<-paste(secondname_vector, "sen", sep="")
last_sen

## Load the url dataset:
noquote<-read.csv('out_noquote.csv')
noquote<-subset(noquote, nchar(as.character(links)) > 0)
noquote$X<-NULL
# Remove extra space:
#require(stringr)
#example(str_trim)
#noquote$links<-str_trim(noquote$links)

## Get states:
states<-as.data.frame(unique(df_clean$state))
names(states)<-c("statenames")
states$statenames<-tolower(states$statenames)
states_vector<-as.vector(states$statenames)
states_stem<-stemDocument(states_vector)

## Letters
letters<-paste(letters[1:26])

## Conduct customized cleaning operations:
docs_clean = docs_clean %>% 
  tm_map(.,removeWords,customstopwords) %>%
  tm_map(.,removeWords,end_sen) %>%
  tm_map(.,removeWords,states_stem) %>%
  tm_map(.,removeWords,candnames) %>%
  tm_map(.,removeWords,pol_sen) %>%
  tm_map(.,removeWords,firststem) %>%
  tm_map(.,removeWords,secondstem) %>%
  tm_map(.,removeWords,last_sen) %>%
  tm_map(.,removeWords,abb_elex) %>%
  tm_map(.,removeWords,handles) %>%
  tm_map(.,removeWords,letters) %>%
  tm_map(.,removeWords,abb) %>%
  tm_map(.,removeWords,polit_sen) %>%
  tm_map(.,removeWords,leg_sen) %>%
  tm_map(.,removeWords,nonwords)%>%
  tm_map(.,removeWords,domains1) %>%
  tm_map(.,removeWords,domains2) %>%
  tm_map(.,removeWords,domains3) %>%
  tm_map(.,removeWords,domains4) %>%
  tm_map(.,removeWords,domains5) %>%
  tm_map(.,removeWords,domains6) %>%
  tm_map(.,removeWords,domains7) %>%
  tm_map(.,removeWords,domains8) %>%
  tm_map(.,removeWords,domains9) %>%
  tm_map(.,removeWords,domains10) %>%
  tm_map(.,removeWords,domains11) %>%
  tm_map(.,removeWords,domains12) %>%
  tm_map(.,removeWords,domains13) %>%
  tm_map(.,removeWords,domains14) %>%
  tm_map(.,removeWords,xterm_vectors)%>%
  tm_map(.,removeWords,xeterm_vectors)%>%
  tm_map(.,removeWords,xaterm_vectors)%>%
  tm_map(.,removeWords,xbterm_vectors)%>%
  tm_map(.,removeWords,text)


######### CHARACTER CHECK ###########
char_df<-data.frame(text = sapply(docs_clean, as.character), stringsAsFactors = FALSE)#, row.names=FALSE)
names(char_df)<-c("newtext")
cand_df<-read.csv("cand_full.csv")
clean_cand<-cbind(cand_df, char_df)
clean_cand<-clean_cand[,c("text", "platform", "newtext")]
char_fb<-clean_cand[clean_cand$platform=="facebook",]$newtext
print(length(char_fb))
sum(nchar(as.character(char_fb)))
char_tw<-clean_cand[clean_cand$platform=="twitter",]$newtext
print(length(char_tw))
sum(nchar(as.character(char_tw)))
##########################

# Create a final Document Term Matrix:
dtm = DocumentTermMatrix(docs_clean)
dtm_m = as.matrix(dtm) #matrix for inspection (it's not used in the analysis)
#convert rownames to filenames
rownames(dtm) = df_clean_general$id_specific  

## Calculate and plot the frequency of terms:
# Make a new dataframe 
term_freq = data.frame(term=names(colSums(as.matrix(dtm))),
                       frequency = colSums(as.matrix(dtm))) %>%
  arrange(desc(frequency))
View(term_freq)
summary(term_freq$frequency)

term_freq %>%
  head(term_freq, n=20) %>%
  mutate(term = factor(term, levels = term[order(desc(frequency))])) %>%
  ggplot(aes(term,frequency)) + 
  #geom_point() +
  geom_bar(stat='identity', fill = 'light blue') +
  theme(axis.text.x=element_text(angle=75, hjust=1)) +s
  ggtitle('Frequency of the top 20 terms')

# Now we can go back to the document to make one more edit - removing the least frequent terms
#leastfreq_three<-as.vector(term_freq[term_freq$frequency<3,]$term)
leastfreq_two<-as.vector(term_freq[term_freq$frequency<2,]$term)
length(leastfreq_two) # There are 10,016 terms that appear just once in the document. 
# What percentage of terms appear just once in the documents?
least_perc<-length(leastfreq_two)/length(term_freq$term)
least_perc # 53.12% of terms appear just twice in the document. 
# Threshold - terms that appear in the document just once.
leastfreq<-leastfreq_two

# Remove most frequent - the top 5
mostfreq<-as.vector(term_freq$term[0:5])
mostfreq

# Remove Least frequent 
leastfreq0<-leastfreq[0:1000]
leastfreq1<-leastfreq[1000:2000]
leastfreq2<-leastfreq[2000:3000]
leastfreq3<-leastfreq[3000:4000]
leastfreq4<-leastfreq[4000:5000]
leastfreq5<-leastfreq[5000:6000]
leastfreq6<-leastfreq[6000:7000]
leastfreq7<-leastfreq[7000:8000]
leastfreq8<-leastfreq[8000:9000]
leastfreq9<-leastfreq[9000:10000]
leastfreq10<-leastfreq[10000:11000]

# Now that we have split the least frequent terms, can run the code to remove them.
docs_clean = docs_clean %>% 
  tm_map(.,removeWords,leastfreq0)%>% 
  tm_map(.,removeWords,leastfreq1)%>% 
  tm_map(.,removeWords,leastfreq2)%>% 
  tm_map(.,removeWords,leastfreq3)%>% 
  tm_map(.,removeWords,leastfreq4)%>% 
  tm_map(.,removeWords,leastfreq5)%>% 
  tm_map(.,removeWords,leastfreq6)%>% 
  tm_map(.,removeWords,leastfreq7)%>% 
  tm_map(.,removeWords,leastfreq8)%>% 
  tm_map(.,removeWords,leastfreq9)%>% 
  tm_map(.,removeWords,leastfreq10)#%>% 

######### CHARACTER CHECK ###########
char_df<-data.frame(text = sapply(docs_clean, as.character), stringsAsFactors = FALSE)#, row.names=FALSE)
names(char_df)<-c("newtext")
cand_df<-read.csv("cand_full.csv")
clean_cand<-cbind(cand_df, char_df)
clean_cand<-clean_cand[,c("text", "platform", "newtext")]
char_fb<-clean_cand[clean_cand$platform=="facebook",]$newtext
print(length(char_fb))
sum(nchar(as.character(char_fb)))
char_tw<-clean_cand[clean_cand$platform=="twitter",]$newtext
print(length(char_tw))
sum(nchar(as.character(char_tw)))
##########################

## Most frequent:
docs_clean = docs_clean %>% 
  tm_map(.,removeWords,mostfreq)

######### CHARACTER CHECK ###########
char_df<-data.frame(text = sapply(docs_clean, as.character), stringsAsFactors = FALSE)#, row.names=FALSE)
names(char_df)<-c("newtext")
cand_df<-read.csv("cand_full.csv")
clean_cand<-cbind(cand_df, char_df)
clean_cand<-clean_cand[,c("text", "platform", "newtext")]
char_fb<-clean_cand[clean_cand$platform=="facebook",]$newtext
print(length(char_fb))
sum(nchar(as.character(char_fb)))
char_tw<-clean_cand[clean_cand$platform=="twitter",]$newtext
print(length(char_tw))
sum(nchar(as.character(char_tw)))
##########################

# Third, create a final Document Term Matrix:
dtm = DocumentTermMatrix(docs_clean)
dtm_m = as.matrix(dtm) #matrix for inspection (it's not used in the analysis)
#convert rownames to filenames
rownames(dtm) = df_clean_general$id_specific  

# Save the file:
clean_df<-data.frame(text = sapply(docs_clean, as.character), stringsAsFactors = FALSE)
# Add rownames:
clean_df$original_row<-rownames(clean_df)

# This shows that the first 187 rows of the dataframe are empty
newdata<-as.data.frame(clean_df[with(clean_df, order(text)), ])
rownames(newdata) <- NULL

# REVIEW THE ABOVE FILE TO DETERMINE WHICH OF THE FIRST X ROWS ARE EMPTY. Here, 306
empty=306
names(newdata)
newdata$empty<-"full"
newdata[1:empty,]$empty<-"empty"
print(length(noempty$text))

## Export new data as the original row file:
write.csv(newdata, 'origrows.csv')

# Manually, we CHECK WHERE EMPTY CUTOFF IS
## Having determined it, Remove rows 1-182, which are empty:
x=empty+1
noempty<-as.data.frame(newdata[x:nrow(newdata),])
# Reset row names, so that they will be consistent with TM output
rownames(noempty) <- NULL
write.csv(noempty, 'origrows(noempty).csv')

## THE 'newdata' file is necessary to MERGE with once have the documents -- because that will allow me to understand what the indexes are of the documents that ARE classified, based on the original file's indices.
without_index<-noempty[,c("text")]
write.table(without_index, 'short_clean_table.txt', col.names = FALSE)

##### COUNT TERMS:
# Count frequency of terms in new and old text
print(length(clean_df$text))
names(clean_df)<-"newtext"
# Combine with full df:
cand_df<-read.csv("cand_full.csv")
clean_cand<-cbind(cand_df, clean_df)
clean_cand<-clean_cand[,c("text", "platform", "newtext")]
# Number of words:
clean_cand$freq_orig<-str_count(clean_cand$text,'\\w+')
clean_cand$freq_new<-str_count(clean_cand$newtext,'\\w+')
# Number of characters:
clean_cand$freq_orig_char<-nchar(as.character(clean_cand$text))
class(clean_cand$text[2])
# Trim whitespace (leading and lagging) in the newtext column:
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
for(i in 1:length(clean_cand$newtext)){
  clean_cand$new_text_trimmed[i]<-trimws(clean_cand$newtext[i])
}
clean_cand$freq_new_char<-nchar(as.character(clean_cand$new_text_trimmed))

# Sum for all platforms:
sum(clean_cand$freq_orig)
sum(clean_cand$freq_new)
summary(clean_cand$freq_orig)
summary(clean_cand$freq_new)
summary(clean_cand$freq_orig_char)
summary(clean_cand$freq_new_char)

# By platforms:
## FACEBOOK
# WORDS:
unique(ave(clean_cand[clean_cand$platform=="facebook",]$freq_orig)) 
unique(ave(clean_cand[clean_cand$platform=="facebook",]$freq_new)) 
# CHARACTERS
unique(ave(clean_cand[clean_cand$platform=="facebook",]$freq_orig_char)) 
unique(ave(clean_cand[clean_cand$platform=="facebook",]$freq_new_char)) 

## TWITTER
## WORDS
unique(ave(clean_cand[clean_cand$platform=="twitter",]$freq_orig)) 
unique(ave(clean_cand[clean_cand$platform=="twitter",]$freq_new))
## CHARACTERS
unique(ave(clean_cand[clean_cand$platform=="twitter",]$freq_orig_char)) 
unique(ave(clean_cand[clean_cand$platform=="twitter",]$freq_new_char))

#### Topic Model Fitting ####

### What is the best number of topics?
# https://stats.stackexchange.com/questions/25113/the-input-parameters-for-using-latent-dirichlet-allocation

# Easy way to remove all documents without words:
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new<- dtm[rowTotals> 0,]           #remove all docs without words

burnin = 1000 
iter = 2000
thin = 500
seed =list(1,2,3,4,5)
nstart = 5
best = TRUE

print(length(dtm))
# START TIME: 3:39 on Wednesday afternoon
# dtm normal:
best.model.normal = lapply(seq(2,20, by=1), function(k){
  LDA(dtm.new, k, method='Gibbs', control=list(
    nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
})
print('done number 1')

## And to check the results:
# normal dtm:
dev.off()
normal.logLik <- as.data.frame(as.matrix(lapply(best.model.normal, logLik)))
normal.logLik.df <- data.frame(topics=c(2:20), LL=as.numeric(as.matrix(normal.logLik)))
normal.logLik.df$topics
## Plot the LL graph:
ll_graph<-ggplot(normal.logLik.df, aes(x=topics, y=LL))+geom_line()+
  theme_wsj(base_size=10, base_family="Verdana", title_family="Verdana")+scale_colour_wsj() + 
  theme(axis.title = element_text()) +  labs(x = "Number of Topics", y = 'Log-Likelihood')
ll_graph



