library(twitteR)
library(ggplot2)
library(SnowballC)
library(tm)
library(wordcloud)

consumerKey <- "Qzfpe1WSN6vn3OOpXjIMJQ"
consumerSecret <- "CosQtBzrT0mBCW97zeJgehMNoOiLIkxrctc3l4xBYXk"

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"

twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)

twitCred$handshake()
registerTwitterOAuth(twitCred)


rdmTweets <- userTimeline("rdatamining", n=200)

df <- do.call("rbind", lapply(rdmTweets, as.data.frame))

myCorpus <- Corpus(VectorSource(df$text))

# convert to lower case
myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords('english'), "available", "via") # remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)


# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)

# stem completion
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=myCorpusCopy)

# count frequency of "mining"
miningCases <- tm_map(myCorpusCopy, grep, pattern="\\<mining")
# count frequency of "miners"
minerCases <- tm_map(myCorpusCopy, grep, pattern="\\<miners")
# replace "miners" with "mining"
myCorpus <- tm_map(myCorpus, gsub, pattern="miners", replacement="mining")

myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))

idx <- which(dimnames(myTdm)$Terms == "r")

termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
qplot(names(termFrequency), termFrequency, geom="bar", xlab="Terms") + coord_flip()

m <- as.matrix(myTdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
# word cloud
set.seed(375) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F, colors=grayLevels)