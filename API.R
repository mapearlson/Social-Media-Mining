library("ROAuth")
library("twitteR")
library("wordcloud")
library("tm")
library(streamR)

consumerKey <- "xxxxx"
consumerSecret <- "yyyyyyyyy"
cred <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, requestURL='https://api.twitter.com/oauth/request_token', accessURL='https://api.twitter.com/oauth/access_token', authURL='https://api.twitter.com/oauth/authorize')
                         
cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
registerTwitterOAuth(cred)

filterStream("tweets.json", track = c("fraud"), location = c(-74, 40, -73, 41), timeout = 60, oauth = cred)

tweets.df <- parseTweets("tweets.json", simplify = TRUE)

c( length(grep("obama", tweets.df$text, ignore.case = TRUE)),length(grep("biden", tweets.df$text, ignore.case = TRUE)) )

filterStream("tweetsUS.json", locations = c(-125, 25, -66, 50), timeout = 300, oauth = cred)
tweets.df <- parseTweets("tweetsUS.json", verbose = FALSE)

library(ggplot2)
library(grid)

map.data <- map_data("state")
points <- data.frame(x = as.numeric(tweets.df$lon), y = as.numeric(tweets.df$lat))
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", color = "grey20", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), plot.background = element_blank(), plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, aes(x = x, y = y), size = 1, alpha = 1/5, color = "darkblue")

###Second method
fraud<- searchTwitter("#bigdata", n=1500)
fraud_text <- sapply(fraud, function(x) x$getText())
fraud_text_corpus <-Corpus(VectorSource(fraud_text))
#clean up
fraud_text_corpus <- tm_map(fraud_text_corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
fraud_text_corpus <- tm_map(fraud_text_corpus, tolower, mc.cores=1) 
fraud_text_corpus <- tm_map(fraud_text_corpus, removePunctuation, mc.cores=1)
fraud_text_corpus <- tm_map(fraud_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
wordcloud(fraud_text_corpus)

###Method 3
# Read tweets from Twitter using ATOM (XML) format
###
 
# installation is required only required once and is rememberd across sessions
install.packages('XML') 
 
# loading the package is required once each session
require(XML)
 
# initialize a storage variable for Twitter tweets
mydata.vectors <- character(0)
 
# paginate to get more tweets
for (page in c(1:15))
{
    # search parameter
    twitter_q <- URLencode('#prolife OR #prochoice')
    # construct a URL
    twitter_url = paste('https://search.twitter.com/search.atom?q=',twitter_q,'&rpp=100&page=', page, sep='')
    # fetch remote URL and parse
    mydata.xml <- xmlParseDoc(twitter_url, asText=F)
    # extract the titles
    mydata.vector <- xpathSApply(mydata.xml, '//s:entry/s:title', xmlValue, namespaces =c('s'='http://www.w3.org/2005/Atom'))
    # aggregate new tweets with previous tweets
    mydata.vectors <- c(mydata.vector, mydata.vectors)
}
 
# how many tweets did we get?
length(mydata.vectors)