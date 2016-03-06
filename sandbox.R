## Search Twitter for airline mentions & collect tweet text
library(stringr)
#install.packages("twitteR");
#install.packages("base64enc");
# load the package
library(twitteR)

CUSTOMER_KEY <- TWITTER_CUSTOMER_KEY
CUSTOMER_SECRET <- TWITTER_CUSTOMER_SECRET
ACCESS_TOKEN <- TWITTER_ACCESS_TOKEN
ACCESS_secret <- TWITTER_ACCESS_SECRET

#setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET)

setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret)

client_htag <- '#ParisAttacks'
client_htag <- '@Valtech'
client_htag <- '@AirFrance'

# get the 1,500 most recent tweets mentioning '@airfrance'
client.tweets <- searchTwitter(client_htag, n=100)

library(plyr)
tweets <- ldply(client.tweets, function(t) {
  data.frame(text = iconv(t$getText(), "UTF-8", "UTF-8", sub=""),
             favoriteCount = t$getFavoriteCount(),
             retweetCount = t$getRetweetCount(),
             stringsAsFactors = FALSE)
  })

## Load sentiment word lists

score.sentiment <- function(tweets, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  sentences <- tweets$text
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = do.call(function(text, favoriteCount, retweetCount, favoriteCountScale, retweetCountScale) {
    sentence <- text
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', ' ', sentence)
    sentence = gsub('[[:cntrl:]]', ' ', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    score * retweetCountScale
  }, tweets )
  
  scores.df = data.frame(score=scores, text=sentences)
  scores.df
}

if(!dir.exists('data')) dir.create('data')

download.file('https://raw.githubusercontent.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/master/data/opinion-lexicon-English/negative-words.txt',
              'data/negative-words.txt',
              method = 'curl')
download.file('https://raw.githubusercontent.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/master/data/opinion-lexicon-English/positive-words.txt',
              'data/positive-words.txt',
              method = 'curl')

hu.liu.pos <- scan('data/positive-words.txt',
                   what='character',
                   comment.char=';')

hu.liu.neg <- scan('data/negative-words.txt',
                   what='character',
                   comment.char=";")


#pos.words = c(hu.liu.pos, 'upgrade');
#neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical');

pos.words <- hu.liu.pos
neg.words <- hu.liu.neg


## score sentiment for each tweet

## add sample ...

tweets$favoriteCountScale <- ((tweets$favoriteCount - min(tweets$favoriteCount)) / (max(tweets$favoriteCount) - min(tweets$favoriteCount)))
tweets$favoriteCountScale[is.na(tweets$favoriteCountScale)] <- 0
tweets$retweetCountScale <- ((tweets$retweetCount - min(tweets$retweetCount)) / (max(tweets$retweetCount) - min(tweets$retweetCount)))
tweets$retweetCountScale[is.na(tweets$retweetCountScale)] <- 0

client.scores <- score.sentiment(tweets, pos.words, neg.words, .progress='text')
hist ( client.scores$score)
library(ggplot2)
ggplot(client.scores, aes(x=score)) + geom_histogram() + ggtitle(client_htag)

+ scale_x_discrete()

range(client.scores$score)
+ scale_x_discrete()
  scale_x_discrete(limits = min(client.scores$score):max(client.scores$score))

client.scores$score
range(client.scores$score )
