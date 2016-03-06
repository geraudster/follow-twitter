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

client_htag <- 'Toulouse'
location <- '43.604652,1.444209,10km'

# get the 1,500 most recent tweets mentioning '@airfrance'
client.tweets <- searchTwitter(client_htag, geocode = location, n=200)

library(plyr)
tweets <- ldply(client.tweets, function(t) {
  data.frame(text = iconv(t$getText(), "UTF-8", "UTF-8", sub=""),
             favoriteCount = t$getFavoriteCount(),
             retweetCount = t$getRetweetCount(),
             latitude = as.numeric(ifelse(is.null(t$getLatitude()), NA, t$getLatitude())),
             longitude = as.numeric(ifelse(is.null(t$getLongitude()), NA, t$getLongitude())),
             screenName = t$screenName,
             stringsAsFactors = FALSE)
})
tweets <- subset(tweets, !is.na(latitude))

library(ggmap)
bbox <- make_bbox(longitude, latitude, data = tweets)
map <- get_map(bbox, source = 'osm')
plot.map <- ggmap(map)
plot.map <- plot.map + geom_point(aes(x=longitude, y=latitude, color=screenName), data = tweets)
plot.map <- plot.map + geom_path(aes(x=longitude, y=latitude, color=screenName), size=0.8,
                                 linetype=2,
                                 data=tweets)

tweetsForGoogleMap <- tweets
tweetsForGoogleMap$latLon <- paste(tweetsForGoogleMap$latitude,
                                   tweetsForGoogleMap$longitude,
                                   sep=':')
library(htmltools)
tweetsForGoogleMap$parsed_text <- gsub('\n', '<br/>', tweetsForGoogleMap$text)

nrow(tweetsForGoogleMap)

library(googleVis)
googleMap <- gvisMap(tweetsForGoogleMap, locationvar = 'latLon',
                     tipvar = 'parsed_text',
                     options = list(dataMode="markers"))
plot(googleMap)

