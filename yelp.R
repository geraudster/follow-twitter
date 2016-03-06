install.packages('httpuv')
require(httr)
require(httpuv)
require(jsonlite)
require(base64enc)

consumerKey <- YELP_CONSUMER_KEY
consumerSecret <- YELP_CONSUMER_SECRET
token <- YELP_TOKEN
token_secret <- YELP_TOKEN_SECRET

yelp_query <- function(path, query_args) {
  # Use OAuth to authorize your request.
  myapp <- oauth_app("YELP", key=consumerKey, secret=consumerSecret)
  sig <- sign_oauth1.0(myapp, token=token, token_secret=token_secret)
  
  # Build Yelp API URL.
  scheme <- "https"
  host <- "api.yelp.com"
  yelpurl <- paste0(scheme, "://", host, path)
  
  # Make request.
  results <- GET(yelpurl, sig, query=query_args)
  
  # If status is not success, print some debugging output.
  HTTP_SUCCESS <- 200
  if (results$status != HTTP_SUCCESS) {
    print(results)
  }
  return(results)
}

yelp_search <- function(term, location = NULL, ll, limit=10) {
  # Search term and location go in the query string.
  path <- "/v2/search/"
  query_args <- list(term=term, location=location, ll=ll, limit=limit)
  
  # Make request.
  results <- yelp_query(path, query_args)
  return(results)
}

yelp_business <- function(business_id) {
  # Business ID goes in the path.
  path <- paste0("/v2/business/", business_id)
  query_args <- list()
  
  # Make request.
  results <- yelp_query(path, query_args)
  return(results)
}

print_search_results <- function(yelp_search_result) {
  print("=== Search Results ===")
  # Load data.  Flip it around to get an easy-to-handle list.
  locationdataContent = content(yelp_search_result)
  locationdataList=jsonlite::fromJSON(toJSON(locationdataContent))
  
  # Print output.
  print(head(data.frame(locationdataList)))
}

print_business_results <- function(yelp_business_result) {
  print("=== Business ===")
  print(content(yelp_business_result))
}

demo <- function() {
  # Query Yelp API, print results.
  yelp_search_result <- yelp_search(term="dinner", ll="43.63006,1.374128", limit=3)
  print_search_results(yelp_search_result)
  
  # Pick the top search result, get more info about it.
  # Find Yelp business ID, such as "giacomos-ristorante-boston".
  # business_id = content(yelp_search_result)$businesses[[1]]$id
  # yelp_business_result <- yelp_business(business_id)
  # print_business_results(yelp_business_result)
}

demo()
