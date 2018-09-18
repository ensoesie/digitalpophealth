#' Authenticate connection to the Twitter API: Part I
#'
#' This function allows users to authenticate a connection to the Twitter API. To establish an API connection, you will need a Twitter Developer's Account and an application. See https://apps.twitter.com/ for details.
#' @param api_key The Consumer Key value associated with your Twitter app
#' @param api_secret Consumer secret associated with your Twitter app
#' @keywords twitter 
#' @return twitCred An authentication token
#' @export



get_twitCred <- function(api_key, api_secret){
  reqURL <- "https://api.twitter.com/oauth/request_token"
  accessURL<- "https://api.twitter.com/oauth/access_token"
  authURL <- "https://api.twitter.com/oauth/authorize"
  
  
  twitCred <- ROAuth::OAuthFactory$new(consumerKey=api_key,
                               consumerSecret=api_secret,
                               requestURL=reqURL,
                               accessURL=accessURL,
                               authURL=authURL)
  
  cat("Now that you have generated your credential, type 'your_credential_name$handshake()' into your console and hit enter. \nFollow the resulting URL and enter the code it provides")
  
  return(twitCred)
}