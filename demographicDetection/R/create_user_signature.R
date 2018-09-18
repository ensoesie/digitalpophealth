#' Authenticate connection to the Twitter API: Part II
#' 
#' This function allows users to authenticate a connection to the Twitter API. To establish an API connection, you will need a Twitter Developer's Account and an application. See https://apps.twitter.com/ for details.
#' @param api_key The Consumer Key value associated with your Twitter app
#' @param api_secret Consumer secret associated with your Twitter app
#' @param acc_token Access token associated with your Twitter app
#' @param acc_token_secret Access token secret associated with your Twitter app
#' @param save A TRUE/FALSE indication of whether you want to auto-save your signature as an .RData file for later use
#' @param acc_token_secret If save==TRUE, your preferred directory
#' @keywords twitter 
#' @return user.signature A user signature that you can use and save
#' @export

create_user_signature <- function(api_key, api_secret, acc_token, acc_token_secret, save=FALSE, dir=NULL){
  setup_twitter_oauth(api_key, api_secret, access_token=acc_token, access_secret=acc_token_secret)
  
  twitter.app <- httr::oauth_app("twitter",key=api_key, 
                                 secret=api_secret)
  
  user.signature <- httr::sign_oauth1.0(twitter.app, 
                                        token = acc_token, 
                                        token_secret = acc_token_secret)
  
  if(save==TRUE){
    save(user.signature, file=paste0(dir, "user_sig_roauth.rdata"))
    return(user.signature)
  }
  if(save==FALSE){
    return(user.signature)
  }
}


