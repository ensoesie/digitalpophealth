#' Clean twitter objects
#'
#' This is a general function that allows you to isolate the clean text of a tweet. It is built from functions found in stringr and regular expressions built by user kRazzy R on StackOverflow. See: https://stackoverflow.com/questions/31348453/how-do-i-clean-twitter-data-in-r. This removes retweets, references to screen names, hashtags, spaces, numbers, punctuations and URLs.
#' @param unclean_tweet Tweet text
#' @keywords twitter, text
#' @return Tweet cleaned of non-alphanumeric characters
#' @export
#' @examples 
#' clean_tweet_obj("Here is an example tweet! :D")


clean_tweet_obj<-function(unclean_tweet){
  clean_tweet = gsub("&amp", "", unclean_tweet)
  require(stringr)
  clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
  clean_tweet = gsub("@\\w+", "", clean_tweet)
  clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
  clean_tweet = gsub("http\\w+", "", clean_tweet)
  clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
  clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
  return(clean_tweet)
}