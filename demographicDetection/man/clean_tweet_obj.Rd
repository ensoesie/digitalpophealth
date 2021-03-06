% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_tweet_obj.R
\name{clean_tweet_obj}
\alias{clean_tweet_obj}
\title{Clean twitter objects}
\usage{
clean_tweet_obj(unclean_tweet)
}
\arguments{
\item{unclean_tweet}{Tweet text}
}
\value{
Tweet cleaned of non-alphanumeric characters
}
\description{
This is a general function that allows you to isolate the clean text of a tweet. It is built from functions found in stringr and regular expressions built by user kRazzy R on StackOverflow. See: https://stackoverflow.com/questions/31348453/how-do-i-clean-twitter-data-in-r. This removes retweets, references to screen names, hashtags, spaces, numbers, punctuations and URLs.
}
\examples{
clean_tweet_obj("Here is an example tweet! :D")
}
\keyword{text}
\keyword{twitter,}
