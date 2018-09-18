#' Prepare a corpus of tweets
#'
#' Prepare a vector of tweets for conversion to a document-term matrix or term-document matrix. First convert to ASCII text. Then stem, normalize, remove stopwords, remove punctuation, remove URLs, remove numbers and strip whitespace
#' @param vector A vector of tweets
#' @keywords text, Twitter
#' @return corp A clean corpus of tweet data, ready to be analyzed and converted to a document-term or term-document matrix.
#' @export


corp_convert<-function(vector){
  text_clean <- iconv(vector, to = "ASCII", sub=" ")
  text_clean <- gsub("&amp", "&", text_clean)
  text_clean <- stemDocument(text_clean, language="english")
  corp <- VCorpus(VectorSource(text_clean))
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, stemDocument, language = "english")  
  corp <- tm_map(corp, content_transformer(removeURL))
  corp <- tm_map(corp, str_replace_all,"[^[:alnum:]]", " ", mc.cores=1)
  corp <- tm_map(corp,removePunctuation)
  corp <- tm_map(corp, removeWords, stopwords("en"),mc.cores=1)
  corp <- tm_map(corp, removeNumbers, mc.cores=1)
  
  corp <- tm_map(corp,stripWhitespace)
  
  corp <- tm_map(corp, PlainTextDocument)
  return(corp)
}