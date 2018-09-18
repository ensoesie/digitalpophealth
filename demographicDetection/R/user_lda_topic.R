#' Assign topics to users
#'
#' Identify topic within Twitter profile text fields, and assign topics to individual users.
#' @param dat A data frame containing Twitter profile information. Created using the function get_user_info.
#' @param topicnum The number of LDA topics to identify
#' @param feature Select text feature to analyze. Options are "laststatus_text" and "description."
#' @param identifier Select feature for identifying users. Options are "id_str" and "screen_name."
#' @keywords text, Twitter
#' @return ldaout_topic A two-column data frame containing either screen names or ID values of Twitter users linked to an assigned topic.
#' @export
#' @seealso : The LDA analysis framework employed in this function is based on instructions from Kailash Awati of Sensanalytics. See: https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/

lda_topic_4_user<-function(dat, topicnum, feature, identifier){
  
  if(feature=="laststatus_text"){
    regex <- "(^|[^@\\w])@(\\w{1,15})\\b"
    
    dat$laststatus_text_clean<-unlist(lapply(dat$laststatus_text, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
    dat$laststatus_text_clean<-unlist(lapply(dat$laststatus_text_clean, function(x) gsub("<.*>", "", x)))
    dat$laststatus_text_clean<-gsub(regex, "", dat$laststatus_text_clean)
    dat$laststatus_text_clean<-gsub("RT: ", "", dat$laststatus_text_clean, ignore.case=FALSE)
    dat$laststatus_text_clean<-gsub("RT ", "", dat$laststatus_text_clean, ignore.case=FALSE)
    dat$laststatus_text_clean<-str_replace_all(dat$laststatus_text_clean, "http.*", "")
    dat$laststatus_text_clean<-str_replace_all(dat$laststatus_text_clean, "&amp", "")
    
    if(length(which(is.na(dat$laststatus_text_clean)))>0 | length(which(dat$laststatus_text==""))>0){
      dat_part<-dat[-which(is.na(dat$laststatus_text_clean) | dat$laststatus_text== ""),]
    }
    else{
      dat_part <- dat
    }
    
    files<-dat_part$laststatus_text_clean
    print("Text documents prepared!")
    
    if(identifier=="id_str"){
      filenames<-dat_part$id_str
    }
    
    if(identifier=="screen_name"){
      filenames<-dat_part$screen_name
    }
    print("Document names prepared!")
    
    docs <- Corpus(VectorSource(files))

    docs <-tm_map(docs,content_transformer(tolower))
    
    toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
    docs <- tm_map(docs, toSpace, "-")
    docs <- tm_map(docs, toSpace, "\"")
    
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removeWords, stopwords("en"))
    docs <- tm_map(docs, stripWhitespace)
    docs <- tm_map(docs,stemDocument)

    
    dtm <- DocumentTermMatrix(docs)
    rownames(dtm) <- filenames
    
    row_total = apply(dtm, 1, sum)
    dtm.new = dtm[row_total>0,]
    
    print("Document term matrix prepared!")
    
    burnin <- 4000
    iter <- 2000
    thin <- 500
    seed <-list(2003,5,63,100001,765)
    nstart <- 5
    best <- TRUE
    
    k <- topicnum
    
    ldaOut <-LDA(dtm.new, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
    print("LDA model is complete!")
    
    
    ldaOut.topics <- as.matrix(topics(ldaOut))
    
    ldaOut.topics<-as.data.frame(cbind(rownames(ldaOut.topics), ldaOut.topics[,1]), row.names=FALSE)
    
    if(identifier=="screen_name"){
      names(ldaOut.topics)<-c("screen_name", "topic")
    }
    
    if(identifier=="id_str"){
      names(ldaOut.topics)<-c("id_str", "topic")
      ldaOut.topics$id_str <- as.character(ldaOut.topics$id_str)
    }
    
    
    print("Results generated!")
    return(ldaOut.topics)
  }
  
  if(feature=="description"){
    regex <- "(^|[^@\\w])@(\\w{1,15})\\b"
    
    dat$description_clean<-unlist(lapply(dat$description, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
    dat$description_clean<-unlist(lapply(dat$description_clean, function(x) gsub("<.*>", "", x)))
    dat$description_clean<-gsub(regex, "", dat$description_clean)
    dat$description_clean<-gsub("RT: ", "", dat$description_clean, ignore.case=FALSE)
    dat$description_clean<-gsub("RT ", "", dat$description_clean, ignore.case=FALSE)
    dat$description_clean<-str_replace_all(dat$description_clean, "http.*", "")
    dat$description_clean<-str_replace_all(dat$description_clean, "&amp", "")
    
    if(length(which(is.na(dat$description_clean)))>0 | length(which(dat$description_clean==""))>0){
      dat_part<-dat[-which(is.na(dat$description_clean) | dat$description_clean == ""),]
    }
    else{
      dat_part <- dat
    }
    
    files<-dat_part$description_clean
    print("Text documents prepared!")
    
    if(identifier=="id_str"){
      filenames<-dat_part$id_str
    }
    
    if(identifier=="screen_name"){
      filenames<-dat_part$screen_name
    }
    
    
    print("Document names prepared!")
    
    docs <- Corpus(VectorSource(files))
    
    docs <-tm_map(docs,content_transformer(tolower))
    
    toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
    docs <- tm_map(docs, toSpace, "-")
    docs <- tm_map(docs, toSpace, "\"")
    
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removeWords, stopwords("en"))
    docs <- tm_map(docs, stripWhitespace)
    docs <- tm_map(docs,stemDocument)
    
    
    dtm <- DocumentTermMatrix(docs)
    rownames(dtm) <- filenames
    freq<-colSums(as.matrix(dtm)) 
    freq<-rowSums(as.matrix(dtm)) 
    
    row_total = apply(dtm, 1, sum)
    dtm.new = dtm[row_total>0,]
    
    print("Document term matrix prepared!")
    
    burnin <- 4000
    iter <- 2000
    thin <- 500
    seed <-list(2003,5,63,100001,765)
    nstart <- 5
    best <- TRUE
    
    k <- topicnum
    
    ldaOut <-LDA(dtm.new, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
    print("LDA model is complete!")
    
    
    ldaOut.topics <- as.matrix(topics(ldaOut))
    ldaOut.topics<-as.data.frame(cbind(rownames(ldaOut.topics), ldaOut.topics[,1]), row.names=FALSE)
    
    if(identifier=="screen_name"){
      names(ldaOut.topics)<-c("screen_name", "topic")
    }
    
    if(identifier=="id_str"){
      names(ldaOut.topics)<-c("id_str", "topic")
      ldaOut.topics$id_str <- as.character(ldaOut.topics$id_str)
    }
    
    print("Results generated!")
    return(ldaOut.topics)
  }
  
  
}

