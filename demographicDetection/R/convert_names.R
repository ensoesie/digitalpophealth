#' Clean user names
#'
#' Clean and extract first names from user names
#' @param vect A character vector containing a Twitter user name
#' @keywords text, Twitter
#' @return name_new a cleaned first name from a user name
#' @export
#' @examples 
#' convert_names("Miss. Jessica is the greatest!")

convert_names<-function(name){
  # list of suffixes
  suffix<-c("mr ", "mr_", "mr-",
            "mrs ", "mrs_", "mrs-",
            "ms ", "ms_", "ms-",
            "miss.", "miss ", "miss_", "miss-",
            "mx ", "mx_", "mx-",
            "sir ", "sir_", "sir-",
            "gentleman ", "gentleman_", "gentleman-",
            "sir ", "sire_", "sire-",
            "mistress ", "mistress_", "mistress-",
            "madame ", "madame_", "madame-",
            "dame ", "dame_", "dame-",
            "lord ", "lord_", "lord-",
            "lady ", "lady_", "lady-",
            "esq ", "esq_", "esq-",
            "dr ", "dr_", "dr-",
            "professor ", "professor_", "professor-",
            "prof ", "prof_", "prof-",
            "reverend ", "reverend_", "reverend-",
            "reverend ", "reverend_", "reverend-",
            "reverend ", "reverend_", "reverend-",
            "rev ", "rev_", "rev-",
            "fr ", "fr_", "fr-",
            "pr ", "pr_", "pr-",
            "br ", "br_", "br-",
            "sr ", "sr_", "sr-",
            "jr ", "jr_", "jr-",
            " ma", " jd", "phd", "dr "," jd", "m.d.", " do", " dc", "pharm d", "mfa", "lil")
  
  
  
  ## add suffixes to other stopwords (alowibdi et al. 2013)
  stopwords_name<-c(stopwords("en"), suffix)
  stopwords_name<-gsub(" ", "", stopwords_name)
  
  
  name_new<-name
  if(Encoding(name_new)=="UTF-8"){
    name_new<-iconv(name_new, "UTF-8", "ASCII", sub="") # remove UTF-8 characters without destroying the entire name
  }
  name_new<-tolower(stripWhitespace(as.character(name_new))) #normalize, remove whitespace
  name_new<-sub("0+", "", name_new)  # remove numbers
  name_new<-sub("1+", "", name_new)
  name_new<-sub("2+", "", name_new)
  name_new<-sub("3+", "", name_new)
  name_new<-sub("4+", "", name_new)
  name_new<-sub("5+", "", name_new)
  name_new<-sub("6+", "", name_new)
  name_new<-sub("7+", "", name_new)
  name_new<-sub("8+", "", name_new)
  name_new<-sub("9+", "", name_new)
  name_new<-gsub("&amp", "", name_new)
  name_new<-gsub("[[:punct:]]", "", name_new)
  name_new<-new_name<-gsub("[ \t]{2,}", "", name_new)
  name_new<-gsub("^\\s+|\\s+$", "", name_new) 
  name_new<-gsub("[<].*[>]", "", name_new)
  name_new<-removePunctuation(name_new) # remove punctuation
  if(length(unlist(strsplit(name_new, " ")))>1){ # if there are two+ strings one may be a first name
    name_new<-strsplit(name_new, " ")[[1]][-length(unlist(strsplit(name_new, " ")))] # remove the last string, that probably isn't the first name
    name_new<-unlist(name_new)[!(unlist(name_new) %in% stopwords_name)]  ## get rid of any stopwords/prefixes
    name_new<-name_new[1]
  }
  if(length(name_new)==0){
    name_new<-NA
  }
  return(name_new)
}