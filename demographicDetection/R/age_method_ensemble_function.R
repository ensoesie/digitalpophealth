#' Predict age using Twitter profile data
#'
#' This function inputs Twitter profile metadata from get_user_info, generates features from the profile content and last posted status, and uses a gradient boosted decision tree to identify whether users are Below 18, 18-30 or 30 and older.
#' @param input A data frame containing Twitter profile information. Created using the function get_user_info.
#' @param ncores Number of cores across which the process is distributed. Note: Windows users cannot request more than two.
#' @param chunk If TRUE, features and predictions are generated for 10,000 user chunks. Recommended for unlabeled data with >10,000 users.
#' @param action If chunk==TRUE, whether the data should be appended and returned as one file, or saved as chunks. Options are append and save.
#' @param dir Where should output files be saved? Used only if action==save. Defaults to NULL
#' @keywords twitter, gender, demography
#' @return results A two-column data frame containing Twitter user IDs and age predictions.
#' @export
#' @examples
#' ## df_list from function get_user_info
#' ## df <- do.call("rbind", df_list)
#' gender_ensemble(df, labeled=FALSE, set.seed=NULL, training_set=0.80, chunk=TRUE, action="save", dir="/home/User/workingDir")


age_ensemble <- function(input, labeled=FALSE, set.seed=NULL, training_set=0.80, chunk=FALSE, action="append", dir=NULL){

  slang_terms <- trim(tolower(c("ama","dafuq","dm","ELI5","FML","FTFY","headdesk","HIFW","ICYMI","IDGAF","IMO","IMHO","IRL","JSYK","lol","lul","lulz","MFW","MRW","MIRL","NSFW","NSFL","PAW","QFT","TBT","TIL","TL;DR","YMMV","YOLO","MHOTY","LMAO","BRB","IKR","IDEK","IDECA","POS","NGL","OB","LIF","MLIS","WYD","HBU","ILY","ROFL","AF","WCW","MCM","FBF","TCSS","IDK","TBH")))

  if(!is.null(dir) & tail(unlist(strsplit(dir, "")), 1) !="/"){
    dir <- paste0(dir, "/")
  }

  if(is.null(input$name)){
    stop("Your data is missing relevant field(s)")
  }

  if(labeled == FALSE){

  #load(paste0(workingDir, "ensemble_methods/R_package/modelObjects/age_ensemble_mod.RData"))
  #load(paste0(workingDir, "ensemble_methods/R_package/modelObjects/age_ensemble_mod_bestIter.RData"))

  print("Classifier is trained!")

      if(chunk == FALSE){

        test<-input

        if(length(which(is.na(test$screen_name)))>0){
          test <- test[-which(is.na(test$screen_name)),]
        }

        ## Modifying activity counts
        test$statuses_count_log<-log(test$statuses_count)
        test$statuses_count_log[is.infinite(test$statuses_count_log)]<-0

        test$statuses_quantile<-test$statuses_count
        test$statuses_quantile[test$statuses_count>=0 & test$statuses_count<157]<-1
        test$statuses_quantile[test$statuses_count>=157 & test$statuses_count<1897]<-2
        test$statuses_quantile[test$statuses_count>=1897 & test$statuses_count<11632]<-3
        test$statuses_quantile[test$statuses_count>=11632 & test$statuses_count<=686052]<-4

        test$followers_count_log<-log(test$statuses_count)
        test$followers_count_log[is.infinite(test$followers_count_log)]<-0

        test$followers_quantile<-test$followers_count
        test$followers_quantile[test$followers_count>=0 & test$followers_count<29]<-1
        test$followers_quantile[test$followers_count>=29 & test$followers_count<159]<-2
        test$followers_quantile[test$followers_count>=159 & test$followers_count<557]<-3
        test$followers_quantile[test$followers_count>=557 & test$followers_count<=208437]<-4

        test$friends_count_log<-log(test$friends_count)
        test$friends_count_log[is.infinite(test$friends_count_log)]<-0

        test$friends_quantile<-test$friends_count
        test$friends_quantile[test$friends_count>=0 & test$friends_count<81]<-1
        test$friends_quantile[test$friends_count>=81 & test$friends_count<256]<-2
        test$friends_quantile[test$friends_count>=256 & test$friends_count<680]<-3
        test$friends_quantile[test$friends_count>=680 & test$friends_count<=101531]<-4

        test$favourites_count_log<-log(test$favourites_count)
        test$favourites_count_log[is.infinite(test$favourites_count_log)]<-0


        test$favourites_quantile<-test$favourites_count
        test$favourites_quantile[test$favourites_count>=0 & test$favourites_count<43]<-1
        test$favourites_quantile[test$favourites_count>=43 & test$favourites_count<807]<-2
        test$favourites_quantile[test$favourites_count>=807 & test$favourites_count<5775]<-3
        test$favourites_quantile[test$favourites_count>=5775 & test$favourites_count<=330554]<-4


        test$listed_count_log<-log(test$listed_count)
        test$listed_count_log[is.infinite(test$listed_count_log)]<-0

        #### Ratio of friends to followers (from PLOS paper)
        test$friend_follower<-test$friends_count/test$followers_count
        test$friend_follower[is.na(test$friend_follower)]<-0
        test$friend_follower[is.infinite(test$friend_follower)]<-test$friends_count[is.infinite(test$friend_follower)]


        #### Does the user share a url?
        test$url_binary<-test$url
        test$url_binary[is.na(test$url)]<-0
        test$url_binary[!is.na(test$url)]<-1


        #### Does the URL mention social media? (missing values marked as NA)
        test$url_expanded_sm_na<-NA

        for(i in 1:dim(test)[1]){
          if(!is.na(test$description[i])){
            test$url_expanded_sm_na[i]<-0
          }
        }
        test$url_expanded_sm_na[grep("insta", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm_na[grep("snap", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm_na[grep("youtube", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm_na[grep("facebook", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm_na[grep("linkedin", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm_na[grep("tumblr", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm_na[grep("wordpress", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm_na[grep("pinterest", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm_na[grep("curiouscat", test$url_expanded, ignore.case=TRUE)]<-1

        #### Does the URL mention social media? (missing values marked as 0)
        test$url_expanded_sm<-0
        test$url_expanded_sm[grep("insta", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm[grep("snap", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm[grep("youtube", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm[grep("facebook", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm[grep("linkedin", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm[grep("tumblr", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm[grep("wordpress", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm[grep("pinterest", test$url_expanded, ignore.case=TRUE)]<-1
        test$url_expanded_sm[grep("curiouscat", test$url_expanded, ignore.case=TRUE)]<-1


        #### Does the user provide a location?
        test$location_binary<-test$location
        test$location_binary[is.na(test$location)]<-0
        test$location_binary[!is.na(test$location)]<-1

        #### Does the user provide a banner URL?
        test$profile_banner_url_binary<-test$profile_banner_url
        test$profile_banner_url_binary[is.na(test$profile_banner_url)]<-0
        test$profile_banner_url_binary[!is.na(test$profile_banner_url)]<-1

        #### Does the user have a default profile image?
        test$profile_image_default<-0
        test$profile_image_default[grep("default", test$profile_image_url)]<-1

        #### Does the user use template profile colors?

        test$template_color<-0
        test$template_color[which(test$profile_link_color=="981CEB" |
                                       test$profile_link_color=="F58EA8" |
                                       test$profile_link_color=="E81C4F" |
                                       test$profile_link_color=="1B95E0" |
                                       test$profile_link_color=="91D2FA" |
                                       test$profile_link_color=="19CF86" |
                                       test$profile_link_color=="7FDBB6" |
                                       test$profile_link_color=="FAB81E" |
                                       test$profile_link_color=="FF691F" |
                                       test$profile_link_color=="ABB8C2")] <-1



        #### How similar are the user's name and screen name?
        test$name_clean<-iconv(test$name, "UTF-8", "ASCII", sub="") # remove UTF-8 characters without destroying the entire name
        test$name_clean<-removePunctuation(test$name_clean) # remove punctuation
        test$name_clean<-tolower(stripWhitespace(as.character(test$name_clean))) #normalize, remove whitespace
        test$name_clean<-unlist(lapply(test$name_clean, function(x) gsub(" ", "", x)))

        test$screen_name_clean<-tolower(test$screen_name)

        test$similarity<-NA
        for(i in 1:dim(test)[1]){
          test$similarity[i]<-levenshteinSim(test$name_clean[i], test$screen_name_clean[i])
        }


        #### How many months has the user been active? How often do they tweet?
        test$days_active <- NA
        test$months_active <- NA
        test$avg_twt_per_mon <- NA
        test$year_created <- NA

        for (i in 1:dim(test)[1]){
          if(!is.na(test$laststatus_created_at[i])){
            test$year_created[i] <- tail(unlist(strsplit(test$created_at[i], " ")), 1)
            test$days_active[i]<-as.numeric(as.Date(dateConvert(test$laststatus_created_at[i]))-as.Date(dateConvert(test$created_at[i])))
            test$months_active[i]<-as.numeric(as.Date(dateConvert(test$laststatus_created_at[i]))-as.Date(dateConvert(test$created_at[i])))/30
            test$avg_twt_per_mon[i]<-as.numeric(test$statuses_count[i])/test$months_active[i]
          }
          if(i %% 100 == 0){
            print(i)
          }
        }


        test$avg_twt_per_mon[is.infinite(test$avg_twt_per_mon)]<-0
        test$avg_twt_per_mon[is.na(test$avg_twt_per_mon)]<-0


        test$avg_twt_per_mon_quant<-test$avg_twt_per_mon
        test$avg_twt_per_mon_quant[test$avg_twt_per_mon<15]<-1
        test$avg_twt_per_mon_quant[test$avg_twt_per_mon>=15 & test$avg_twt_per_mon<104]<-2
        test$avg_twt_per_mon_quant[test$avg_twt_per_mon>=104 & test$avg_twt_per_mon<388]<-3
        test$avg_twt_per_mon_quant[test$avg_twt_per_mon>=388]<-4


        test$avg_twt_per_mon_log<-log(test$avg_twt_per_mon)
        test$avg_twt_per_mon_log[is.infinite(test$avg_twt_per_mon_log)]<-0


        #### Favourites count, normalized for lifespan of the account
        test$favourites_normalized<-test$favourites_count/test$days_active

        #### Source: what is the user tweeting from?
        test$device<-unlist(lapply(test$laststatus_source, function(x) gsub("</a", "", unlist(strsplit(x, ">"))[2])))

        test$device[-which(test$device=="tweetDeck" |
                                test$device=="SocialToaster" |
                                test$device=="Lucktastic Android" |
                                test$device=="Facebook" |
                                test$device=="Instagram" |
                                test$device=="Twitter for iPad" |
                                test$device=="Twitter Lite" |
                                test$device=="Google" |
                                test$device=="Twitter Web Client" |
                                test$device=="Twitter for iPhone" |
                                test$device=="Twitter for Android") ] <-"Other"


        #### Source: Does the user tweet from an iPhone?
        test$iPhone<-0
        test$iPhone[which(test$device=="Twitter for iPhone")]<-1


        #### Does the profile include a background image?
        test$profile_background_image_url_binary<-test$profile_background_image_url
        test$profile_background_image_url_binary[is.na(test$profile_background_image_url)]<-0
        test$profile_background_image_url_binary[!is.na(test$profile_background_image_url)]<-1


        #### How many words are in the user's description?
        test$description_length_word<-unlist(lapply(test$description, function(x) length(unlist(strsplit(x, " ")))))


        #### How many characters are in the user's description?
        test$description_length_char<-unlist(lapply(test$description, function(x) length(unlist(strsplit(x, "")))))


        #### Presence of emojis in description
        #test$description_decode<-unlist(lapply(test$description, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode

        test$description_emoji <- NA
        test$name_emoji <- NA

        #for(i in 1:dim(test)[1]){
        #  test<-unlist(strsplit(test$description_decode[i], ""))
        #  test<-test[-grep(" ", test)]
        #  all<-length(test)
        #  alphanumeric<-length(grep("[[:digit:]]", test))+length(grep("[[:alpha:]]", test))
        #  test$description_emoji[i]<-(all-alphanumeric)/all
        #  print(i)
        #}

        for(i in 1:dim(test)[1]){
          vec <- unlist(strsplit(test$description[i], " "))
          vec <- vec[-which(vec=="")]
          sum1 <- which(unlist(lapply(vec, function(x) isUpperMethod1(trim(gsub('[[:digit:]]+', '', x)))))==TRUE) # unicode caps
          sum2 <- grep("<", vec)
          sum3 <- grep("[^[:alnum:]]", unlist(strsplit(vec, " ")))
          sum4 <- grep("#", unlist(strsplit(vec, " ")))
          sum5 <- grep("@", unlist(strsplit(vec, " ")))

          vec1 <- unique(c(sum1, sum2, sum3))
          vec2 <- unique(c(sum4, sum5))

          sum <- length(setdiff(vec1, vec2))
          test$description_emoji[i] <- sum
          if(i %% 100 == 0){
            print(i)
          }
        }


        #### Does the user's name include emojis
        for(i in 1:dim(test)[1]){
          vec <- unlist(strsplit(test$name[i], " "))
          vec <- vec[-which(vec=="")]
          sum1 <- which(unlist(lapply(vec, function(x) isUpperMethod1(trim(gsub('[[:digit:]]+', '', x)))))==TRUE) # unicode caps
          sum2 <- grep("<", vec)
          sum3 <- grep("[^[:alnum:]]", unlist(strsplit(vec, " ")))
          sum4 <- grep("#", unlist(strsplit(vec, " ")))
          sum5 <- grep("@", unlist(strsplit(vec, " ")))

          vec1 <- unique(c(sum1, sum2, sum3))
          vec2 <- unique(c(sum4, sum5))

          sum <- length(setdiff(vec1, vec2))
          test$name_emoji[i] <- sum
          if(i %% 100 == 0){
            print(i)
          }
        }


        #### Does the description include personal pronouns?
        test$description_pronouns<-0

        for(i in 1:dim(test)[1]){
          if(length(grep("i", unlist(strsplit(test$description[i], " ")), ignore.case=TRUE))>1){
            test$description_pronouns[i]<-1
          }
          if(length(grep("me", unlist(strsplit(test$description[i], " ")), ignore.case=TRUE))>1){
            test$description_pronouns[i]<-1
          }
          if(length(grep("my", unlist(strsplit(test$description[i], " ")), ignore.case=TRUE))>1){
            test$description_pronouns[i]<-1
          }
        }


        #### How many hashtags are in the description?
        test$description_hashtag_count<-NA

        for(i in 1:dim(test)[1]){
          if(!is.na(test$description[i])){
            test$description_hashtag_count[i]<-length(grep("#", unlist(strsplit(test$description[i], ""))))
          }
        }

        test$description_hashtag_binary<-test$description_hashtag_count
        test$description_hashtag_binary[test$description_hashtag_count>=1]<-1
        test$description_hashtag_binary[test$description_hashtag_count<1 & !is.na(test$description_hashtag_count)]<-0


        #### How many "@" are in the description?
        test$description_at_count <- unlist(lapply(test$description, function(x) length(grep("@", unlist(strsplit(x, ""))))))

        test$description_at_binary<-test$description_at_count
        test$description_at_binary[test$description_at_count>=1]<-1
        test$description_at_binary[test$description_at_count<1 & !is.na(test$description_at_count)]<-0


        #### Does the user mention of other links in description?
        test$description_web_count <- unlist(lapply(test$description, function(x) length(grep("http", x))))

        #### Does the description mention social media?
        test$description_sm<-0
        test$description_sm[grep("insta", test$description, ignore.case=TRUE)]<-1
        test$description_sm[grep("snap", test$description, ignore.case=TRUE)]<-1
        test$description_sm[grep("youtube", test$description, ignore.case=TRUE)]<-1
        test$description_sm[grep("facebook", test$description, ignore.case=TRUE)]<-1
        test$description_sm[grep("linkedin", test$description, ignore.case=TRUE)]<-1
        test$description_sm[grep("tumblr", test$description, ignore.case=TRUE)]<-1
        test$description_sm[grep("wordpress", test$description, ignore.case=TRUE)]<-1
        test$description_sm[grep("pinterest", test$description, ignore.case=TRUE)]<-1
        test$description_sm[grep("curiouscat", test$description, ignore.case=TRUE)]<-1


        #### Mention of mail in description
        test$description_mail_count <- unlist(lapply(test$description[i], function(x) length(grep("@.*.com", x))))

        #### Mention of pipe in description
        test$description_pipe_count <- length(grep("|", unlist(strsplit(test$description[i], "")), fixed=TRUE))


        test$description_pipe_binary<-test$description_pipe_count
        test$description_pipe_binary[test$description_pipe_count>=1]<-1
        test$description_pipe_binary[test$description_pipe_count<1 & !is.na(test$description_pipe_count)]<-0


        #### Mention of height in description
        test$description_height_count <- unlist(lapply(test$description, function(x) length(grep("[:digit:]'[:digit:]", x))))


        #### Does the description contain stopwords?
        test$description_stopword_count<-unlist(lapply(test$description, function(x) length(intersect(unlist(strsplit(x, " ")), stopwords("en")))))

        test$description_stopword_count_normalized<-NA
        for(i in 1:dim(test)[1]){
          test$description_stopword_count_normalized[i]<-test$description_stopword_count[i]/length(unlist(strsplit(test$description[i], " ")))
        }


        #### Does the last status include media?
        test$laststatus_media_binary<-test$laststatus_media
        test$laststatus_media_binary[which(!is.na(test$laststatus_text) & is.na(test$laststatus_media))]<-0
        test$laststatus_media_binary[which(!is.na(test$laststatus_media))]<-1


        #### Does the last status include a URL?
        test$laststatus_url_binary<-test$laststatus_urls
        test$laststatus_url_binary[which(!is.na(test$laststatus_text) & is.na(test$laststatus_urls))]<-0
        test$laststatus_url_binary[which(!is.na(test$laststatus_urls))]<-1


        #### Prepare laststatus for feature building
        test$laststatus_text_clean<-trim(test$laststatus_text)
        test$laststatus_text_clean<-unlist(lapply(test$laststatus_text_clean, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
        test$laststatus_text_clean<-unlist(lapply(test$laststatus_text_clean, function(x) gsub("RT ", "", x))) #these two lines remove unicode

        test$laststatus_text_clean <-as.character(test$laststatus_text_clean)

        test$description<-trim(test$description)
        test$description<-unlist(lapply(test$description, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
        test$description<-unlist(lapply(test$description, function(x) gsub("RT ", "", x)))

        test$description<-as.character(test$description)

        #### Number of characters in laststatus
        test$laststatus_length <- unlist(lapply(test$laststatus_text_clean, function(x) nchar(x)))


        #### Number of characters in description
        test$description_length <- unlist(lapply(test$description, function(x) nchar(x)))


        test$description_length[is.na(test$description_length)]<-0

        #### Count upper case letters
        test$laststatus_upper<-sapply(regmatches(test$laststatus_text, gregexpr("[A-Z]", test$laststatus_text, perl=TRUE)), length)
        test$description_upper<-sapply(regmatches(test$description, gregexpr("[A-Z]", test$description, perl=TRUE)), length)


        #### Count all digits
        test$laststatus_digit<-sapply(regmatches(test$laststatus_text_clean, gregexpr("[0-9]", test$laststatus_text_clean, perl=TRUE)), length)
        test$description_digit<-sapply(regmatches(test$description, gregexpr("[0-9]", test$description, perl=TRUE)), length)


        #### Count spaces
        test$laststatus_space<-unlist(lapply(test$laststatus_text_clean, function(x) countSpaces(x)))
        test$description_space<-unlist(lapply(test$description, function(x) countSpaces(x)))


        #### Standard deviation of word length
        test$laststatus_avg_word_length<-NA
        test$laststatus_sd_word_length<-NA
        test$laststatus_min_word_length<-NA
        test$laststatus_max_word_length<-NA


        for(i in 1:length(test$laststatus_text_clean)){
          if(!is.na(test$laststatus_text_clean[i]) & test$laststatus_text_clean[i]!=""){
            vec<-gsub('[[:punct:] ]+',' ', test$laststatus_text_clean[i])
            vec<-unlist(strsplit(vec, " "))
            if(length(grep("RT", vec, ignore.case=FALSE))>=1){
              vec<-vec[-grep("RT", vec, ignore.case=FALSE)]
            }
            if(length(grep("@.*", vec, ignore.case=FALSE))>=1){
              vec<-vec[-grep("@.*", vec, ignore.case=FALSE)]
            }
            vals<-NULL
            for(j in 1:length(vec)){
              val<-nchar(vec[j])
              vals<-c(vals, val)
            }
            test$laststatus_avg_word_length[i]<-mean(vals, na.rm=TRUE)
            test$laststatus_sd_word_length[i]<-sd(vals, na.rm=TRUE)
            test$laststatus_min_word_length[i]<-min(vals, na.rm=TRUE)
            test$laststatus_max_word_length[i]<-max(vals, na.rm=TRUE)
          }
          if(i %% 100 == 0){
            print(i)
          }
        }


        test$description_avg_word_length<-NA
        test$description_sd_word_length<-NA
        test$description_min_word_length<-NA
        test$description_max_word_length<-NA


        for(i in 1:length(test$description)){
          if(!is.na(test$description[i]) & test$description[i]!=""){
            vec<-gsub('[[:punct:] ]+',' ', test$description[i])
            vec<-unlist(strsplit(vec, " "))
            if(length(grep("RT", vec, ignore.case=FALSE))>=1){
              vec<-vec[-grep("RT", vec, ignore.case=FALSE)]
            }
            if(length(grep("@.*", vec, ignore.case=FALSE))>=1){
              vec<-vec[-grep("@.*", vec, ignore.case=FALSE)]
            }
            vals<-NULL
            for(j in 1:length(vec)){
              val<-nchar(vec[j])
              vals<-c(vals, val)
            }
            test$description_avg_word_length[i]<-mean(vals, na.rm=TRUE)
            test$description_sd_word_length[i]<-sd(vals, na.rm=TRUE)
            test$description_min_word_length[i]<-min(vals, na.rm=TRUE)
            test$description_max_word_length[i]<-max(vals, na.rm=TRUE)
          }
          if(i %% 100 == 0){
            print(i)
          }
        }

        ## Slang terms in the laststatus
        test$laststatus_slang<-0
        for(i in 1:length(slang_terms)){
          idx<-grep(slang_terms[i], test$laststatus_text, ignore.case=TRUE)
          test$laststatus_slang[idx]<-1
        }

        test$laststatus_slang[which(is.na(test$laststatus_text_clean))]<-NA


        ## Punctuation
        test$laststatus_punct <- NA

        for(i in 1:length(test$laststatus_text)){
          vec<-unlist(strsplit(test$laststatus_text[i], ""))
          val1<-which(vec == ".")
          val2<-which(vec == ",")
          val3<-which(vec == "!")
          val4<-which(vec == "?")
          val5<-which(vec == ":")
          val6<-which(vec == ";")
          val7<-which(vec == "'")
          val7<-which(vec == "\"")
          len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7)))
          test$laststatus_punct[i]<-len
          if(i %% 100 == 0){
            print(i)
          }
        }

        test$description_punct <- NA

        for(i in 1:length(test$description)){
          vec<-unlist(strsplit(test$description[i], ""))
          val1<-which(vec == ".")
          val2<-which(vec == ",")
          val3<-which(vec == "!")
          val4<-which(vec == "?")
          val5<-which(vec == ":")
          val6<-which(vec == ";")
          val7<-which(vec == "'")
          val7<-which(vec == "\"")
          len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7)))
          test$description_punct[i]<-len
          if(i %% 100 == 0){
            print(i)
          }
        }



        ## number of special characters
        test$laststatus_char <- NA

        for(i in 1:length(test$laststatus_text_clean)){
          vec<-unlist(strsplit(test$laststatus_text_clean[i], ""))
          val1<-which(vec == "@")
          val2<-which(vec == "#")
          val3<-which(vec == "$")
          val4<-which(vec == "%")
          val5<-which(vec == "&")
          val6<-which(vec == "*")
          val7<-which(vec == "~")
          val8<-which(vec == "^")
          val9<-which(vec == "-")
          val10<-which(vec == "=")
          val11<-which(vec == "+")
          val12<-which(vec == ">")
          val13<-which(vec == "<")
          val14<-which(vec == "[")
          val15<-which(vec == "]")
          val16<-which(vec == "{")
          val17<-which(vec == "}")
          val18<-which(vec == "|")
          val19<-which(vec == "\\")
          val20<-which(vec == "/")
          len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7,val8,val9,val10,val11,val12,val13,val14,val15,val16,val17,val18,val19,val20)))
          test$laststatus_char[i]<-len
          if(i %% 100 == 0){
            print(i)
          }
        }

        test$description_char <- NA

        for(i in 1:length(test$description)){
          vec<-unlist(strsplit(test$description[i], ""))
          val1<-which(vec == "@")
          val2<-which(vec == "#")
          val3<-which(vec == "$")
          val4<-which(vec == "%")
          val5<-which(vec == "&")
          val6<-which(vec == "*")
          val7<-which(vec == "~")
          val8<-which(vec == "^")
          val9<-which(vec == "-")
          val10<-which(vec == "=")
          val11<-which(vec == "+")
          val12<-which(vec == ">")
          val13<-which(vec == "<")
          val14<-which(vec == "[")
          val15<-which(vec == "]")
          val16<-which(vec == "{")
          val17<-which(vec == "}")
          val18<-which(vec == "|")
          val19<-which(vec == "\\")
          val20<-which(vec == "/")
          len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7,val8,val9,val10,val11,val12,val13,val14,val15,val16,val17,val18,val19,val20)))
          test$description_char[i]<-len
          if(i %% 100 == 0){
            print(i)
          }
        }


        #### length of description
        test$description_length<-unlist(lapply(test$description, function(x) length(unlist(strsplit(x, " ")))))


        #### Number of emoticons
        test$laststatus_emoticon_count<-0
        test$laststatus_emoticon_count<-unlist(lapply(test$laststatus_text_clean, function(x) find_emoticon(x)))

        test$description_emoticon_count<-0
        test$description_emoticon_count<-unlist(lapply(test$description, function(x) find_emoticon(x)))


        #### Use of profanity
        test$laststatus_profane<-NA

        for (i in 1:length(test$laststatus_text_clean)){
          if(!is.na(test$laststatus_text_clean[i])){
            vec<-unlist(strsplit(test$laststatus_text_clean[i], " "))
            val1<-grep("shit", vec, ignore.case=TRUE)
            val2<-grep("fuck", vec, ignore.case=TRUE)
            val3<-grep("ass", vec, ignore.case=TRUE)
            val4<-grep("piss", vec, ignore.case=TRUE)
            val5<-grep("bitch", vec, ignore.case=TRUE)
            val6<-grep("damn", vec, ignore.case=TRUE)
            test$laststatus_profane[i]<-length(unique(c(val1, val2, val3, val4, val5, val6)))
          }
        }


        test$description_profane<-NA

        for (i in 1:length(test$description)){
          if(!is.na(test$description[i])){
            vec<-unlist(strsplit(test$description[i], " "))
            val1<-grep("shit", vec, ignore.case=TRUE)
            val2<-grep("fuck", vec, ignore.case=TRUE)
            val3<-grep("ass", vec, ignore.case=TRUE)
            val4<-grep("piss", vec, ignore.case=TRUE)
            val5<-grep("bitch", vec, ignore.case=TRUE)
            val6<-grep("damn", vec, ignore.case=TRUE)
            test$description_profane[i]<-length(unique(c(val1, val2, val3, val4, val5, val6)))
          }
        }


        #### Vocabularly richness (number of unique words per tweet - minus stop words)
        test$laststatus_vocab_richness<-NA

        for (i in 1:length(test$laststatus_text_clean)){
          if(!is.na(test$laststatus_text_clean[i])){
            newVec<-removeWords(test$laststatus_text_clean[i], stopwords("en"))
            newVec<-removePunctuation(newVec)
            newVec<-trim(newVec)
            len1<-length(unlist(strsplit(newVec, " ")))
            len2<-length(unique(unlist(strsplit(newVec, " "))))
            test$laststatus_vocab_richness[i]<-len2/len1
          }
        }

        test$description_vocab_richness<-NA

        for (i in 1:length(test$description)){
          if(!is.na(test$description[i])){
            newVec<-removeWords(test$description[i], stopwords("en"))
            newVec<-removePunctuation(newVec)
            newVec<-trim(newVec)
            len1<-length(unlist(strsplit(newVec, " ")))
            len2<-length(unique(unlist(strsplit(newVec, " "))))
            test$description_vocab_richness[i]<-len2/len1
          }
        }

        #### User retweets someone else
        test$laststatus_retweet<-0
        test$laststatus_retweet[grep("RT ", test$laststatus_text, ignore.case=FALSE)]<-1

        #### User tweets at someone else
        test$laststatus_at<-0
        test$laststatus_at[grep("(^|[^@\\w])@(\\w{1,15})\\b", test$laststatus_text, ignore.case=FALSE)]<-1
        test$laststatus_at[grep("RT ", test$laststatus_text, ignore.case=FALSE)]<-0

        ## Laststatus retweet count
        ## use as is

        ## Laststatus favorite
        ## use as is

        #### Laststatus media binary
        test$laststatus_media_binary<-0
        test$laststatus_media_binary[!is.na(test$laststatus_media)]<-1

        #### Laststatus geo binary
        test$laststatus_geo_binary<-0
        test$laststatus_geo_binary[!is.na(test$laststatus_geo)]<-1

        #### Laststatus term repetition (i.e. okaaaaaaay)
        test$laststatus_repeated<-NA

        for(i in 1:dim(test)[1]){
          if(!is.na(test$laststatus_text[i])){
            test$laststatus_repeated[i]<-nchar(test$laststatus_text_clean[i])-nchar(gsub('([[:alpha:]])\\1+', '\\1', test$laststatus_text_clean[i]))
          }
          if(i %% 100 == 0){
            print(i)
          }
        }


        ### Description term repetition
        test$description_repeated<-NA

        for(i in 1:dim(test)[1]){
          if(!is.na(test$description[i])){
            test$description_repeated[i]<-nchar(test$description[i])-nchar(gsub('([[:alpha:]])\\1+', '\\1', test$description[i]))
          }
          if(i %% 100 == 0){
            print(i)
          }
        }


        ## Positive or negative content using senti-word
        print("Estimating sentiment of last posted status...this may take a moment")
        test$laststatus_sentiment<-unlist(mclapply(test$laststatus_text_clean, function(x) mean(sentiment(x)$sentiment), mc.cores=10L))

        print("Estimating sentiment of profile description...this may take a moment")
        test$description_sentiment<-unlist(mclapply(test$description, function(x) mean(sentiment(x)$sentiment), mc.cores=10L))

        ## Assign topics to users
        test$id_str <- as.character(test$id_str)
        print("Assigning topics to last posted status...this may take a moment")
        test$screen_name<-test$screen_name
        test$laststatus_text<-test$laststatus_text

        results<-lda_topic_4_user(test, 5, "laststatus_text", "id_str")
        names(results)[2]<-"laststatus_topic"

        colName <- names(results)[1]

        test<-merge(test, results, by=colName, all=TRUE)

        ## Assign topics to description field
        test$id_str <- as.character(test$id_str)
        print("Assigning topics to last posted status...this may take a moment")
        results<-lda_topic_4_user(test, 5, "description", "id_str")
        names(results)[2]<-"description_topic"

        colName <- names(results)[1]

        test<-merge(test, results, by=colName, all=TRUE)

        test$year_created <- as.factor(test$year_created)
        test$geo_enabled <- as.factor(test$geo_enabled)
        test$profile_banner_url_binary <- as.factor(test$profile_banner_url_binary)
        test$description_hashtag_binary <- as.factor(test$description_hashtag_binary)
        test$profile_background_image_url_binary <- as.factor(test$profile_background_image_url_binary)
        test$location_binary <- as.factor(test$location_binary)
        #return(test)

        print("Predicting values")
        model_predict_probs <- predict.gbm(model, test, best.iter, type="response")
        model_predict <- apply(model_predict_probs[,,1], 1, which.max)
        model_predict[model_predict==1]<-"18 to 30"
        model_predict[model_predict==2]<-"30 or older"
        model_predict[model_predict==3]<-"Below 18"


        print("predictions are done!")
        results<-as.data.frame(cbind(as.character(test$id_str), model_predict))
        names(results)<-c("id_str", "age_prediction")
        return(results)
      }

      if(chunk == TRUE & action == "append"){

        test<-input

        if(length(which(is.na(test$screen_name)))>0){
          test <- test[-which(is.na(test$screen_name)),]
        }

        vec <- seq(1, nrow(test), by=10000)

        results <- NULL
        iter <- 0

        for(i in vec){

          iter <- iter + 1
          sub <- test[(i:(i+9999)),]

          #### Modifying activity counts
          sub$statuses_count_log<-log(sub$statuses_count)
          sub$statuses_count_log[is.infinite(sub$statuses_count_log)]<-0

          sub$statuses_quantile<-sub$statuses_count
          sub$statuses_quantile[sub$statuses_count>=0 & sub$statuses_count<157]<-1
          sub$statuses_quantile[sub$statuses_count>=157 & sub$statuses_count<1897]<-2
          sub$statuses_quantile[sub$statuses_count>=1897 & sub$statuses_count<11632]<-3
          sub$statuses_quantile[sub$statuses_count>=11632 & sub$statuses_count<=686052]<-4

          sub$followers_count_log<-log(sub$statuses_count)
          sub$followers_count_log[is.infinite(sub$followers_count_log)]<-0

          sub$followers_quantile<-sub$followers_count
          sub$followers_quantile[sub$followers_count>=0 & sub$followers_count<29]<-1
          sub$followers_quantile[sub$followers_count>=29 & sub$followers_count<159]<-2
          sub$followers_quantile[sub$followers_count>=159 & sub$followers_count<557]<-3
          sub$followers_quantile[sub$followers_count>=557 & sub$followers_count<=208437]<-4

          sub$friends_count_log<-log(sub$friends_count)
          sub$friends_count_log[is.infinite(sub$friends_count_log)]<-0

          sub$friends_quantile<-sub$friends_count
          sub$friends_quantile[sub$friends_count>=0 & sub$friends_count<81]<-1
          sub$friends_quantile[sub$friends_count>=81 & sub$friends_count<256]<-2
          sub$friends_quantile[sub$friends_count>=256 & sub$friends_count<680]<-3
          sub$friends_quantile[sub$friends_count>=680 & sub$friends_count<=101531]<-4

          sub$favourites_count_log<-log(sub$favourites_count)
          sub$favourites_count_log[is.infinite(sub$favourites_count_log)]<-0


          sub$favourites_quantile<-sub$favourites_count
          sub$favourites_quantile[sub$favourites_count>=0 & sub$favourites_count<43]<-1
          sub$favourites_quantile[sub$favourites_count>=43 & sub$favourites_count<807]<-2
          sub$favourites_quantile[sub$favourites_count>=807 & sub$favourites_count<5775]<-3
          sub$favourites_quantile[sub$favourites_count>=5775 & sub$favourites_count<=330554]<-4


          sub$listed_count_log<-log(sub$listed_count)
          sub$listed_count_log[is.infinite(sub$listed_count_log)]<-0

          ## Ratio of friends to followers (from PLOS paper)
          sub$friend_follower<-sub$friends_count/sub$followers_count
          sub$friend_follower[is.na(sub$friend_follower)]<-0
          sub$friend_follower[is.infinite(sub$friend_follower)]<-sub$friends_count[is.infinite(sub$friend_follower)]


          #### Does the user share a url?
          sub$url_binary<-sub$url
          sub$url_binary[is.na(sub$url)]<-0
          sub$url_binary[!is.na(sub$url)]<-1


          #### Does the URL mention social media? (missing values marked as NA)
          sub$url_expanded_sm_na<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$url_expanded_sm_na[i]<-0
            }
          }
          sub$url_expanded_sm_na[grep("insta", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("snap", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("youtube", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("facebook", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("linkedin", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("tumblr", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("wordpress", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("pinterest", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("curiouscat", sub$url_expanded, ignore.case=TRUE)]<-1

          #### Does the URL mention social media? (missing values marked as 0)
          sub$url_expanded_sm<-0
          sub$url_expanded_sm[grep("insta", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("snap", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("youtube", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("facebook", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("linkedin", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("tumblr", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("wordpress", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("pinterest", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("curiouscat", sub$url_expanded, ignore.case=TRUE)]<-1


          #### Does the user provide a location?
          sub$location_binary<-sub$location
          sub$location_binary[is.na(sub$location)]<-0
          sub$location_binary[!is.na(sub$location)]<-1

          #### Does the user provide a banner URL?
          sub$profile_banner_url_binary<-sub$profile_banner_url
          sub$profile_banner_url_binary[is.na(sub$profile_banner_url)]<-0
          sub$profile_banner_url_binary[!is.na(sub$profile_banner_url)]<-1

          #### Does the user have a default profile image?
          sub$profile_image_default<-0
          sub$profile_image_default[grep("default", sub$profile_image_url)]<-1

          #### Does the user use template profile colors?
          sub$template_color<-0
          sub$template_color[which(sub$profile_link_color=="981CEB" |
                                         sub$profile_link_color=="F58EA8" |
                                         sub$profile_link_color=="E81C4F" |
                                         sub$profile_link_color=="1B95E0" |
                                         sub$profile_link_color=="91D2FA" |
                                         sub$profile_link_color=="19CF86" |
                                         sub$profile_link_color=="7FDBB6" |
                                         sub$profile_link_color=="FAB81E" |
                                         sub$profile_link_color=="FF691F" |
                                         sub$profile_link_color=="ABB8C2")] <-1



          #### How similar are the user's name and screen name?
          sub$name_clean<-iconv(sub$name, "UTF-8", "ASCII", sub="") # remove UTF-8 characters without destroying the entire name
          sub$name_clean<-removePunctuation(sub$name_clean) # remove punctuation
          sub$name_clean<-tolower(stripWhitespace(as.character(sub$name_clean))) #normalize, remove whitespace
          sub$name_clean<-unlist(lapply(sub$name_clean, function(x) gsub(" ", "", x)))

          sub$screen_name_clean<-tolower(sub$screen_name)

          sub$similarity<-NA
          for(i in 1:dim(sub)[1]){
            sub$similarity[i]<-levenshteinSim(sub$name_clean[i], sub$screen_name_clean[i])
          }


          #### How many months has the user been active? How often do they tweet
          sub$year_created <- NA
          sub$days_active <- NA
          sub$months_active <- NA
          sub$avg_twt_per_mon <- NA

          for (i in 1:dim(sub)[1]){
            if(!is.na(sub$laststatus_created_at[i])){
              sub$year_created[i] <- tail(unlist(strsplit(sub$created_at[i], " ")), 1)
              sub$days_active[i]<-as.numeric(as.Date(dateConvert(sub$laststatus_created_at[i]))-as.Date(dateConvert(age_all$created_at[i])))
              sub$months_active[i]<-as.numeric(as.Date(dateConvert(sub$laststatus_created_at[i]))-as.Date(dateConvert(sub$created_at[i])))/30
              sub$avg_twt_per_mon[i]<-as.numeric(sub$statuses_count[i])/sub$months_active[i]
            }
            if(i %% 100 == 0){
              print(i)
            }
          }


          sub$avg_twt_per_mon[is.infinite(sub$avg_twt_per_mon)]<-0
          sub$avg_twt_per_mon[is.na(sub$avg_twt_per_mon)]<-0

          sub$avg_twt_per_mon_quant<-sub$avg_twt_per_mon
          sub$avg_twt_per_mon_quant[sub$avg_twt_per_mon<15]<-1
          sub$avg_twt_per_mon_quant[sub$avg_twt_per_mon>=15 & sub$avg_twt_per_mon<104]<-2
          sub$avg_twt_per_mon_quant[sub$avg_twt_per_mon>=104 & sub$avg_twt_per_mon<388]<-3
          sub$avg_twt_per_mon_quant[sub$avg_twt_per_mon>=388]<-4


          sub$avg_twt_per_mon_log<-log(sub$avg_twt_per_mon)
          sub$avg_twt_per_mon_log[is.infinite(sub$avg_twt_per_mon_log)]<-0


          #### Favourites count, normalized for lifespan of the account
          sub$favourites_normalized<-NA

          for(i in 1:dim(sub)[1]){
            days<-as.numeric(as.POSIXct(sub$laststatus_created_at[i])-as.POSIXct(dateConvert(sub$created_at[i])))
            if(days==1){
              sub$favourites_normalized[i]<-sub$favourites_count[i]
            }
            else{
              sub$favourites_normalized[i]<-sub$favourites_count[i]/days
            }
          }



          #### Source: what is the user tweeting from?
          sub$device<-unlist(lapply(sub$laststatus_source, function(x) gsub("</a", "", unlist(strsplit(x, ">"))[2])))

          sub$device[-which(sub$device=="tweetDeck" |
                                  sub$device=="SocialToaster" |
                                  sub$device=="Lucktastic Android" |
                                  sub$device=="Facebook" |
                                  sub$device=="Instagram" |
                                  sub$device=="Twitter for iPad" |
                                  sub$device=="Twitter Lite" |
                                  sub$device=="Google" |
                                  sub$device=="Twitter Web Client" |
                                  sub$device=="Twitter for iPhone" |
                                  sub$device=="Twitter for Android") ] <-"Other"


          #### Source: Does the user tweet from an iPhone?
          sub$iPhone<-0
          sub$iPhone[which(sub$device=="Twitter for iPhone")]<-1


          #### Does the profile include a background image?
          sub$profile_background_image_url_binary<-sub$profile_background_image_url
          sub$profile_background_image_url_binary[is.na(sub$profile_background_image_url)]<-0
          sub$profile_background_image_url_binary[!is.na(sub$profile_background_image_url)]<-1


          #### How many words are in the user's description?
          sub$description_length_word<-unlist(lapply(sub$description, function(x) length(unlist(strsplit(x, " ")))))


          #### How many characters are in the user's description?
          sub$description_length_char<-unlist(lapply(sub$description, function(x) length(unlist(strsplit(x, "")))))


          #### Presence of emojis in description
          #sub$description_decode<-unlist(lapply(sub$description, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode

          sub$description_emoji <- NA
          sub$name_emoji <- NA

          #for(i in 1:dim(sub)[1]){
          #  sub<-unlist(strsplit(sub$description_decode[i], ""))
          #  sub<-sub[-grep(" ", sub)]
          #  all<-length(sub)
          #  alphanumeric<-length(grep("[[:digit:]]", sub))+length(grep("[[:alpha:]]", sub))
          #  sub$description_emoji[i]<-(all-alphanumeric)/all
          #  print(i)
          #}

          for(i in 1:dim(sub)[1]){
            vec <- unlist(strsplit(sub$description[i], " "))
            vec <- vec[-which(vec=="")]
            sum1 <- which(unlist(lapply(vec, function(x) isUpperMethod1(trim(gsub('[[:digit:]]+', '', x)))))==TRUE) # unicode caps
            sum2 <- grep("<", vec)
            sum3 <- grep("[^[:alnum:]]", unlist(strsplit(vec, " ")))
            sum4 <- grep("#", unlist(strsplit(vec, " ")))
            sum5 <- grep("@", unlist(strsplit(vec, " ")))

            vec1 <- unique(c(sum1, sum2, sum3))
            vec2 <- unique(c(sum4, sum5))

            sum <- length(setdiff(vec1, vec2))
            sub$description_emoji[i] <- sum
            if(i %% 100 == 0){
              print(i)
            }
          }


          #### Does the user's name include emojis
          for(i in 1:dim(sub)[1]){
            vec <- unlist(strsplit(sub$name[i], " "))
            vec <- vec[-which(vec=="")]
            sum1 <- which(unlist(lapply(vec, function(x) isUpperMethod1(trim(gsub('[[:digit:]]+', '', x)))))==TRUE) # unicode caps
            sum2 <- grep("<", vec)
            sum3 <- grep("[^[:alnum:]]", unlist(strsplit(vec, " ")))
            sum4 <- grep("#", unlist(strsplit(vec, " ")))
            sum5 <- grep("@", unlist(strsplit(vec, " ")))

            vec1 <- unique(c(sum1, sum2, sum3))
            vec2 <- unique(c(sum4, sum5))

            sum <- length(setdiff(vec1, vec2))
            sub$name_emoji[i] <- sum
            if(i %% 100 == 0){
              print(i)
            }
          }


          #### Does the description include personal pronouns?
          sub$description_pronouns<-0

          for(i in 1:dim(sub)[1]){
            if(length(grep("i", unlist(strsplit(sub$description[i], " ")), ignore.case=TRUE))>1){
              sub$description_pronouns[i]<-1
            }
            if(length(grep("me", unlist(strsplit(sub$description[i], " ")), ignore.case=TRUE))>1){
              sub$description_pronouns[i]<-1
            }
            if(length(grep("my", unlist(strsplit(sub$description[i], " ")), ignore.case=TRUE))>1){
              sub$description_pronouns[i]<-1
            }
          }


          #### How many hashtags are in the description?
          sub$description_hashtag_count<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$description_hashtag_count[i]<-length(grep("#", unlist(strsplit(sub$description[i], ""))))
            }
          }

          sub$description_hashtag_binary<-sub$description_hashtag_count
          sub$description_hashtag_binary[sub$description_hashtag_count>=1]<-1
          sub$description_hashtag_binary[sub$description_hashtag_count<1 & !is.na(sub$description_hashtag_count)]<-0


          ### How many "@" are in the description?
          sub$description_at_count<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$description_at_count[i]<-length(grep("@", unlist(strsplit(sub$description[i], ""))))
            }
          }

          sub$description_at_binary<-sub$description_at_count
          sub$description_at_binary[sub$description_at_count>=1]<-1
          sub$description_at_binary[sub$description_at_count<1 & !is.na(sub$description_at_count)]<-0


          ### Does the user mention of other links in description?
          sub$description_web_count<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$description_web_count[i]<-length(grep("http", sub$description[i]))
            }
          }

          #### Does the description mention social media?
          sub$description_sm<-0
          sub$description_sm[grep("insta", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("snap", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("youtube", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("facebook", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("linkedin", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("tumblr", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("wordpress", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("pinterest", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("curiouscat", sub$description, ignore.case=TRUE)]<-1


          #### Mention of mail in description
          sub$description_mail_count<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$description_mail_count[i]<-length(grep("@.*.com", sub$description[i]))
            }
          }

          #### Mention of pipe in description
         sub$description_pipe_count <- length(grep("|", unlist(strsplit(sub$description[i], "")), fixed=TRUE))


          sub$description_pipe_binary<-sub$description_pipe_count
          sub$description_pipe_binary[sub$description_pipe_count>=1]<-1
          sub$description_pipe_binary[sub$description_pipe_count<1 & !is.na(sub$description_pipe_count)]<-0


          #### Mention of height in description
          sub$description_height_count<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$description_height_count[i]<-length(grep("[:digit:]'[:digit:]", sub$description[i]))
            }
          }

          ### Does the description contain stopwords?
          sub$description_stopword_count<-unlist(lapply(sub$description, function(x) length(intersect(unlist(strsplit(x, " ")), stopwords("en")))))

          sub$description_stopword_count_normalized<-NA

          for(i in 1:dim(sub)[1]){
            sub$description_stopword_count_normalized[i]<-sub$description_stopword_count[i]/length(unlist(strsplit(sub$description[i], " ")))
          }


          #### Does the last status include media?
          sub$laststatus_media_binary<-sub$laststatus_media
          sub$laststatus_media_binary[which(!is.na(sub$laststatus_text) & is.na(sub$laststatus_media))]<-0
          sub$laststatus_media_binary[which(!is.na(sub$laststatus_media))]<-1


          #### Does the last status include a URL?
          sub$laststatus_url_binary<-sub$laststatus_urls
          sub$laststatus_url_binary[which(!is.na(sub$laststatus_text) & is.na(sub$laststatus_urls))]<-0
          sub$laststatus_url_binary[which(!is.na(sub$laststatus_urls))]<-1



          #### Prepare laststatus for feature building
          sub$laststatus_text_clean<-trim(sub$laststatus_text)
          sub$laststatus_text_clean<-unlist(lapply(sub$laststatus_text_clean, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
          sub$laststatus_text_clean <-unlist(lapply(sub$laststatus_text_clean, function(x) gsub("RT ", "", x))) #these two lines remove unicode
          sub$laststatus_text_clean <-unlist(lapply(sub$laststatus_text_clean, function(x) gsub("RT: ", "", x))) #these two lines remove unicode

          sub$laststatus_text_clean<-as.character(sub$laststatus_text_clean)

          sub$description<-trim(sub$description)
          sub$description<-unlist(lapply(sub$description, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
          sub$description<-unlist(lapply(sub$description, function(x) gsub("RT ", "", x)))

          sub$description<-as.character(sub$description)

          #### Number of characters in laststatus
          for(i in 1:length(sub$laststatus_text_clean)){
            sub$laststatus_length[i]<-nchar(sub$laststatus_text_clean[i])
            if(i %% 100 == 0){
              print(i)
            }
          }

          #### Number of characters in description
          for(i in 1:length(sub$description)){
            sub$description_length[i]<-nchar(sub$description[i])
            if(i %% 100 == 0){
              print(i)
            }
          }

          sub$description_length[is.na(sub$description_length)]<-0

          #### Count upper case letters
          sub$laststatus_upper<-sapply(regmatches(sub$laststatus_text, gregexpr("[A-Z]", sub$laststatus_text, perl=TRUE)), length)
          sub$description_upper<-sapply(regmatches(sub$description, gregexpr("[A-Z]", sub$description, perl=TRUE)), length)


          #### Count all digits
          sub$laststatus_digit<-sapply(regmatches(sub$laststatus_text_clean, gregexpr("[0-9]", sub$laststatus_text_clean, perl=TRUE)), length)
          sub$description_digit<-sapply(regmatches(sub$description, gregexpr("[0-9]", sub$description, perl=TRUE)), length)


          ### Count spaces
          sub$laststatus_space<-unlist(lapply(sub$laststatus_text_clean, function(x) countSpaces(x)))
          sub$description_space<-unlist(lapply(sub$description, function(x) countSpaces(x)))


          #### Standard deviation of word length
          sub$laststatus_avg_word_length<-NA
          sub$laststatus_sd_word_length<-NA
          sub$laststatus_min_word_length<-NA
          sub$laststatus_max_word_length<-NA


          for(i in 1:length(sub$laststatus_text_clean)){
            if(!is.na(sub$laststatus_text_clean[i]) & sub$laststatus_text_clean[i]!=""){
              vec<-gsub('[[:punct:] ]+',' ', sub$laststatus_text_clean[i])
              vec<-unlist(strsplit(vec, " "))
              if(length(grep("RT", vec, ignore.case=FALSE))>=1){
                vec<-vec[-grep("RT", vec, ignore.case=FALSE)]
              }
              if(length(grep("@.*", vec, ignore.case=FALSE))>=1){
                vec<-vec[-grep("@.*", vec, ignore.case=FALSE)]
              }
              vals<-NULL
              for(j in 1:length(vec)){
                val<-nchar(vec[j])
                vals<-c(vals, val)
              }
              sub$laststatus_avg_word_length[i]<-mean(vals, na.rm=TRUE)
              sub$laststatus_sd_word_length[i]<-sd(vals, na.rm=TRUE)
              sub$laststatus_min_word_length[i]<-min(vals, na.rm=TRUE)
              sub$laststatus_max_word_length[i]<-max(vals, na.rm=TRUE)
            }
            if(i %% 100 == 0){
              print(i)
            }
          }


          sub$description_avg_word_length<-NA
          sub$description_sd_word_length<-NA
          sub$description_min_word_length<-NA
          sub$description_max_word_length<-NA


          for(i in 1:length(sub$description)){
            if(!is.na(sub$description[i]) & sub$description[i]!=""){
              vec<-gsub('[[:punct:] ]+',' ', sub$description[i])
              vec<-unlist(strsplit(vec, " "))
              if(length(grep("RT", vec, ignore.case=FALSE))>=1){
                vec<-vec[-grep("RT", vec, ignore.case=FALSE)]
              }
              if(length(grep("@.*", vec, ignore.case=FALSE))>=1){
                vec<-vec[-grep("@.*", vec, ignore.case=FALSE)]
              }
              vals<-NULL
              for(j in 1:length(vec)){
                val<-nchar(vec[j])
                vals<-c(vals, val)
              }
              sub$description_avg_word_length[i]<-mean(vals, na.rm=TRUE)
              sub$description_sd_word_length[i]<-sd(vals, na.rm=TRUE)
              sub$description_min_word_length[i]<-min(vals, na.rm=TRUE)
              sub$description_max_word_length[i]<-max(vals, na.rm=TRUE)
            }
            if(i %% 100 == 0){
              print(i)
            }
          }

          #### Slang terms in the laststatus
          sub$laststatus_slang<-0
          for(i in 1:length(slang_terms)){
            idx<-grep(slang_terms[i], sub$laststatus_text, ignore.case=TRUE)
            sub$laststatus_slang[idx]<-1
          }

          sub$laststatus_slang[which(is.na(sub$laststatus_text_clean))]<-NA


          #### Punctuation
          sub$laststatus_punct <- NA

          for(i in 1:length(sub$laststatus_text)){
            vec<-unlist(strsplit(sub$laststatus_text[i], ""))
            val1<-which(vec == ".")
            val2<-which(vec == ",")
            val3<-which(vec == "!")
            val4<-which(vec == "?")
            val5<-which(vec == ":")
            val6<-which(vec == ";")
            val7<-which(vec == "'")
            val7<-which(vec == "\"")
            len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7)))
            sub$laststatus_punct[i]<-len
            if(i %% 100 == 0){
              print(i)
            }
          }

          sub$description_punct <- NA

          for(i in 1:length(sub$description)){
            vec<-unlist(strsplit(sub$description[i], ""))
            val1<-which(vec == ".")
            val2<-which(vec == ",")
            val3<-which(vec == "!")
            val4<-which(vec == "?")
            val5<-which(vec == ":")
            val6<-which(vec == ";")
            val7<-which(vec == "'")
            val7<-which(vec == "\"")
            len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7)))
            sub$description_punct[i]<-len
            if(i %% 100 == 0){
              print(i)
            }
          }



          #### Number of special characters
          sub$laststatus_char <- NA

          for(i in 1:length(sub$laststatus_text_clean)){
            vec<-unlist(strsplit(sub$laststatus_text_clean[i], ""))
            val1<-which(vec == "@")
            val2<-which(vec == "#")
            val3<-which(vec == "$")
            val4<-which(vec == "%")
            val5<-which(vec == "&")
            val6<-which(vec == "*")
            val7<-which(vec == "~")
            val8<-which(vec == "^")
            val9<-which(vec == "-")
            val10<-which(vec == "=")
            val11<-which(vec == "+")
            val12<-which(vec == ">")
            val13<-which(vec == "<")
            val14<-which(vec == "[")
            val15<-which(vec == "]")
            val16<-which(vec == "{")
            val17<-which(vec == "}")
            val18<-which(vec == "|")
            val19<-which(vec == "\\")
            val20<-which(vec == "/")
            len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7,val8,val9,val10,val11,val12,val13,val14,val15,val16,val17,val18,val19,val20)))
            sub$laststatus_char[i]<-len
            if(i %% 100 == 0){
              print(i)
            }
          }

          sub$description_char <- NA

          for(i in 1:length(sub$description)){
            vec<-unlist(strsplit(sub$description[i], ""))
            val1<-which(vec == "@")
            val2<-which(vec == "#")
            val3<-which(vec == "$")
            val4<-which(vec == "%")
            val5<-which(vec == "&")
            val6<-which(vec == "*")
            val7<-which(vec == "~")
            val8<-which(vec == "^")
            val9<-which(vec == "-")
            val10<-which(vec == "=")
            val11<-which(vec == "+")
            val12<-which(vec == ">")
            val13<-which(vec == "<")
            val14<-which(vec == "[")
            val15<-which(vec == "]")
            val16<-which(vec == "{")
            val17<-which(vec == "}")
            val18<-which(vec == "|")
            val19<-which(vec == "\\")
            val20<-which(vec == "/")
            len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7,val8,val9,val10,val11,val12,val13,val14,val15,val16,val17,val18,val19,val20)))
            sub$description_char[i]<-len
            if(i %% 100 == 0){
              print(i)
            }
          }


          #### Length of description
          sub$description_length<-unlist(lapply(sub$description, function(x) length(unlist(strsplit(x, " ")))))


          #### Number of emoticons
          sub$laststatus_emoticon_count<-0
          sub$laststatus_emoticon_count<-unlist(lapply(sub$laststatus_text_clean, function(x) find_emoticon(x)))

          sub$description_emoticon_count<-0
          sub$description_emoticon_count<-unlist(lapply(sub$description, function(x) find_emoticon(x)))


          #### Use of profanity
          sub$laststatus_profane<-NA

          for (i in 1:length(sub$laststatus_text_clean)){
            if(!is.na(sub$laststatus_text_clean[i])){
              vec<-unlist(strsplit(sub$laststatus_text_clean[i], " "))
              val1<-grep("shit", vec, ignore.case=TRUE)
              val2<-grep("fuck", vec, ignore.case=TRUE)
              val3<-grep("ass", vec, ignore.case=TRUE)
              val4<-grep("piss", vec, ignore.case=TRUE)
              val5<-grep("bitch", vec, ignore.case=TRUE)
              val6<-grep("damn", vec, ignore.case=TRUE)
              sub$laststatus_profane[i]<-length(unique(c(val1, val2, val3, val4, val5, val6)))
            }
          }


          sub$description_profane<-NA

          for (i in 1:length(sub$description)){
            if(!is.na(sub$description[i])){
              vec<-unlist(strsplit(sub$description[i], " "))
              val1<-grep("shit", vec, ignore.case=TRUE)
              val2<-grep("fuck", vec, ignore.case=TRUE)
              val3<-grep("ass", vec, ignore.case=TRUE)
              val4<-grep("piss", vec, ignore.case=TRUE)
              val5<-grep("bitch", vec, ignore.case=TRUE)
              val6<-grep("damn", vec, ignore.case=TRUE)
              sub$description_profane[i]<-length(unique(c(val1, val2, val3, val4, val5, val6)))
            }
          }


          #### Vocabularly richness (number of unique words per tweet - minus stop words)
          sub$laststatus_vocab_richness<-NA

          for (i in 1:length(sub$laststatus_text_clean)){
            if(!is.na(sub$laststatus_text_clean[i])){
              newVec<-removeWords(sub$laststatus_text_clean[i], stopwords("en"))
              newVec<-removePunctuation(newVec)
              newVec<-trim(newVec)
              len1<-length(unlist(strsplit(newVec, " ")))
              len2<-length(unique(unlist(strsplit(newVec, " "))))
              sub$laststatus_vocab_richness[i]<-len2/len1
            }
          }

          sub$description_vocab_richness<-NA

          for (i in 1:length(sub$description)){
            if(!is.na(sub$description[i])){
              newVec<-removeWords(sub$description[i], stopwords("en"))
              newVec<-removePunctuation(newVec)
              newVec<-trim(newVec)
              len1<-length(unlist(strsplit(newVec, " ")))
              len2<-length(unique(unlist(strsplit(newVec, " "))))
              sub$description_vocab_richness[i]<-len2/len1
            }
          }

          #### User retweets someone else
          sub$laststatus_retweet<-0
          sub$laststatus_retweet[grep("RT ", sub$laststatus_text, ignore.case=FALSE)]<-1

          #### User tweets at someone else
          sub$laststatus_at<-0
          sub$laststatus_at[grep("(^|[^@\\w])@(\\w{1,15})\\b", sub$laststatus_text, ignore.case=FALSE)]<-1
          sub$laststatus_at[grep("RT ", sub$laststatus_text, ignore.case=FALSE)]<-0

          #### Laststatus retweet count
          ## use as is

          #### Laststatus favorite
          ## use as is

          #### Laststatus media binary
          sub$laststatus_media_binary<-0
          sub$laststatus_media_binary[!is.na(sub$laststatus_media)]<-1

          #### Laststatus geo binary
          sub$laststatus_geo_binary<-0
          sub$laststatus_geo_binary[!is.na(sub$laststatus_geo)]<-1

          #### Laststatus term repetition (i.e. okaaaaaaay)
          sub$laststatus_repeated<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$laststatus_text[i])){
              sub$laststatus_repeated[i]<-nchar(sub$laststatus_text_clean[i])-nchar(gsub('([[:alpha:]])\\1+', '\\1', sub$laststatus_text_clean[i]))
            }
            if(i %% 100 == 0){
              print(i)
            }
          }


          #### Description term repetition
          sub$description_repeated<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$description_repeated[i]<-nchar(sub$description[i])-nchar(gsub('([[:alpha:]])\\1+', '\\1', sub$description[i]))
            }
            if(i %% 100 == 0){
              print(i)
            }

          #### Positive or negative content using senti-word
          print("Estimating sentiment of last posted status...this may take a moment")
          sub$laststatus_sentiment<-unlist(mclapply(sub$laststatus_text_clean, function(x) mean(sentiment(x)$sentiment), mc.cores=10L))

          print("Estimating sentiment of profile description...this may take a moment")
          sub$description_sentiment<-unlist(mclapply(sub$description, function(x) mean(sentiment(x)$sentiment), mc.cores=10L))

          #### Assign topics to users
          print("Assigning topics to last posted status.. this may take a moment")
          sub$screen_name<-sub$screen_name
          sub$laststatus_text<-sub$laststatus_text

          source(paste0(workingDir, "ensemble_methods/frequently_used_functions/user_lda_topic.R"))
          results<-lda_topic_4_user(sub, 5, "laststatus_text", "id_str")
          names(results)[2]<-"laststatus_topic"

          colName <- names(results)[1]

          sub<-merge(sub, results, by=colName, all=TRUE)

          #### Assign topics to description field
          print("Assigning topics to last posted status...this may take a moment")
          results<-lda_topic_4_user(sub, 5, "description", "id_str")
          names(results)[2]<-"description_topic"

          colName <- names(results)[1]

          sub<-merge(sub, results, by=colName, all=TRUE)

          sub$year_created <- as.factor(sub$year_created)
          sub$geo_enabled <- as.factor(sub$geo_enabled)
          sub$profile_banner_url_binary <- as.factor(sub$profile_banner_url_binary)
          sub$description_hashtag_binary <- as.factor(sub$description_hashtag_binary)
          sub$profile_background_image_url_binary <- as.factor(sub$profile_background_image_url_binary)
          sub$location_binary <- as.factor(sub$location_binary)

          print(paste0("Predicting values for chunk ", iter, "!"))
          model_predict_probs <- predict.gbm(model, sub, best.iter, type="response")
          model_predict <- apply(model_predict_probs[,,1], 1, which.max)
          model_predict[model_predict==1]<-"20 to 30"
          model_predict[model_predict==2]<-"Above 30"
          model_predict[model_predict==3]<-"Below 20"


          print(paste0("Predictions are done for chunk ", iter, "!"))

          results_part<-data.frame(id_str = as.character(sub$id_str), age_prediction=model_predict)

          results <- rbind(results, results_part)
        }
        return(results)
      }
    }

      if(chunk==TRUE & action=="save"){

        test<-input

        if(length(which(is.na(test$screen_name)))>0){
          test <- test[-which(is.na(test$screen_name)),]
        }

        vec <- seq(1, nrow(test), by=10000)

        results <- NULL
        iter <- 0

        for(i in vec){

          iter <- iter +1
          sub <- test[(i:(i+9999)),]

          #### Modifying activity counts
          sub$statuses_count_log<-log(sub$statuses_count)
          sub$statuses_count_log[is.infinite(sub$statuses_count_log)]<-0

          sub$statuses_quantile<-sub$statuses_count
          sub$statuses_quantile[sub$statuses_count>=0 & sub$statuses_count<157]<-1
          sub$statuses_quantile[sub$statuses_count>=157 & sub$statuses_count<1897]<-2
          sub$statuses_quantile[sub$statuses_count>=1897 & sub$statuses_count<11632]<-3
          sub$statuses_quantile[sub$statuses_count>=11632 & sub$statuses_count<=686052]<-4

          sub$followers_count_log<-log(sub$statuses_count)
          sub$followers_count_log[is.infinite(sub$followers_count_log)]<-0

          sub$followers_quantile<-sub$followers_count
          sub$followers_quantile[sub$followers_count>=0 & sub$followers_count<29]<-1
          sub$followers_quantile[sub$followers_count>=29 & sub$followers_count<159]<-2
          sub$followers_quantile[sub$followers_count>=159 & sub$followers_count<557]<-3
          sub$followers_quantile[sub$followers_count>=557 & sub$followers_count<=208437]<-4

          sub$friends_count_log<-log(sub$friends_count)
          sub$friends_count_log[is.infinite(sub$friends_count_log)]<-0

          sub$friends_quantile<-sub$friends_count
          sub$friends_quantile[sub$friends_count>=0 & sub$friends_count<81]<-1
          sub$friends_quantile[sub$friends_count>=81 & sub$friends_count<256]<-2
          sub$friends_quantile[sub$friends_count>=256 & sub$friends_count<680]<-3
          sub$friends_quantile[sub$friends_count>=680 & sub$friends_count<=101531]<-4

          sub$favourites_count_log<-log(sub$favourites_count)
          sub$favourites_count_log[is.infinite(sub$favourites_count_log)]<-0


          sub$favourites_quantile<-sub$favourites_count
          sub$favourites_quantile[sub$favourites_count>=0 & sub$favourites_count<43]<-1
          sub$favourites_quantile[sub$favourites_count>=43 & sub$favourites_count<807]<-2
          sub$favourites_quantile[sub$favourites_count>=807 & sub$favourites_count<5775]<-3
          sub$favourites_quantile[sub$favourites_count>=5775 & sub$favourites_count<=330554]<-4


          sub$listed_count_log<-log(sub$listed_count)
          sub$listed_count_log[is.infinite(sub$listed_count_log)]<-0

          ## Ratio of friends to followers (from PLOS paper)
          sub$friend_follower<-sub$friends_count/sub$followers_count
          sub$friend_follower[is.na(sub$friend_follower)]<-0
          sub$friend_follower[is.infinite(sub$friend_follower)]<-sub$friends_count[is.infinite(sub$friend_follower)]


          #### Does the user share a url?
          sub$url_binary<-sub$url
          sub$url_binary[is.na(sub$url)]<-0
          sub$url_binary[!is.na(sub$url)]<-1


          #### Does the URL mention social media? (missing values marked as NA)
          sub$url_expanded_sm_na<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$url_expanded_sm_na[i]<-0
            }
          }
          sub$url_expanded_sm_na[grep("insta", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("snap", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("youtube", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("facebook", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("linkedin", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("tumblr", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("wordpress", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("pinterest", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm_na[grep("curiouscat", sub$url_expanded, ignore.case=TRUE)]<-1

          #### Does the URL mention social media? (missing values marked as 0)
          sub$url_expanded_sm<-0
          sub$url_expanded_sm[grep("insta", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("snap", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("youtube", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("facebook", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("linkedin", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("tumblr", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("wordpress", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("pinterest", sub$url_expanded, ignore.case=TRUE)]<-1
          sub$url_expanded_sm[grep("curiouscat", sub$url_expanded, ignore.case=TRUE)]<-1


          #### Does the user provide a location?
          sub$location_binary<-sub$location
          sub$location_binary[is.na(sub$location)]<-0
          sub$location_binary[!is.na(sub$location)]<-1

          #### Does the user provide a banner URL?
          sub$profile_banner_url_binary<-sub$profile_banner_url
          sub$profile_banner_url_binary[is.na(sub$profile_banner_url)]<-0
          sub$profile_banner_url_binary[!is.na(sub$profile_banner_url)]<-1

          #### Does the user have a default profile image?
          sub$profile_image_default<-0
          sub$profile_image_default[grep("default", sub$profile_image_url)]<-1

          #### Does the user use template profile colors?
          sub$template_color<-0
          sub$template_color[which(sub$profile_link_color=="981CEB" |
                                         sub$profile_link_color=="F58EA8" |
                                         sub$profile_link_color=="E81C4F" |
                                         sub$profile_link_color=="1B95E0" |
                                         sub$profile_link_color=="91D2FA" |
                                         sub$profile_link_color=="19CF86" |
                                         sub$profile_link_color=="7FDBB6" |
                                         sub$profile_link_color=="FAB81E" |
                                         sub$profile_link_color=="FF691F" |
                                         sub$profile_link_color=="ABB8C2")] <-1



          #### How similar are the user's name and screen name?
          sub$name_clean<-iconv(sub$name, "UTF-8", "ASCII", sub="") # remove UTF-8 characters without destroying the entire name
          sub$name_clean<-removePunctuation(sub$name_clean) # remove punctuation
          sub$name_clean<-tolower(stripWhitespace(as.character(sub$name_clean))) #normalize, remove whitespace
          sub$name_clean<-unlist(lapply(sub$name_clean, function(x) gsub(" ", "", x)))

          sub$screen_name_clean<-tolower(sub$screen_name)

          sub$similarity<-NA
          for(i in 1:dim(sub)[1]){
            sub$similarity[i]<-levenshteinSim(sub$name_clean[i], sub$screen_name_clean[i])
          }


          #### How many months has the user been active? How often do they tweet
          sub$year_created<-NA
          sub$days_active <- NA
          sub$months_active<-NA
          sub$avg_twt_per_mon<-NA

          for (i in 1:dim(sub)[1]){
            if(!is.na(sub$laststatus_created_at[i])){
              sub$year_created[i] <- tail(unlist(strsplit(sub$created_at[i], " ")), 1)
              sub$days_active[i]<-as.numeric(as.Date(dateConvert(sub$laststatus_created_at[i]))-as.Date(dateConvert(age_all$created_at[i])))
              sub$months_active[i]<-as.numeric(as.Date(dateConvert(sub$laststatus_created_at[i]))-as.Date(dateConvert(sub$created_at[i])))/30
              sub$avg_twt_per_mon[i]<-as.numeric(sub$statuses_count[i])/sub$months_active[i]
            }
            if(i %% 100 == 0){
              print(i)
            }
          }


          sub$avg_twt_per_mon[is.infinite(sub$avg_twt_per_mon)]<-0
          sub$avg_twt_per_mon[is.na(sub$avg_twt_per_mon)]<-0

          sub$avg_twt_per_mon_quant<-sub$avg_twt_per_mon
          sub$avg_twt_per_mon_quant[sub$avg_twt_per_mon<15]<-1
          sub$avg_twt_per_mon_quant[sub$avg_twt_per_mon>=15 & sub$avg_twt_per_mon<104]<-2
          sub$avg_twt_per_mon_quant[sub$avg_twt_per_mon>=104 & sub$avg_twt_per_mon<388]<-3
          sub$avg_twt_per_mon_quant[sub$avg_twt_per_mon>=388]<-4


          sub$avg_twt_per_mon_log<-log(sub$avg_twt_per_mon)
          sub$avg_twt_per_mon_log[is.infinite(sub$avg_twt_per_mon_log)]<-0


          #### Favourites count, normalized for lifespan of the account
          sub$favourites_normalized<-NA

          for(i in 1:dim(sub)[1]){
            days<-as.numeric(as.POSIXct(sub$laststatus_created_at[i])-as.POSIXct(dateConvert(sub$created_at[i])))
            if(days==1){
              sub$favourites_normalized[i]<-sub$favourites_count[i]
            }
            else{
              sub$favourites_normalized[i]<-sub$favourites_count[i]/days
            }
          }



          #### Source: what is the user tweeting from?
          sub$device<-unlist(lapply(sub$laststatus_source, function(x) gsub("</a", "", unlist(strsplit(x, ">"))[2])))

          sub$device[-which(sub$device=="tweetDeck" |
                                  sub$device=="SocialToaster" |
                                  sub$device=="Lucktastic Android" |
                                  sub$device=="Facebook" |
                                  sub$device=="Instagram" |
                                  sub$device=="Twitter for iPad" |
                                  sub$device=="Twitter Lite" |
                                  sub$device=="Google" |
                                  sub$device=="Twitter Web Client" |
                                  sub$device=="Twitter for iPhone" |
                                  sub$device=="Twitter for Android") ] <-"Other"


          #### Source: Does the user tweet from an iPhone?
          sub$iPhone<-0
          sub$iPhone[which(sub$device=="Twitter for iPhone")]<-1


          #### Does the profile include a background image?
          sub$profile_background_image_url_binary<-sub$profile_background_image_url
          sub$profile_background_image_url_binary[is.na(sub$profile_background_image_url)]<-0
          sub$profile_background_image_url_binary[!is.na(sub$profile_background_image_url)]<-1


          #### How many words are in the user's description?
          sub$description_length_word<-unlist(lapply(sub$description, function(x) length(unlist(strsplit(x, " ")))))


          #### How many characters are in the user's description?
          sub$description_length_char<-unlist(lapply(sub$description, function(x) length(unlist(strsplit(x, "")))))


          #### Presence of emojis in description
          #sub$description_decode<-unlist(lapply(sub$description, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode

          sub$description_emoji <- NA
          sub$name_emoji <- NA

          #for(i in 1:dim(sub)[1]){
          #  sub<-unlist(strsplit(sub$description_decode[i], ""))
          #  sub<-sub[-grep(" ", sub)]
          #  all<-length(sub)
          #  alphanumeric<-length(grep("[[:digit:]]", sub))+length(grep("[[:alpha:]]", sub))
          #  sub$description_emoji[i]<-(all-alphanumeric)/all
          #  print(i)
          #}

          for(i in 1:dim(sub)[1]){
            vec <- unlist(strsplit(sub$description[i], " "))
            vec <- vec[-which(vec=="")]
            sum1 <- which(unlist(lapply(vec, function(x) isUpperMethod1(trim(gsub('[[:digit:]]+', '', x)))))==TRUE) # unicode caps
            sum2 <- grep("<", vec)
            sum3 <- grep("[^[:alnum:]]", unlist(strsplit(vec, " ")))
            sum4 <- grep("#", unlist(strsplit(vec, " ")))
            sum5 <- grep("@", unlist(strsplit(vec, " ")))

            vec1 <- unique(c(sum1, sum2, sum3))
            vec2 <- unique(c(sum4, sum5))

            sum <- length(setdiff(vec1, vec2))
            sub$description_emoji[i] <- sum
            if(i %% 100 == 0){
              print(i)
            }
          }


          #### Does the user's name include emojis
          for(i in 1:dim(sub)[1]){
            vec <- unlist(strsplit(sub$name[i], " "))
            vec <- vec[-which(vec=="")]
            sum1 <- which(unlist(lapply(vec, function(x) isUpperMethod1(trim(gsub('[[:digit:]]+', '', x)))))==TRUE) # unicode caps
            sum2 <- grep("<", vec)
            sum3 <- grep("[^[:alnum:]]", unlist(strsplit(vec, " ")))
            sum4 <- grep("#", unlist(strsplit(vec, " ")))
            sum5 <- grep("@", unlist(strsplit(vec, " ")))

            vec1 <- unique(c(sum1, sum2, sum3))
            vec2 <- unique(c(sum4, sum5))

            sum <- length(setdiff(vec1, vec2))
            sub$name_emoji[i] <- sum
            if(i %% 100 == 0){
              print(i)
            }
          }


          #### Does the description include personal pronouns?
          sub$description_pronouns<-0

          for(i in 1:dim(sub)[1]){
            if(length(grep("i", unlist(strsplit(sub$description[i], " ")), ignore.case=TRUE))>1){
              sub$description_pronouns[i]<-1
            }
            if(length(grep("me", unlist(strsplit(sub$description[i], " ")), ignore.case=TRUE))>1){
              sub$description_pronouns[i]<-1
            }
            if(length(grep("my", unlist(strsplit(sub$description[i], " ")), ignore.case=TRUE))>1){
              sub$description_pronouns[i]<-1
            }
          }


          #### How many hashtags are in the description?
          sub$description_hashtag_count<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$description_hashtag_count[i]<-length(grep("#", unlist(strsplit(sub$description[i], ""))))
            }
          }

          sub$description_hashtag_binary<-sub$description_hashtag_count
          sub$description_hashtag_binary[sub$description_hashtag_count>=1]<-1
          sub$description_hashtag_binary[sub$description_hashtag_count<1 & !is.na(sub$description_hashtag_count)]<-0


          ### How many "@" are in the description?
          sub$description_at_count<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$description_at_count[i]<-length(grep("@", unlist(strsplit(sub$description[i], ""))))
            }
          }

          sub$description_at_binary<-sub$description_at_count
          sub$description_at_binary[sub$description_at_count>=1]<-1
          sub$description_at_binary[sub$description_at_count<1 & !is.na(sub$description_at_count)]<-0


          ### Does the user mention of other links in description?
          sub$description_web_count<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$description_web_count[i]<-length(grep("http", sub$description[i]))
            }
          }

          #### Does the description mention social media?
          sub$description_sm<-0
          sub$description_sm[grep("insta", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("snap", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("youtube", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("facebook", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("linkedin", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("tumblr", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("wordpress", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("pinterest", sub$description, ignore.case=TRUE)]<-1
          sub$description_sm[grep("curiouscat", sub$description, ignore.case=TRUE)]<-1


          #### Mention of mail in description
          sub$description_mail_count<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$description_mail_count[i]<-length(grep("@.*.com", sub$description[i]))
            }
          }

          #### Mention of pipe in description
          sub$description_pipe_count <-length(grep("|", unlist(strsplit(sub$description[i], "")), fixed=TRUE))


          sub$description_pipe_binary<-sub$description_pipe_count
          sub$description_pipe_binary[sub$description_pipe_count>=1]<-1
          sub$description_pipe_binary[sub$description_pipe_count<1 & !is.na(sub$description_pipe_count)]<-0


          #### Mention of height in description
          sub$description_height_count<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$description_height_count[i]<-length(grep("[:digit:]'[:digit:]", sub$description[i]))
            }
          }

          ### Does the description contain stopwords?
          sub$description_stopword_count<-unlist(lapply(sub$description, function(x) length(intersect(unlist(strsplit(x, " ")), stopwords("en")))))

          sub$description_stopword_count_normalized<-NA

          for(i in 1:dim(sub)[1]){
            sub$description_stopword_count_normalized[i]<-sub$description_stopword_count[i]/length(unlist(strsplit(sub$description[i], " ")))
          }


          #### Does the last status include media?
          sub$laststatus_media_binary<-sub$laststatus_media
          sub$laststatus_media_binary[which(!is.na(sub$laststatus_text) & is.na(sub$laststatus_media))]<-0
          sub$laststatus_media_binary[which(!is.na(sub$laststatus_media))]<-1


          #### Does the last status include a URL?
          sub$laststatus_url_binary<-sub$laststatus_urls
          sub$laststatus_url_binary[which(!is.na(sub$laststatus_text) & is.na(sub$laststatus_urls))]<-0
          sub$laststatus_url_binary[which(!is.na(sub$laststatus_urls))]<-1



          #### Prepare laststatus for feature building
          sub$laststatus_text_clean<-trim(sub$laststatus_text)
          sub$laststatus_text_clean<-unlist(lapply(sub$laststatus_text_clean, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
          sub$laststatus_text_clean<-unlist(lapply(sub$laststatus_text_clean, function(x) gsub("RT ", "", x))) #these two lines remove unicode
          sub$laststatus_text_clean<-unlist(lapply(sub$laststatus_text_clean, function(x) gsub("RT: ", "", x))) #these two lines remove unicode

          sub$laststatus_text_clean<-as.character(sub$laststatus_text_clean)

          sub$description<-trim(sub$description)
          sub$description<-unlist(lapply(sub$description, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
          sub$description<-unlist(lapply(sub$description, function(x) gsub("RT ", "", x)))

          sub$description<-as.character(sub$description)

          #### Number of characters in laststatus
          for(i in 1:length(sub$laststatus_text_clean)){
            sub$laststatus_length[i]<-nchar(sub$laststatus_text_clean[i])
            if(i %% 100 == 0){
              print(i)
            }
          }

          #### Number of characters in description
          for(i in 1:length(sub$description)){
            sub$description_length[i]<-nchar(sub$description[i])
            if(i %% 100 == 0){
              print(i)
            }
          }

          sub$description_length[is.na(sub$description_length)]<-0

          #### Count upper case letters
          sub$laststatus_upper<-sapply(regmatches(sub$laststatus_text, gregexpr("[A-Z]", sub$laststatus_text, perl=TRUE)), length)
          sub$description_upper<-sapply(regmatches(sub$description, gregexpr("[A-Z]", sub$description, perl=TRUE)), length)


          #### Count all digits
          sub$laststatus_digit<-sapply(regmatches(sub$laststatus_text_clean, gregexpr("[0-9]", sub$laststatus_text_clean, perl=TRUE)), length)
          sub$description_digit<-sapply(regmatches(sub$description, gregexpr("[0-9]", sub$description, perl=TRUE)), length)


          ### Count spaces
          sub$laststatus_space<-unlist(lapply(sub$laststatus_text_clean, function(x) countSpaces(x)))
          sub$description_space<-unlist(lapply(sub$description, function(x) countSpaces(x)))


          #### Standard deviation of word length
          sub$laststatus_avg_word_length<-NA
          sub$laststatus_sd_word_length<-NA
          sub$laststatus_min_word_length<-NA
          sub$laststatus_max_word_length<-NA


          for(i in 1:length(sub$laststatus_text_clean)){
            if(!is.na(sub$laststatus_text_clean[i]) & sub$laststatus_text_clean[i]!=""){
              vec<-gsub('[[:punct:] ]+',' ', sub$laststatus_text_clean[i])
              vec<-unlist(strsplit(vec, " "))
              if(length(grep("RT", vec, ignore.case=FALSE))>=1){
                vec<-vec[-grep("RT", vec, ignore.case=FALSE)]
              }
              if(length(grep("@.*", vec, ignore.case=FALSE))>=1){
                vec<-vec[-grep("@.*", vec, ignore.case=FALSE)]
              }
              vals<-NULL
              for(j in 1:length(vec)){
                val<-nchar(vec[j])
                vals<-c(vals, val)
              }
              sub$laststatus_avg_word_length[i]<-mean(vals, na.rm=TRUE)
              sub$laststatus_sd_word_length[i]<-sd(vals, na.rm=TRUE)
              sub$laststatus_min_word_length[i]<-min(vals, na.rm=TRUE)
              sub$laststatus_max_word_length[i]<-max(vals, na.rm=TRUE)
            }
            if(i %% 100 == 0){
              print(i)
            }
          }


          sub$description_avg_word_length<-NA
          sub$description_sd_word_length<-NA
          sub$description_min_word_length<-NA
          sub$description_max_word_length<-NA


          for(i in 1:length(sub$description)){
            if(!is.na(sub$description[i]) & sub$description[i]!=""){
              vec<-gsub('[[:punct:] ]+',' ', sub$description[i])
              vec<-unlist(strsplit(vec, " "))
              if(length(grep("RT", vec, ignore.case=FALSE))>=1){
                vec<-vec[-grep("RT", vec, ignore.case=FALSE)]
              }
              if(length(grep("@.*", vec, ignore.case=FALSE))>=1){
                vec<-vec[-grep("@.*", vec, ignore.case=FALSE)]
              }
              vals<-NULL
              for(j in 1:length(vec)){
                val<-nchar(vec[j])
                vals<-c(vals, val)
              }
              sub$description_avg_word_length[i]<-mean(vals, na.rm=TRUE)
              sub$description_sd_word_length[i]<-sd(vals, na.rm=TRUE)
              sub$description_min_word_length[i]<-min(vals, na.rm=TRUE)
              sub$description_max_word_length[i]<-max(vals, na.rm=TRUE)
            }
            if(i %% 100 == 0){
              print(i)
            }
          }

          #### Slang terms in the laststatus
          sub$laststatus_slang<-0
          for(i in 1:length(slang_terms)){
            idx<-grep(slang_terms[i], sub$laststatus_text, ignore.case=TRUE)
            sub$laststatus_slang[idx]<-1
          }

          sub$laststatus_slang[which(is.na(sub$laststatus_text_clean))]<-NA


          #### Punctuation
          sub$laststatus_punct <- NA

          for(i in 1:length(sub$laststatus_text)){
            vec<-unlist(strsplit(sub$laststatus_text[i], ""))
            val1<-which(vec == ".")
            val2<-which(vec == ",")
            val3<-which(vec == "!")
            val4<-which(vec == "?")
            val5<-which(vec == ":")
            val6<-which(vec == ";")
            val7<-which(vec == "'")
            val7<-which(vec == "\"")
            len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7)))
            sub$laststatus_punct[i]<-len
            if(i %% 100 == 0){
              print(i)
            }
          }

          sub$description_punct <- NA

          for(i in 1:length(sub$description)){
            vec<-unlist(strsplit(sub$description[i], ""))
            val1<-which(vec == ".")
            val2<-which(vec == ",")
            val3<-which(vec == "!")
            val4<-which(vec == "?")
            val5<-which(vec == ":")
            val6<-which(vec == ";")
            val7<-which(vec == "'")
            val7<-which(vec == "\"")
            len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7)))
            sub$description_punct[i]<-len
            if(i %% 100 == 0){
              print(i)
            }
          }



          #### Number of special characters
          sub$laststatus_char <- NA

          for(i in 1:length(sub$laststatus_text_clean)){
            vec<-unlist(strsplit(sub$laststatus_text_clean[i], ""))
            val1<-which(vec == "@")
            val2<-which(vec == "#")
            val3<-which(vec == "$")
            val4<-which(vec == "%")
            val5<-which(vec == "&")
            val6<-which(vec == "*")
            val7<-which(vec == "~")
            val8<-which(vec == "^")
            val9<-which(vec == "-")
            val10<-which(vec == "=")
            val11<-which(vec == "+")
            val12<-which(vec == ">")
            val13<-which(vec == "<")
            val14<-which(vec == "[")
            val15<-which(vec == "]")
            val16<-which(vec == "{")
            val17<-which(vec == "}")
            val18<-which(vec == "|")
            val19<-which(vec == "\\")
            val20<-which(vec == "/")
            len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7,val8,val9,val10,val11,val12,val13,val14,val15,val16,val17,val18,val19,val20)))
            sub$laststatus_char[i]<-len
            if(i %% 100 == 0){
              print(i)
            }
          }

          sub$description_char <- NA

          for(i in 1:length(sub$description)){
            vec<-unlist(strsplit(sub$description[i], ""))
            val1<-which(vec == "@")
            val2<-which(vec == "#")
            val3<-which(vec == "$")
            val4<-which(vec == "%")
            val5<-which(vec == "&")
            val6<-which(vec == "*")
            val7<-which(vec == "~")
            val8<-which(vec == "^")
            val9<-which(vec == "-")
            val10<-which(vec == "=")
            val11<-which(vec == "+")
            val12<-which(vec == ">")
            val13<-which(vec == "<")
            val14<-which(vec == "[")
            val15<-which(vec == "]")
            val16<-which(vec == "{")
            val17<-which(vec == "}")
            val18<-which(vec == "|")
            val19<-which(vec == "\\")
            val20<-which(vec == "/")
            len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7,val8,val9,val10,val11,val12,val13,val14,val15,val16,val17,val18,val19,val20)))
            sub$description_char[i]<-len
            if(i %% 100 == 0){
              print(i)
            }
          }


          #### Length of description
          sub$description_length<-unlist(lapply(sub$description, function(x) length(unlist(strsplit(x, " ")))))


          #### Number of emoticons
          sub$laststatus_emoticon_count<-0
          sub$laststatus_emoticon_count<-unlist(lapply(sub$laststatus_text_clean, function(x) find_emoticon(x)))

          sub$description_emoticon_count<-0
          sub$description_emoticon_count<-unlist(lapply(sub$description, function(x) find_emoticon(x)))


          #### Use of profanity
          sub$laststatus_profane<-NA

          for (i in 1:length(sub$laststatus_text_clean)){
            if(!is.na(sub$laststatus_text_clean[i])){
              vec<-unlist(strsplit(sub$laststatus_text_clean[i], " "))
              val1<-grep("shit", vec, ignore.case=TRUE)
              val2<-grep("fuck", vec, ignore.case=TRUE)
              val3<-grep("ass", vec, ignore.case=TRUE)
              val4<-grep("piss", vec, ignore.case=TRUE)
              val5<-grep("bitch", vec, ignore.case=TRUE)
              val6<-grep("damn", vec, ignore.case=TRUE)
              sub$laststatus_profane[i]<-length(unique(c(val1, val2, val3, val4, val5, val6)))
            }
          }


          sub$description_profane<-NA

          for (i in 1:length(sub$description)){
            if(!is.na(sub$description[i])){
              vec<-unlist(strsplit(sub$description[i], " "))
              val1<-grep("shit", vec, ignore.case=TRUE)
              val2<-grep("fuck", vec, ignore.case=TRUE)
              val3<-grep("ass", vec, ignore.case=TRUE)
              val4<-grep("piss", vec, ignore.case=TRUE)
              val5<-grep("bitch", vec, ignore.case=TRUE)
              val6<-grep("damn", vec, ignore.case=TRUE)
              sub$description_profane[i]<-length(unique(c(val1, val2, val3, val4, val5, val6)))
            }
          }


          #### Vocabularly richness (number of unique words per tweet - minus stop words)
          sub$laststatus_vocab_richness<-NA

          for (i in 1:length(sub$laststatus_text_clean)){
            if(!is.na(sub$laststatus_text_clean[i])){
              newVec<-removeWords(sub$laststatus_text_clean[i], stopwords("en"))
              newVec<-removePunctuation(newVec)
              newVec<-trim(newVec)
              len1<-length(unlist(strsplit(newVec, " ")))
              len2<-length(unique(unlist(strsplit(newVec, " "))))
              sub$laststatus_vocab_richness[i]<-len2/len1
            }
          }

          sub$description_vocab_richness<-NA

          for (i in 1:length(sub$description)){
            if(!is.na(sub$description[i])){
              newVec<-removeWords(sub$description[i], stopwords("en"))
              newVec<-removePunctuation(newVec)
              newVec<-trim(newVec)
              len1<-length(unlist(strsplit(newVec, " ")))
              len2<-length(unique(unlist(strsplit(newVec, " "))))
              sub$description_vocab_richness[i]<-len2/len1
            }
          }

          #### User retweets someone else
          sub$laststatus_retweet<-0
          sub$laststatus_retweet[grep("RT ", sub$laststatus_text, ignore.case=FALSE)]<-1

          #### User tweets at someone else
          sub$laststatus_at<-0
          sub$laststatus_at[grep("(^|[^@\\w])@(\\w{1,15})\\b", sub$laststatus_text, ignore.case=FALSE)]<-1
          sub$laststatus_at[grep("RT ", sub$laststatus_text, ignore.case=FALSE)]<-0

          #### Laststatus retweet count
          ## use as is

          #### Laststatus favorite
          ## use as is

          #### Laststatus media binary
          sub$laststatus_media_binary<-0
          sub$laststatus_media_binary[!is.na(sub$laststatus_media)]<-1

          #### Laststatus geo binary
          sub$laststatus_geo_binary<-0
          sub$laststatus_geo_binary[!is.na(sub$laststatus_geo)]<-1

          #### Laststatus term repetition (i.e. okaaaaaaay)
          sub$laststatus_repeated<-NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$laststatus_text[i])){
              sub$laststatus_repeated[i]<-nchar(sub$laststatus_text_clean[i])-nchar(gsub('([[:alpha:]])\\1+', '\\1', sub$laststatus_text_clean[i]))
            }
            if(i %% 100 == 0){
              print(i)
            }
          }


          #### Description term repetition
          sub$description_repeated <- NA

          for(i in 1:dim(sub)[1]){
            if(!is.na(sub$description[i])){
              sub$description_repeated[i]<-nchar(sub$description[i])-nchar(gsub('([[:alpha:]])\\1+', '\\1', sub$description[i]))
            }
            if(i %% 100 == 0){
              print(i)
            }

          #### Positive or negative content using senti-word
          print("Estimating sentiment of last posted status...this may take a moment")
          sub$laststatus_sentiment<-unlist(mclapply(sub$laststatus_text_clean, function(x) mean(sentiment(x)$sentiment), mc.cores=10L))

          print("Estimating sentiment of profile description...this may take a moment")
          sub$description_sentiment<-unlist(mclapply(sub$description, function(x) mean(sentiment(x)$sentiment), mc.cores=10L))

          #### Assign topics to users
          print("Assigning topics to last posted status.. this may take a moment")
          source(paste0(workingDir, "ensemble_methods/frequently_used_functions/user_lda_topic.R"))
          results <- lda_topic_4_user(sub, 5, "laststatus_text", "id_str")
          names(results)[2]<-"laststatus_topic"

          colName <- names(results)[1]

          sub <- merge(sub, results, by=colName, all=TRUE)

          #### Assign topics to description field
          print("Assigning topics to last posted status...this may take a moment")
          results<-lda_topic_4_user(sub, 5, "description", "id_str")
          names(results)[2]<-"description_topic"

          colName <- names(results)[1]

          sub <- merge(sub, results, by=colName, all=TRUE)

          sub$year_created <- as.factor(sub$year_created)
          sub$geo_enabled <- as.factor(sub$geo_enabled)
          sub$profile_banner_url_binary <- as.factor(sub$profile_banner_url_binary)
          sub$description_hashtag_binary <- as.factor(sub$description_hashtag_binary)
          sub$profile_background_image_url_binary <- as.factor(sub$profile_background_image_url_binary)
          sub$location_binary <- as.factor(sub$location_binary)

          print(paste0("Predicting values for ", iter, "!"))
          model_predict_probs <- predict.gbm(model, sub, best.iter, type="response")
          model_predict <- apply(model_predict_probs[,,1], 1, which.max)
          model_predict[model_predict==1]<-"20 to 30"
          model_predict[model_predict==2]<-"Above 30"
          model_predict[model_predict==3]<-"Below 20"


          print(paste0("Predictions are done for chunk ", iter, "!"))

          results_part<-data.frame(id_str = as.character(sub$id_str), age_prediction=model_predict)

          write.csv(results_part, paste0(dir, "results_part_", iter, ".csv", row.names=FALSE))
        }
      }
    }
  }

  if(labeled==TRUE){

    df <- input
    print("Data are ready!")

    if(length(which(is.na(df$screen_name)))>0){
      df <- df[-which(is.na(df$screen_name)),]
    }

    ## Modifying activity counts
    df$statuses_count_log<-log(df$statuses_count)
    df$statuses_count_log[is.infinite(df$statuses_count_log)]<-0

    df$statuses_quantile<-df$statuses_count
    df$statuses_quantile[df$statuses_count>=0 & df$statuses_count<157]<-1
    df$statuses_quantile[df$statuses_count>=157 & df$statuses_count<1897]<-2
    df$statuses_quantile[df$statuses_count>=1897 & df$statuses_count<11632]<-3
    df$statuses_quantile[df$statuses_count>=11632 & df$statuses_count<=686052]<-4

    df$followers_count_log<-log(df$statuses_count)
    df$followers_count_log[is.infinite(df$followers_count_log)]<-0

    df$followers_quantile<-df$followers_count
    df$followers_quantile[df$followers_count>=0 & df$followers_count<29]<-1
    df$followers_quantile[df$followers_count>=29 & df$followers_count<159]<-2
    df$followers_quantile[df$followers_count>=159 & df$followers_count<557]<-3
    df$followers_quantile[df$followers_count>=557 & df$followers_count<=208437]<-4

    df$friends_count_log<-log(df$friends_count)
    df$friends_count_log[is.infinite(df$friends_count_log)]<-0

    df$friends_quantile<-df$friends_count
    df$friends_quantile[df$friends_count>=0 & df$friends_count<81]<-1
    df$friends_quantile[df$friends_count>=81 & df$friends_count<256]<-2
    df$friends_quantile[df$friends_count>=256 & df$friends_count<680]<-3
    df$friends_quantile[df$friends_count>=680 & df$friends_count<=101531]<-4

    df$favourites_count_log<-log(df$favourites_count)
    df$favourites_count_log[is.infinite(df$favourites_count_log)]<-0

    df$favourites_quantile<-df$favourites_count
    df$favourites_quantile[df$favourites_count>=0 & df$favourites_count<43]<-1
    df$favourites_quantile[df$favourites_count>=43 & df$favourites_count<807]<-2
    df$favourites_quantile[df$favourites_count>=807 & df$favourites_count<5775]<-3
    df$favourites_quantile[df$favourites_count>=5775 & df$favourites_count<=330554]<-4


    df$listed_count_log<-log(df$listed_count)
    df$listed_count_log[is.infinite(df$listed_count_log)]<-0

    #### Ratio of friends to followers (from PLOS paper)
    df$friend_follower<-df$friends_count/df$followers_count
    df$friend_follower[is.na(df$friend_follower)]<-0
    df$friend_follower[is.infinite(df$friend_follower)]<-df$friends_count[is.infinite(df$friend_follower)]


    #### Does the user share a url?
    df$url_binary<-df$url
    df$url_binary[is.na(df$url)]<-0
    df$url_binary[!is.na(df$url)]<-1


    #### Does the URL mention social media? (missing values marked as NA)
    df$url_expanded_sm_na<-NA

    for(i in 1:dim(df)[1]){
      if(!is.na(df$description[i])){
        df$url_expanded_sm_na[i]<-0
       }
   }
    df$url_expanded_sm_na[grep("insta", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm_na[grep("snap", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm_na[grep("youtube", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm_na[grep("facebook", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm_na[grep("linkedin", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm_na[grep("tumblr", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm_na[grep("wordpress", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm_na[grep("pinterest", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm_na[grep("curiouscat", df$url_expanded, ignore.case=TRUE)]<-1

    #### Does the URL mention social media? (missing values marked as 0)
    df$url_expanded_sm<-0
    df$url_expanded_sm[grep("insta", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm[grep("snap", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm[grep("youtube", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm[grep("facebook", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm[grep("linkedin", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm[grep("tumblr", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm[grep("wordpress", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm[grep("pinterest", df$url_expanded, ignore.case=TRUE)]<-1
    df$url_expanded_sm[grep("curiouscat", df$url_expanded, ignore.case=TRUE)]<-1


    #### Does the user provide a location?
    df$location_binary<-df$location
    df$location_binary[is.na(df$location)]<-0
    df$location_binary[!is.na(df$location)]<-1

    #### Does the user provide a banner URL?
    df$profile_banner_url_binary<-df$profile_banner_url
    df$profile_banner_url_binary[is.na(df$profile_banner_url)]<-0
    df$profile_banner_url_binary[!is.na(df$profile_banner_url)]<-1

    #### Does the user have a default profile image?
    df$profile_image_default<-0
    df$profile_image_default[grep("default", df$profile_image_url)]<-1

    #### Does the user use template profile colors?

    df$template_color<-0
    df$template_color[which(df$profile_link_color=="981CEB" |
                            df$profile_link_color=="F58EA8" |
                            df$profile_link_color=="E81C4F" |
                            df$profile_link_color=="1B95E0" |
                            df$profile_link_color=="91D2FA" |
                            df$profile_link_color=="19CF86" |
                            df$profile_link_color=="7FDBB6" |
                            df$profile_link_color=="FAB81E" |
                            df$profile_link_color=="FF691F" |
                            df$profile_link_color=="ABB8C2")] <-1



    #### How similar are the user's name and screen name?
    df$name_clean<-iconv(df$name, "UTF-8", "ASCII", sub="") # remove UTF-8 characters without destroying the entire name
    df$name_clean<-removePunctuation(df$name_clean) # remove punctuation
    df$name_clean<-tolower(stripWhitespace(as.character(df$name_clean))) #normalize, remove whitespace
    df$name_clean<-unlist(lapply(df$name_clean, function(x) gsub(" ", "", x)))

    df$screen_name_clean<-tolower(df$screen_name)

    df$similarity<-NA
        for(i in 1:dim(df)[1]){
          df$similarity[i]<-levenshteinSim(df$name_clean[i], df$screen_name_clean[i])
    }


     #### How many months has the user been active? How often do they tweet?
        df$days_active <- NA
        df$months_active <- NA
        df$avg_twt_per_mon <- NA
        df$year_created <- NA

        for (i in 1:dim(df)[1]){
          if(!is.na(df$laststatus_created_at[i])){
            df$year_created[i] <- tail(unlist(strsplit(df$created_at[i], " ")), 1)
            df$days_active[i]<-as.numeric(as.Date(dateConvert(df$laststatus_created_at[i]))-as.Date(dateConvert(df$created_at[i])))
            df$months_active[i]<-as.numeric(as.Date(dateConvert(df$laststatus_created_at[i]))-as.Date(dateConvert(df$created_at[i])))/30
            df$avg_twt_per_mon[i]<-as.numeric(df$statuses_count[i])/df$months_active[i]
          }
          if(i %% 100 == 0){
            print(i)
          }
        }


        df$avg_twt_per_mon[is.infinite(df$avg_twt_per_mon)]<-0
        df$avg_twt_per_mon[is.na(df$avg_twt_per_mon)]<-0


        df$avg_twt_per_mon_quant<-df$avg_twt_per_mon
        df$avg_twt_per_mon_quant[df$avg_twt_per_mon<15]<-1
        df$avg_twt_per_mon_quant[df$avg_twt_per_mon>=15 & df$avg_twt_per_mon<104]<-2
        df$avg_twt_per_mon_quant[df$avg_twt_per_mon>=104 & df$avg_twt_per_mon<388]<-3
        df$avg_twt_per_mon_quant[df$avg_twt_per_mon>=388]<-4


        df$avg_twt_per_mon_log<-log(df$avg_twt_per_mon)
        df$avg_twt_per_mon_log[is.infinite(df$avg_twt_per_mon_log)]<-0


        #### Favourites count, normalized for lifespan of the account
        df$favourites_normalized<-df$favourites_count/df$days_active

        #### Source: what is the user tweeting from?
        df$device<-unlist(lapply(df$laststatus_source, function(x) gsub("</a", "", unlist(strsplit(x, ">"))[2])))

        df$device[-which(df$device=="tweetDeck" |
                                df$device=="SocialToaster" |
                                df$device=="Lucktastic Android" |
                                df$device=="Facebook" |
                                df$device=="Instagram" |
                                df$device=="Twitter for iPad" |
                                df$device=="Twitter Lite" |
                                df$device=="Google" |
                                df$device=="Twitter Web Client" |
                                df$device=="Twitter for iPhone" |
                                df$device=="Twitter for Android") ] <-"Other"


        #### Source: Does the user tweet from an iPhone?
        df$iPhone<-0
        df$iPhone[which(df$device=="Twitter for iPhone")]<-1


        #### Does the profile include a background image?
        df$profile_background_image_url_binary<-df$profile_background_image_url
        df$profile_background_image_url_binary[is.na(df$profile_background_image_url)]<-0
        df$profile_background_image_url_binary[!is.na(df$profile_background_image_url)]<-1


        #### How many words are in the user's description?
        df$description_length_word<-unlist(lapply(df$description, function(x) length(unlist(strsplit(x, " ")))))


        #### How many characters are in the user's description?
        df$description_length_char<-unlist(lapply(df$description, function(x) length(unlist(strsplit(x, "")))))


        #### Presence of emojis in description
        #df$description_decode<-unlist(lapply(df$description, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode

        df$description_emoji <- NA
        df$name_emoji <- NA

        #for(i in 1:dim(df)[1]){
        #  df<-unlist(strsplit(df$description_decode[i], ""))
        #  df<-df[-grep(" ", df)]
        #  all<-length(df)
        #  alphanumeric<-length(grep("[[:digit:]]", df))+length(grep("[[:alpha:]]", df))
        #  df$description_emoji[i]<-(all-alphanumeric)/all
        #  print(i)
        #}

        for(i in 1:dim(df)[1]){
          vec <- unlist(strsplit(df$description[i], " "))
          vec <- vec[-which(vec=="")]
          sum1 <- which(unlist(lapply(vec, function(x) isUpperMethod1(trim(gsub('[[:digit:]]+', '', x)))))==TRUE) # unicode caps
          sum2 <- grep("<", vec)
          sum3 <- grep("[^[:alnum:]]", unlist(strsplit(vec, " ")))
          sum4 <- grep("#", unlist(strsplit(vec, " ")))
          sum5 <- grep("@", unlist(strsplit(vec, " ")))

          vec1 <- unique(c(sum1, sum2, sum3))
          vec2 <- unique(c(sum4, sum5))

          sum <- length(setdiff(vec1, vec2))
          df$description_emoji[i] <- sum
          if(i %% 100 == 0){
            print(i)
          }
        }


        #### Does the user's name include emojis
        for(i in 1:dim(df)[1]){
          vec <- unlist(strsplit(df$name[i], " "))
          vec <- vec[-which(vec=="")]
          sum1 <- which(unlist(lapply(vec, function(x) isUpperMethod1(trim(gsub('[[:digit:]]+', '', x)))))==TRUE) # unicode caps
          sum2 <- grep("<", vec)
          sum3 <- grep("[^[:alnum:]]", unlist(strsplit(vec, " ")))
          sum4 <- grep("#", unlist(strsplit(vec, " ")))
          sum5 <- grep("@", unlist(strsplit(vec, " ")))

          vec1 <- unique(c(sum1, sum2, sum3))
          vec2 <- unique(c(sum4, sum5))

          sum <- length(setdiff(vec1, vec2))
          df$name_emoji[i] <- sum
          if(i %% 100 == 0){
            print(i)
          }
        }


        #### Does the description include personal pronouns?
        df$description_pronouns<-0

        for(i in 1:dim(df)[1]){
          if(length(grep("i", unlist(strsplit(df$description[i], " ")), ignore.case=TRUE))>1){
            df$description_pronouns[i]<-1
          }
          if(length(grep("me", unlist(strsplit(df$description[i], " ")), ignore.case=TRUE))>1){
            df$description_pronouns[i]<-1
          }
          if(length(grep("my", unlist(strsplit(df$description[i], " ")), ignore.case=TRUE))>1){
            df$description_pronouns[i]<-1
          }
        }


        #### How many hashtags are in the description?
        df$description_hashtag_count<-NA

        for(i in 1:dim(df)[1]){
          if(!is.na(df$description[i])){
            df$description_hashtag_count[i]<-length(grep("#", unlist(strsplit(df$description[i], ""))))
          }
        }

        df$description_hashtag_binary<-df$description_hashtag_count
        df$description_hashtag_binary[df$description_hashtag_count>=1]<-1
        df$description_hashtag_binary[df$description_hashtag_count<1 & !is.na(df$description_hashtag_count)]<-0


        #### How many "@" are in the description?
        df$description_at_count <- unlist(lapply(df$description, function(x) length(grep("@", unlist(strsplit(x, ""))))))

        df$description_at_binary<-df$description_at_count
        df$description_at_binary[df$description_at_count>=1]<-1
        df$description_at_binary[df$description_at_count<1 & !is.na(df$description_at_count)]<-0


        #### Does the user mention of other links in description?
        df$description_web_count <- unlist(lapply(df$description, function(x) length(grep("http", x))))

        #### Does the description mention social media?
        df$description_sm<-0
        df$description_sm[grep("insta", df$description, ignore.case=TRUE)]<-1
        df$description_sm[grep("snap", df$description, ignore.case=TRUE)]<-1
        df$description_sm[grep("youtube", df$description, ignore.case=TRUE)]<-1
        df$description_sm[grep("facebook", df$description, ignore.case=TRUE)]<-1
        df$description_sm[grep("linkedin", df$description, ignore.case=TRUE)]<-1
        df$description_sm[grep("tumblr", df$description, ignore.case=TRUE)]<-1
        df$description_sm[grep("wordpress", df$description, ignore.case=TRUE)]<-1
        df$description_sm[grep("pinterest", df$description, ignore.case=TRUE)]<-1
        df$description_sm[grep("curiouscat", df$description, ignore.case=TRUE)]<-1


        #### Mention of mail in description
        df$description_mail_count <- unlist(lapply(df$description[i], function(x) length(grep("@.*.com", x))))

        #### Mention of pipe in description
        df$description_pipe_count <- length(grep("|", unlist(strsplit(df$description[i], "")), fixed=TRUE))


        df$description_pipe_binary<-df$description_pipe_count
        df$description_pipe_binary[df$description_pipe_count>=1]<-1
        df$description_pipe_binary[df$description_pipe_count<1 & !is.na(df$description_pipe_count)]<-0


        #### Mention of height in description
        df$description_height_count <- unlist(lapply(df$description, function(x) length(grep("[:digit:]'[:digit:]", x))))


        #### Does the description contain stopwords?
        df$description_stopword_count<-unlist(lapply(df$description, function(x) length(intersect(unlist(strsplit(x, " ")), stopwords("en")))))

        df$description_stopword_count_normalized<-NA
        for(i in 1:dim(df)[1]){
          df$description_stopword_count_normalized[i]<-df$description_stopword_count[i]/length(unlist(strsplit(df$description[i], " ")))
        }


        #### Does the last status include media?
        df$laststatus_media_binary<-df$laststatus_media
        df$laststatus_media_binary[which(!is.na(df$laststatus_text) & is.na(df$laststatus_media))]<-0
        df$laststatus_media_binary[which(!is.na(df$laststatus_media))]<-1


        #### Does the last status include a URL?
        df$laststatus_url_binary<-df$laststatus_urls
        df$laststatus_url_binary[which(!is.na(df$laststatus_text) & is.na(df$laststatus_urls))]<-0
        df$laststatus_url_binary[which(!is.na(df$laststatus_urls))]<-1


        #### Prepare laststatus for feature building
        df$laststatus_text_clean<-trim(df$laststatus_text)
        df$laststatus_text_clean<-unlist(lapply(df$laststatus_text_clean, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
        df$laststatus_text_clean<-unlist(lapply(df$laststatus_text_clean, function(x) gsub("RT ", "", x))) #these two lines remove unicode

        df$laststatus_text_clean<-as.character(df$laststatus_text_clean)

        df$description<-trim(df$description)
        df$description<-unlist(lapply(df$description, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
        df$description<-unlist(lapply(df$description, function(x) gsub("RT ", "", x)))

        df$description<-as.character(df$description)

        #### Number of characters in laststatus
        df$laststatus_length <- unlist(lapply(df$laststatus_text_clean, function(x) nchar(x)))


        #### Number of characters in description
        df$description_length <- unlist(lapply(df$description, function(x) nchar(x)))


        df$description_length[is.na(df$description_length)]<-0

        #### Count upper case letters
        df$laststatus_upper<-sapply(regmatches(df$laststatus_text, gregexpr("[A-Z]", df$laststatus_text, perl=TRUE)), length)
        df$description_upper<-sapply(regmatches(df$description, gregexpr("[A-Z]", df$description, perl=TRUE)), length)


        #### Count all digits
        df$laststatus_digit<-sapply(regmatches(df$laststatus_text_clean, gregexpr("[0-9]", df$laststatus_text_clean, perl=TRUE)), length)
        df$description_digit<-sapply(regmatches(df$description, gregexpr("[0-9]", df$description, perl=TRUE)), length)


        #### Count spaces
        df$laststatus_space<-unlist(lapply(df$laststatus_text_clean, function(x) countSpaces(x)))
        df$description_space<-unlist(lapply(df$description, function(x) countSpaces(x)))


        #### Standard deviation of word length
        df$laststatus_avg_word_length<-NA
        df$laststatus_sd_word_length<-NA
        df$laststatus_min_word_length<-NA
        df$laststatus_max_word_length<-NA


        for(i in 1:length(df$laststatus_text_clean)){
          if(!is.na(df$laststatus_text_clean[i]) & df$laststatus_text_clean[i]!=""){
            vec<-gsub('[[:punct:] ]+',' ', df$laststatus_text_clean[i])
            vec<-unlist(strsplit(vec, " "))
            if(length(grep("RT", vec, ignore.case=FALSE))>=1){
              vec<-vec[-grep("RT", vec, ignore.case=FALSE)]
            }
            if(length(grep("@.*", vec, ignore.case=FALSE))>=1){
              vec<-vec[-grep("@.*", vec, ignore.case=FALSE)]
            }
            vals<-NULL
            for(j in 1:length(vec)){
              val<-nchar(vec[j])
              vals<-c(vals, val)
            }
            df$laststatus_avg_word_length[i]<-mean(vals, na.rm=TRUE)
            df$laststatus_sd_word_length[i]<-sd(vals, na.rm=TRUE)
            df$laststatus_min_word_length[i]<-min(vals, na.rm=TRUE)
            df$laststatus_max_word_length[i]<-max(vals, na.rm=TRUE)
          }
          if(i %% 100 == 0){
            print(i)
          }
        }


        df$description_avg_word_length<-NA
        df$description_sd_word_length<-NA
        df$description_min_word_length<-NA
        df$description_max_word_length<-NA


        for(i in 1:length(df$description)){
          if(!is.na(df$description[i]) & df$description[i]!=""){
            vec<-gsub('[[:punct:] ]+',' ', df$description[i])
            vec<-unlist(strsplit(vec, " "))
            if(length(grep("RT", vec, ignore.case=FALSE))>=1){
              vec<-vec[-grep("RT", vec, ignore.case=FALSE)]
            }
            if(length(grep("@.*", vec, ignore.case=FALSE))>=1){
              vec<-vec[-grep("@.*", vec, ignore.case=FALSE)]
            }
            vals<-NULL
            for(j in 1:length(vec)){
              val<-nchar(vec[j])
              vals<-c(vals, val)
            }
            df$description_avg_word_length[i]<-mean(vals, na.rm=TRUE)
            df$description_sd_word_length[i]<-sd(vals, na.rm=TRUE)
            df$description_min_word_length[i]<-min(vals, na.rm=TRUE)
            df$description_max_word_length[i]<-max(vals, na.rm=TRUE)
          }
          if(i %% 100 == 0){
            print(i)
          }
        }

        ## Slang terms in the laststatus
        df$laststatus_slang<-0
        for(i in 1:length(slang_terms)){
          idx<-grep(slang_terms[i], df$laststatus_text, ignore.case=TRUE)
          df$laststatus_slang[idx]<-1
        }

        df$laststatus_slang[which(is.na(df$laststatus_text_clean))]<-NA


        ## Punctuation
        df$laststatus_punct <- NA

        for(i in 1:length(df$laststatus_text)){
          vec <- unlist(strsplit(df$laststatus_text[i], ""))
          val1 <- which(vec == ".")
          val2 <- which(vec == ",")
          val3 <- which(vec == "!")
          val4 <- which(vec == "?")
          val5 <- which(vec == ":")
          val6 <- which(vec == ";")
          val7 <- which(vec == "'")
          val7 <- which(vec == "\"")
          len <- length(unique(c(val1,val2, val3,val4,val5,val6,val7)))
          df$laststatus_punct[i] <- len
          if(i %% 100 == 0){
            print(i)
          }
        }

        df$description_punct <- NA

        for(i in 1:length(df$description)){
          vec<-unlist(strsplit(df$description[i], ""))
          val1<-which(vec == ".")
          val2<-which(vec == ",")
          val3<-which(vec == "!")
          val4<-which(vec == "?")
          val5<-which(vec == ":")
          val6<-which(vec == ";")
          val7<-which(vec == "'")
          val7<-which(vec == "\"")
          len<-length(unique(c(val1,val2, val3,val4,val5,val6,val7)))
          df$description_punct[i]<-len
          if(i %% 100 == 0){
            print(i)
          }
        }



        ## number of special characters
        df$laststatus_char <- NA

        for(i in 1:length(df$laststatus_text_clean)){
          vec <- unlist(strsplit(df$laststatus_text_clean[i], ""))
          val1 <- which(vec == "@")
          val2 <- which(vec == "#")
          val3 <- which(vec == "$")
          val4 <- which(vec == "%")
          val5 <- which(vec == "&")
          val6 <- which(vec == "*")
          val7 <- which(vec == "~")
          val8 <- which(vec == "^")
          val9 <- which(vec == "-")
          val10 <- which(vec == "=")
          val11 <- which(vec == "+")
          val12 <- which(vec == ">")
          val13 <- which(vec == "<")
          val14 <- which(vec == "[")
          val15 <- which(vec == "]")
          val16 <- which(vec == "{")
          val17 <- which(vec == "}")
          val18 <- which(vec == "|")
          val19 <- which(vec == "\\")
          val20 <- which(vec == "/")
          len <- length(unique(c(val1,val2, val3,val4,val5,val6,val7,val8,val9,val10,val11,val12,val13,val14,val15,val16,val17,val18,val19,val20)))
          df$laststatus_char[i] <- len
          if(i %% 100 == 0){
            print(i)
          }
        }

        df$description_char <- NA

        for(i in 1:length(df$description)){
          vec <- unlist(strsplit(df$description[i], ""))
          val1 <- which(vec == "@")
          val2 <- which(vec == "#")
          val3 <- which(vec == "$")
          val4 <- which(vec == "%")
          val5 <- which(vec == "&")
          val6 <- which(vec == "*")
          val7 <- which(vec == "~")
          val8 <- which(vec == "^")
          val9 <- which(vec == "-")
          val10 <- which(vec == "=")
          val11 <- which(vec == "+")
          val12 <- which(vec == ">")
          val13 <- which(vec == "<")
          val14 <- which(vec == "[")
          val15 <- which(vec == "]")
          val16 <- which(vec == "{")
          val17 <- which(vec == "}")
          val18 <- which(vec == "|")
          val19 <- which(vec == "\\")
          val20 <- which(vec == "/")
          len <- length(unique(c(val1,val2, val3,val4,val5,val6,val7,val8,val9,val10,val11,val12,val13,val14,val15,val16,val17,val18,val19,val20)))
          df$description_char[i]<-len
          if(i %% 100 == 0){
            print(i)
          }
        }


        #### length of description
        df$description_length <- unlist(lapply(df$description, function(x) length(unlist(strsplit(x, " ")))))


        #### Number of emoticons
        df$laststatus_emoticon_count <- 0
        df$laststatus_emoticon_count <- unlist(lapply(df$laststatus_text_clean, function(x) find_emoticon(x)))

        df$description_emoticon_count <- 0
        df$description_emoticon_count <- unlist(lapply(df$description, function(x) find_emoticon(x)))


        #### Use of profanity
        df$laststatus_profane <- NA

        for (i in 1:length(df$laststatus_text_clean)){
          if(!is.na(df$laststatus_text_clean[i])){
            vec<-unlist(strsplit(df$laststatus_text_clean[i], " "))
            val1<-grep("shit", vec, ignore.case=TRUE)
            val2<-grep("fuck", vec, ignore.case=TRUE)
            val3<-grep("ass", vec, ignore.case=TRUE)
            val4<-grep("piss", vec, ignore.case=TRUE)
            val5<-grep("bitch", vec, ignore.case=TRUE)
            val6<-grep("damn", vec, ignore.case=TRUE)
            df$laststatus_profane[i]<-length(unique(c(val1, val2, val3, val4, val5, val6)))
          }
        }


        df$description_profane <- NA

        for (i in 1:length(df$description)){
          if(!is.na(df$description[i])){
            vec<-unlist(strsplit(df$description[i], " "))
            val1<-grep("shit", vec, ignore.case=TRUE)
            val2<-grep("fuck", vec, ignore.case=TRUE)
            val3<-grep("ass", vec, ignore.case=TRUE)
            val4<-grep("piss", vec, ignore.case=TRUE)
            val5<-grep("bitch", vec, ignore.case=TRUE)
            val6<-grep("damn", vec, ignore.case=TRUE)
            df$description_profane[i]<-length(unique(c(val1, val2, val3, val4, val5, val6)))
          }
        }


        #### Vocabularly richness (number of unique words per tweet - minus stop words)
        df$laststatus_vocab_richness <- NA

        for (i in 1:length(df$laststatus_text_clean)){
          if(!is.na(df$laststatus_text_clean[i])){
            newVec<-removeWords(df$laststatus_text_clean[i], stopwords("en"))
            newVec<-removePunctuation(newVec)
            newVec<-trim(newVec)
            len1<-length(unlist(strsplit(newVec, " ")))
            len2<-length(unique(unlist(strsplit(newVec, " "))))
            df$laststatus_vocab_richness[i]<-len2/len1
          }
        }

        df$description_vocab_richness<-NA

        for (i in 1:length(df$description)){
          if(!is.na(df$description[i])){
            newVec<-removeWords(df$description[i], stopwords("en"))
            newVec<-removePunctuation(newVec)
            newVec<-trim(newVec)
            len1<-length(unlist(strsplit(newVec, " ")))
            len2<-length(unique(unlist(strsplit(newVec, " "))))
            df$description_vocab_richness[i]<-len2/len1
          }
        }

        #### User retweets someone else
        df$laststatus_retweet <- 0
        df$laststatus_retweet[grep("RT ", df$laststatus_text, ignore.case=FALSE)]<-1

        #### User tweets at someone else
        df$laststatus_at <- 0
        df$laststatus_at[grep("(^|[^@\\w])@(\\w{1,15})\\b", df$laststatus_text, ignore.case=FALSE)]<-1
        df$laststatus_at[grep("RT ", df$laststatus_text, ignore.case=FALSE)]<-0

        ## Laststatus retweet count
        ## use as is

        ## Laststatus favorite
        ## use as is

        #### Laststatus media binary
        df$laststatus_media_binary <- 0
        df$laststatus_media_binary[!is.na(df$laststatus_media)] <- 1

        #### Laststatus geo binary
        df$laststatus_geo_binary <- 0
        df$laststatus_geo_binary[!is.na(df$laststatus_geo)] <- 1

        #### Laststatus term repetition (i.e. okaaaaaaay)
        df$laststatus_repeated <- NA

        for(i in 1:dim(df)[1]){
          if(!is.na(df$laststatus_text[i])){
            df$laststatus_repeated[i] <- nchar(df$laststatus_text_clean[i])-nchar(gsub('([[:alpha:]])\\1+', '\\1', df$laststatus_text_clean[i]))
          }
          if(i %% 100 == 0){
            print(i)
          }
        }


        ### Description term repetition
        df$description_repeated<-NA

        for(i in 1:dim(df)[1]){
          if(!is.na(df$description[i])){
            df$description_repeated[i]<-nchar(df$description[i])-nchar(gsub('([[:alpha:]])\\1+', '\\1', df$description[i]))
          }
          if(i %% 100 == 0){
            print(i)
          }
        }

        ## Positive or negative content using senti-word
    print("Estimating sentiment of last posted status...this may take a moment")
    df$laststatus_sentiment<-unlist(mclapply(df$laststatus_text_clean, function(x) mean(sentiment(x)$sentiment), mc.cores=10L))

    print("Estimating sentiment of profile description...this may take a moment")
    df$description_sentiment<-unlist(mclapply(df$description, function(x) mean(sentiment(x)$sentiment), mc.cores=10L))

    ## Assign topics to users
    print("Assigning topics to last posted status...this may take a moment")
    df$screen_name<-df$screen_name
    df$laststatus_text<-df$laststatus_text

    results <- lda_topic_4_user(df, 5, "laststatus_text", "id_str")
    names(results)[2]<-"laststatus_topic"

    colName <- names(results)[1]

    df <- merge(df, results, by=colName, all=TRUE)

    ## Assign topics to description field
    print("Assigning topics to last posted status...this may take a moment")
    results<-lda_topic_4_user(df, 5, "description", "id_str")
    names(results)[2]<-"description_topic"

    colName <- names(results)[1]

    df <- merge(df, results, by=colName, all=TRUE)

    df$year_created <- as.factor(df$year_created)
    df$geo_enabled <- as.factor(df$geo_enabled)
    df$profile_banner_url_binary <- as.factor(df$profile_banner_url_binary)
    df$description_hashtag_binary <- as.factor(df$description_hashtag_binary)
    df$profile_background_image_url_binary <- as.factor(df$profile_background_image_url_binary)
    df$location_binary <- as.factor(df$location_binary)

    train <- df[,]
    test <- df[-tindex,]

    print("Modeling results")
    model <- gbm(age_cat_6~year_created+
                       description_emoji+
                       name_emoji+
                       description_length_word+
                       similarity+
                       template_color+
                       profile_image_default+
                       profile_banner_url_binary+
                       location_binary+
                       description_sm+
                       description_pronouns+
                       friends_quantile+
                       followers_quantile+
                       favourites_normalized+
                       friend_follower+
                       listed_count+
                       iPhone+
                       description_hashtag_binary+
                       description_at_binary+
                       description_web_count+
                       description_mail_count+
                       description_pipe_binary+
                       avg_twt_per_mon_quant+
                       profile_background_image_url_binary+
                       geo_enabled+
                       description_stopword_count_normalized+
                       description_upper+
                       description_digit+
                       description_space+
                       description_avg_word_length+
                       description_sd_word_length+
                       description_min_word_length+
                       description_max_word_length+
                       description_punct+
                       description_char+
                       description_emoticon_count+
                       description_profane+
                       description_vocab_richness+
                       description_repeated+
                       description_topic+
                       laststatus_length+
                       laststatus_upper+
                       laststatus_digit+
                       laststatus_space+
                       laststatus_avg_word_length+
                       laststatus_sd_word_length+
                       laststatus_min_word_length+
                       laststatus_max_word_length+
                       laststatus_punct+
                       laststatus_char+
                       laststatus_emoticon_count+
                       laststatus_profane+
                       laststatus_vocab_richness+
                       laststatus_sentiment+
                       laststatus_retweet+
                       laststatus_at+
                       laststatus_retweet_count+
                       laststatus_favorite_count+
                       laststatus_media_binary+
                       laststatus_slang+
                       laststatus_geo_binary+
                       laststatus_repeated+
                       laststatus_topic, data=train, distribution="multinomial", n.trees = 5000, interaction.depth = 4, n.cores=2)

    print("Classifier is trained!")

    best.iter <- gbm.perf(model)

    print("Predicting values")
    model_predict_probs <- predict.gbm(model, sub, best.iter, type="response")
    model_predict <- apply(model_predict_probs[,,1], 1, which.max)
    model_predict[model_predict==1]<-"20 to 30"
    model_predict[model_predict==2]<-"Above 30"
    model_predict[model_predict==3]<-"Below 20"

    print("Predictions are made!")

    train$age_prediction <- NA
    train$test_train <- "train"

    test$age_prediction <- model_predict
    test$test_train <- "test"

    results <- rbind(train, test)
    return(results)
  }
}
