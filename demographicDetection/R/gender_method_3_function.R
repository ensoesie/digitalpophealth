#' Predict gender using structural name features 
#'
#' This function inputs a data frame containing user metadata, builds features based on the linguistic structure of the users' names, and employs a decision tree classifier to predict gender.
#' @param input A data frame containing Twitter profile information. Created using the function get_user_info.
#' @param labeled Set to TRUE if you have labeled data and want to train/test the algorithm on your data. Defautls to FALSE.
#' @param set.seed If labeled==TRUE, enter a seed to replicate creation of test/training data. Defaults to NULL.
#' @param training_set If labeled==TRUE, what proportion of your data will be your training set?
#' @param chunk If TRUE, features and predictions are generated for 10,000 user chunks. Recommended for unlabeled data with >10,000 users.
#' @param action If chunk==TRUE, whether the data should be appended and returned as one file, or saved as chunks. Options are append and save.
#' @param dir Where should output files be saved? Used only if action==save. Defaults to NULL
#' @keywords twitter, gender, demography
#' @return A two-column data frame containing Twitter user IDs and gender predictions.
#' @export
#' @examples 
#' ## df_list from function get_user_info
#' ## df <- do.call("rbind", df_list)
#' #' gender_method_3(df, labeled=FALSE, set.seed=NULL, training_set=0.80, chunk=TRUE, action="save", dir="/home/User/workingDir")
#' @seealso Prediction method is based on: Mueller, J. and Stumme, G. 2016. Gender Inference using Statistical Name Characteristics in Twitter. Proceedings of the The 3rd Multidisciplinary International Social Networks Conference on SocialInformatics, Albany, NY, 2016, 1-8.



gender_method_3  <-  function(input, labeled=FALSE, set.seed=NULL, training_set=0.80, chunk=FALSE, action="append", dir=NULL){
  Sys.setlocale("LC_ALL", "C")

  
  if(tail(unlist(strsplit(dir, "")), 1) !="/"){
    dir <- paste0(dir, "/")
  }
  
  if(is.null(input$name)){
    stop("Your data is missing relevant field(s)")
  }

  if(labeled == FALSE){

    load("genderMethod3_mod3.rda")
    print("Classifier is trained!")

    if(chunk==FALSE){

      index1 <- which(colnames(input)=="id_str")
      index2 <- which(colnames(input)=="name")
      
      gender_new  <-  rep(NA, dim(input)[1])
      input  <-  as.data.frame(cbind(input[,c(index1)], gender_new, input[,c(index2)]))
      names(input)  <-  c("id_str", "gender_new", "name")
      print("Test data are ready!")

      input$name_clean <- unlist(lapply(input$name, function(x) convert_names(x)))

      input$end_char <- unlist(lapply(input$name_clean, function(x) end_char(x)))
      
      for(i in 1:dim(input)[1]){
        if(!is.na(input$name_clean[i])){
          input$syllables[i] <- english_syllable_count(input$name_clean[i])
        }
        else{
          input$syllables[i] <- NA
        }
      }
      
      input$vowel_count <- unlist(lapply(input$name_clean, function(x) vowel_count(x)))
      
      input$consonant_count <- unlist(lapply(input$name_clean, function(x) consonant_count(x)))
      
      input$bouba_c <- unlist(lapply(input$name_clean, function(x) length(c(grep("b", unlist(strsplit(x, ""))), grep("l", unlist(strsplit(x, ""))),grep("n", unlist(strsplit(x, ""))),grep("m", unlist(strsplit(x, "")))))))
      
      input$bouba_v <- unlist(lapply(input$name_clean, function(x) length(c(grep("o", unlist(strsplit(x, ""))), grep("u", unlist(strsplit(x, "")))))))
      
      input$kiki_c <- unlist(lapply(input$name_clean, function(x) length(c(grep("k", unlist(strsplit(x, ""))), grep("p", unlist(strsplit(x, ""))),grep("t", unlist(strsplit(x, "")))))))
       
      input$kiki_v <- unlist(lapply(input$name_clean, function(x) length(c(grep("i", unlist(strsplit(x, ""))), grep("e", unlist(strsplit(x, "")))))))
      print("Features are built!")

      predictions  <-  predict(mod3, input)$class
      print("Predictions are made!")
       
      results <- as.data.frame(cbind(test, as.character(predictions)))
      names(results)[dim(results)[2]]<-"gender_prediction"
    
      return(results)

    }

    if(chunk==TRUE & action=="append"){
      
      index1 <- which(colnames(input)=="id_str")
      index2 <- which(colnames(input)=="name")
      
      gender_new  <-  rep(NA, dim(input)[1])
      input  <-  as.data.frame(cbind(input[,c(index1)], gender_new, input[,c(index2)]))
      names(input)  <-  c("id_str", "gender_new", "name")
      print("Test data are ready!")

      vec <- seq(1, nrow(input), by=10000)
      iter <- 0

      results <- NULL

      for(i in vec){

        iter <- iter+1

        sub <- input[(i:(i+9999)),]

        if(length(which(is.na(sub$name)))>0){
          sub <- sub[-which(is.na(sub$name)),]
        }

        sub$name <- as.character(sub$name)
        sub$name_clean <- unlist(lapply(sub$name, function(x) convert_names(x)))

        sub$end_char <- unlist(lapply(sub$name_clean, function(x) end_char(x)))
        
        for(i in 1:dim(sub)[1]){
          if(!is.na(sub$name_clean[i])){
            sub$syllables[i] <- english_syllable_count(sub$name_clean[i])
          }
          else{
            sub$syllables[i] <- NA
          }
        }
        
        sub$vowel_count <- unlist(lapply(sub$name_clean, function(x) vowel_count(x)))
        sub$consonant_count <- unlist(lapply(sub$name_clean, function(x) consonant_count(x)))
        sub$bouba_c <- unlist(lapply(sub$name_clean, function(x) length(c(grep("b", unlist(strsplit(x, ""))), grep("l", unlist(strsplit(x, ""))),grep("n", unlist(strsplit(x, ""))),grep("m", unlist(strsplit(x, "")))))))
        sub$bouba_v <- unlist(lapply(sub$name_clean, function(x) length(c(grep("o", unlist(strsplit(x, ""))), grep("u", unlist(strsplit(x, ""))))))) 
        sub$kiki_c <- unlist(lapply(sub$name_clean, function(x) length(c(grep("k", unlist(strsplit(x, ""))), grep("p", unlist(strsplit(x, ""))),grep("t", unlist(strsplit(x, "")))))))
        sub$kiki_v <- unlist(lapply(sub$name_clean, function(x) length(c(grep("i", unlist(strsplit(x, ""))), grep("e", unlist(strsplit(x, ""))))))) 
        print(paste0("Features for chunk ", iter , " are made!"))

        predictions <- predict(mod3, sub)$class

        print(paste0("Predictions for chunk ", iter , " are made!"))

        predictions<-as.character(predictions)
                    
        results_part <- data.frame(id_str=as.character(sub$id_str), gender_prediction=predictions)

        results <- rbind(results, results_part)
      }
      return(results)
    }

   if(chunk==TRUE & action=="save"){
      
      index1 <- which(colnames(input)=="id_str")
      index2 <- which(colnames(input)=="name")
      
      gender_new  <-  rep(NA, dim(input)[1])
      input  <-  as.data.frame(cbind(input[,c(index1)], gender_new, input[,c(index2)]))
      names(input)  <-  c("id_str", "gender_new", "name")
      print("Test data are ready!")

      vec <- seq(1, nrow(input), by=10000)

      results <- NULL
      iter <- 0

      for(i in vec){

        iter <- iter+1

        sub <- input[(i:(i+9999)),]

        if(length(which(is.na(sub$name)))>0){
          sub <- sub[-which(is.na(sub$name)),]
        }

        sub$name <- as.character(sub$name)
        sub$name_clean <- unlist(lapply(sub$name, function(x) convert_names(x)))

        sub$end_char <- unlist(lapply(sub$name_clean, function(x) end_char(x)))
        
        for(i in 1:dim(sub)[1]){
          if(!is.na(sub$name_clean[i])){
            sub$syllables[i] <- english_syllable_count(sub$name_clean[i])
          }
          else{
            sub$syllables[i] <- NA
          }
        }
        
        sub$vowel_count <- unlist(lapply(sub$name_clean, function(x) vowel_count(x)))
        sub$consonant_count <- unlist(lapply(sub$name_clean, function(x) consonant_count(x)))
        sub$bouba_c <- unlist(lapply(sub$name_clean, function(x) length(c(grep("b", unlist(strsplit(x, ""))), grep("l", unlist(strsplit(x, ""))),grep("n", unlist(strsplit(x, ""))),grep("m", unlist(strsplit(x, "")))))))
        sub$bouba_v <- unlist(lapply(sub$name_clean, function(x) length(c(grep("o", unlist(strsplit(x, ""))), grep("u", unlist(strsplit(x, ""))))))) 
        sub$kiki_c <- unlist(lapply(sub$name_clean, function(x) length(c(grep("k", unlist(strsplit(x, ""))), grep("p", unlist(strsplit(x, ""))),grep("t", unlist(strsplit(x, "")))))))
        sub$kiki_v <- unlist(lapply(sub$name_clean, function(x) length(c(grep("i", unlist(strsplit(x, ""))), grep("e", unlist(strsplit(x, ""))))))) 
        print(paste0("Features for chunk ", iter , " are made!"))

        predictions <- predict(mod3, sub)$class

        print(paste0("Predictions for chunk ", iter , " are made!"))

        predictions<-as.character(predictions)
                    
        results_part <- data.frame(id_str=as.character(sub$id_str), gender_prediction=predictions)

        write.csv(results_part, paste0(dir, "results_part_", iter, ".csv"), row.names=FALSE)
      }
    }
  }
    
  if(labeled == TRUE){

    df <- input
    df$name <- as.character(df$name)
    df$name_clean <- unlist(lapply(df$name, function(x) convert_names(x)))

    df$end_char <- unlist(lapply(df$name_clean, function(x) end_char(x)))
        
    for(i in 1:dim(df)[1]){
      if(!is.na(df$name_clean[i])){
        df$syllables[i] <- english_syllable_count(df$name_clean[i])
      }
      else{
        df$syllables[i] <- NA
       }
    }
        
    df$vowel_count <- unlist(lapply(df$name_clean, function(x) vowel_count(x)))
    df$consonant_count <- unlist(lapply(df$name_clean, function(x) consonant_count(x)))
    df$bouba_c <- unlist(lapply(df$name_clean, function(x) length(c(grep("b", unlist(strsplit(x, ""))), grep("l", unlist(strsplit(x, ""))),grep("n", unlist(strsplit(x, ""))),grep("m", unlist(strsplit(x, "")))))))
    df$bouba_v <- unlist(lapply(df$name_clean, function(x) length(c(grep("o", unlist(strsplit(x, ""))), grep("u", unlist(strsplit(x, ""))))))) 
    df$kiki_c <- unlist(lapply(df$name_clean, function(x) length(c(grep("k", unlist(strsplit(x, ""))), grep("p", unlist(strsplit(x, ""))),grep("t", unlist(strsplit(x, "")))))))
    df$kiki_v <- unlist(lapply(df$name_clean, function(x) length(c(grep("i", unlist(strsplit(x, ""))), grep("e", unlist(strsplit(x, ""))))))) 

    print("Data are ready!")
        
    set.seed(seed)
    ntrain <- round(dim(df)[1]*as.numeric(training_set))
    tindex <- sample(1:dim(df)[1],ntrain) 
      
    train<-df[tindex,]
    test<-df[-tindex,]

    mod3 <- Coremodel(as.factor(gender_new) ~ syllables+vowel_count+consonant_count+end_char+bouba_c+bouba_v+kiki_c+kiki_v, data=train, model="tree")
    
    print("Classifier is trained!")
      
    predictions  <-  predict(mod3, test)$class
    predictions <- as.character(predictions)

    print("Predictions are made!")
      
    train$gender_prediction <- NA
    train$test_train <- "train"
      
    test$gender_prediction <- predictions
    test$test_train <- "test"
      
    results <- rbind(train, test)
    return(results)
    }
}


