#' Predict gender with word and character unigrams
#'
#' This function inputs a data frame containing user metadata, builds features containing word and character unigrams from users' names, and employs an SVM classifier to predict gender.
#' @param input A data frame containing Twitter profile information. Created using the function get_user_info.
#' @param labeled Set to TRUE if you have labeled data and want to train/test the algorithm on your data. Defautls to FALSE.
#' @param set.seed If labeled==TRUE, enter a seed to replicate creation of test/training data. Defaults to NULL.
#' @param training_set If labeled==TRUE, what proportion of your data will be your training set?
#' @param chunk If TRUE, features and predictions are generated for 10,000 user chunks. Recommended for unlabeled data with >10,000 users.
#' @param action If chunk==TRUE, whether the data should be appended and returned as one file, or saved as chunks. Options are append and save.
#' @param dir Where should output files be saved? Used only if action==save. Defaults to NULL
#' @keywords twitter, gender, demography
#' @return results A two-column data frame containing Twitter user IDs and gender predictions.
#' @export
#' @examples
#' ## df_list from function get_user_info
#' ## df <- do.call("rbind", df_list)
#' gender_method_2(df, labeled=FALSE, set.seed=NULL, training_set=0.80, chunk=TRUE, action="save", dir="/home/User/workingDir")
#' @seealso Prediction method is based on: Burger, J.D. et al. 2011. Discriminating gender on Twitter. Proceedings of the Conference on Empirical Methods in Natural Language Processing (2011), 1301-1309.




gender_method_2 <- function(input, labeled=FALSE, set.seed=NULL, training_set=0.80, chunk=FALSE, action="append", dir=NULL){

  if(is.null(input$name) | is.null(input$id_str)){
    stop("Your input data is missing relevant field(s)")
  }

  if(tail(unlist(strsplit(dir, "")), 1) !="/"){
     dir <- paste0(dir, "/")
  }

  if(labeled == FALSE){

    #load("/DATA/genderMethod2_mod.rda")
    print("Classifier is trained!")

    if(chunk == FALSE){

      #load("/DATA/gender_training_premade.rda")
      df <- df_gen

      index1 <- which(colnames(input)=="id_str")
      index2 <- which(colnames(input)=="name")

      gender_new  <-  rep(NA, dim(input)[1])
      input  <-  as.data.frame(cbind(input[,c(index1)], gender_new, input[,c(index2)]))
      names(input)  <-  c("id_str", "gender_new", "name")

      input <- rbind(df, input)

      ## Start building features

      input$name <- as.character(input$name)
      input$name <- unlist(lapply(input$name, function(x) iconv(x,"UTF-8","ASCII")))
      input$name <- unlist(lapply(input$name, function(x) gsub("<", " <", x)))
      input$name <- unlist(lapply(input$name, function(x) gsub(">", "> ", x)))
      input$name <- unlist(lapply(input$name, function(x) str_replace_all(x,"\\<U[^\\>]*\\>","")))
      input$name<-str_replace_all(input$name, "[[:punct:]]", "")
      input$name<-trim(tolower(input$name))

      print("Data are ready!")

      ## Feature 1: making user name n-gram features

      name_gram<-unlist(lapply(input$name, function(x) strsplit(gsub("[^[:alnum:] ]", "", x), " +")[[1]]))

      dat<-as.data.frame(table(name_gram))
      dat<-dat[which(dat$Freq>2),]
      colNames<-as.character(dat$name_gram)
      colNames<-colNames[-which(colNames=="")]
      colNames<-unlist(lapply(colNames, function(x) paste(x, "nameGram", sep="_")))

      input[,c(colNames)]<-0

      for(i in (dim(input)[2]-(length(colNames)-1)):dim(input)[2]){
      word<-colnames(input)[i]
        word<-gsub("_nameGram", "", word)
        input[grep(word, input$name, ignore.case = TRUE), i]<-1
      }

      ### Feature 2: making user name n-char features

      name_char<-unlist(lapply(input$name, function(x) gsub(" ", "", x)))
      name_char<-unlist(lapply(name_char, function(x) unlist(strsplit(x, ""))))


      dat<-as.data.frame(table(name_char))
      dat<-dat[which(dat$Freq>2),]
      colNames<-as.character(dat$name_char)
      colNames<-unlist(lapply(colNames, function(x) paste(x, "nameChar", sep="_")))

      input[,c(colNames)]<-0

      for(i in (dim(input)[2]-(length(colNames)-1)):dim(input)[2]){
        word<-colnames(input)[i]
        word<-gsub("_nameChar", "", word)
        input[grep(word, input$name, fixed=TRUE), i]<-1
      }

      print("Features are built!")

      ## Generate predictions
      input_part <- input[which(is.na(input$gender_new)),]
      predictions <- predict(mod, input_part)
      predictions<-as.character(predictions)

      results <- data.frame(id_str=input_part$id_str, gender_prediction =as.character(predictions))

      return(results)
      print("Predictions are made!")

    }

    if(chunk == TRUE & action=="append"){

      index1 <- which(colnames(input)=="id_str")
      index2 <- which(colnames(input)=="name")

      gender_new  <-  rep(NA, dim(input)[1])
      input  <-  as.data.frame(cbind(input[,c(index1)], gender_new, input[,c(index2)]))
      names(input)  <-  c("id_str", "gender_new", "name")

      vec <- seq(1, nrow(input), by=10000)

      results <- NULL
      iter <- 0

      for(i in vec){

        iter <- iter+1

        sub <- input[c(i:(i+9999)),]

        if(length(which(is.na(sub$name)))>0){
          sub <- sub[-which(is.na(sub$name)),]
        }

        sub$name <- as.character(sub$name)

        #load("/DATA/gender_training_premade.rda")
        df <- df_gen

        sub <- rbind(df, sub)

        sub$name <- as.character(sub$name)
        sub$name <- unlist(lapply(sub$name, function(x) iconv(x,"UTF-8","ASCII")))
        sub$name <- unlist(lapply(sub$name, function(x) gsub("<", " <", x)))
        sub$name <- unlist(lapply(sub$name, function(x) gsub(">", "> ", x)))
        sub$name <- unlist(lapply(sub$name, function(x) str_replace_all(x,"\\<U[^\\>]*\\>","")))
        sub$name<-str_replace_all(sub$name, "[[:punct:]]", "")
        sub$name<-trim(tolower(sub$name))

        ## Feature 1: making user name n-gram features

        name_gram<-unlist(lapply(sub$name, function(x) strsplit(gsub("[^[:alnum:] ]", "", x), " +")[[1]]))

        dat<-as.data.frame(table(name_gram))
        dat<-dat[which(dat$Freq>2),]
        colNames<-as.character(dat$name_gram)
        colNames<-colNames[-which(colNames=="")]
        colNames<-unlist(lapply(colNames, function(x) paste(x, "nameGram", sep="_")))

        sub[,c(colNames)]<-0

        for(d in (dim(sub)[2]-(length(colNames)-1)):dim(sub)[2]){
          word<-colnames(sub)[d]
          word<-gsub("_nameGram", "", word)
          sub[grep(word, sub$name, ignore.case = TRUE), d]<-1
        }

        for(d in (dim(sub)[2]-(length(colNames)-1)):dim(sub)[2]){
          word<-colnames(sub)[d]
          word<-gsub("_nameGram", "", word)
          sub[grep(word, sub$name, ignore.case = TRUE), d]<-1
        }

        ### Feature 2: making user name n-char features

        name_char<-unlist(lapply(sub$name, function(x) gsub(" ", "", x)))
        name_char<-unlist(lapply(name_char, function(x) unlist(strsplit(x, ""))))

        dat<-as.data.frame(table(name_char))
        dat<-dat[which(dat$Freq>2),]
        colNames<-as.character(dat$name_char)
        colNames<-unlist(lapply(colNames, function(x) paste(x, "nameChar", sep="_")))

        sub[,c(colNames)]<-0

        for(d in (dim(sub)[2]-(length(colNames)-1)):dim(sub)[2]){
          word<-colnames(sub)[d]
          word<-gsub("_nameChar", "", word)
          sub[grep(word, sub$name, fixed=TRUE), d]<-1
        }
        print(paste0("Features for chunk ", iter , " are made!"))

        ## Generate predictions
        sub_part <- sub[which(is.na(sub$gender_new)),]
        predictions <- predict(mod, sub_part)

        print(paste0("Predictions for chunk ", iter , " are made!"))

        results_part <- data.frame(id_str=as.character(sub_part$id_str), gender_prediction=as.character(predictions))

        results <- rbind(results, results_part)
      }
      return(results)
    }

  if(chunk == TRUE & action=="save"){

      index1 <- which(colnames(input)=="id_str")
      index2 <- which(colnames(input)=="name")

      gender_new  <-  rep(NA, dim(input)[1])
      input  <-  as.data.frame(cbind(input[,c(index1)], gender_new, input[,c(index2)]))
      names(input)  <-  c("id_str", "gender_new", "name")

      vec <- seq(1, nrow(input), by=10000)

      results <- NULL
      iter <- 0

      for(i in vec){

        iter <- iter+1

        sub <- input[c(i:(i+9999)),]

        if(length(which(is.na(sub$name)))>0){
          sub <- sub[-which(is.na(sub$name)),]
        }

        sub$name <- as.character(sub$name)

        #load("/DATA/gender_training_premade.rda")
        df <- df_gen

        sub <- rbind(df, sub)

        sub$name <- as.character(sub$name)
        sub$name <- unlist(lapply(sub$name, function(x) iconv(x,"UTF-8","ASCII")))
        sub$name <- unlist(lapply(sub$name, function(x) gsub("<", " <", x)))
        sub$name <- unlist(lapply(sub$name, function(x) gsub(">", "> ", x)))
        sub$name <- unlist(lapply(sub$name, function(x) str_replace_all(x,"\\<U[^\\>]*\\>","")))
        sub$name<-str_replace_all(sub$name, "[[:punct:]]", "")
        sub$name<-trim(tolower(sub$name))

        ## Feature 1: making user name n-gram features

        name_gram<-unlist(lapply(sub$name, function(x) strsplit(gsub("[^[:alnum:] ]", "", x), " +")[[1]]))

        dat<-as.data.frame(table(name_gram))
        dat<-dat[which(dat$Freq>2),]
        colNames<-as.character(dat$name_gram)
        colNames<-colNames[-which(colNames=="")]
        colNames<-unlist(lapply(colNames, function(x) paste(x, "nameGram", sep="_")))

        sub[,c(colNames)]<-0

        for(d in (dim(sub)[2]-(length(colNames)-1)):dim(sub)[2]){
        word<-colnames(sub)[d]
          word<-gsub("_nameGram", "", word)
          sub[grep(word, sub$name, ignore.case = TRUE), d]<-1
        }

        ### Feature 2: making user name n-char features

        name_char<-unlist(lapply(sub$name, function(x) gsub(" ", "", x)))
        name_char<-unlist(lapply(name_char, function(x) unlist(strsplit(x, ""))))

        dat<-as.data.frame(table(name_char))
        dat<-dat[which(dat$Freq>2),]
        colNames<-as.character(dat$name_char)
        colNames<-unlist(lapply(colNames, function(x) paste(x, "nameChar", sep="_")))

        sub[,c(colNames)]<-0

        for(d in (dim(sub)[2]-(length(colNames)-1)):dim(sub)[2]){
          word<-colnames(sub)[d]
          word<-gsub("_nameChar", "", word)
          sub[grep(word, sub$name, fixed=TRUE), d]<-1
        }
        print(paste0("Features for chunk ", iter , " are made!"))

        ## Generate predictions
        sub_part <- sub[which(is.na(sub$gender_new)),]
        predictions <- predict(mod, sub_part)

        print(paste0("Predictions for chunk ", iter , " are made!"))

        predictions<-as.character(predictions)

        results_part <- data.frame(id_str=as.character(sub_part$id_str), gender_prediction=as.character(predictions))

        write.csv(results_part, paste0(dir, "results_part_", iter, ".csv"), row.names=FALSE)
      }
    }
  }

  if(labeled == TRUE){

    if(is.null(input$name)){
      stop("Your input data is missing relevant field(s)")
    }

    df<-input
    print("Data are ready!")

    ## Feature 1: user name n-gram features

    name_gram<-unlist(lapply(df$name, function(x) strsplit(gsub("[^[:alnum:] ]", "", x), " +")[[1]]))

    dat<-as.data.frame(table(name_gram))
    dat<-dat[which(dat$Freq>2),]
    colNames<-as.character(dat$name_gram)
    colNames<-colNames[-which(colNames=="")]
    colNames<-unlist(lapply(colNames, function(x) paste(x, "nameGram", sep="_")))

    df[,c(colNames)]<-0

    for(i in (dim(df)[2]-(length(colNames)-1)):dim(df)[2]){
    word<-colnames(df)[i]
      word<-gsub("_nameGram", "", word)
      df[grep(word, df$name, ignore.case = TRUE), i]<-1
    }

    ### Feature 2: user name n-char features

    name_char<-unlist(lapply(df$name, function(x) gsub(" ", "", x)))
    name_char<-unlist(lapply(name_char, function(x) unlist(strsplit(x, ""))))

    dat<-as.data.frame(table(name_char))
    dat<-dat[which(dat$Freq>2),]
    colNames<-as.character(dat$name_char)
    colNames<-unlist(lapply(colNames, function(x) paste(x, "nameChar", sep="_")))

    df[,c(colNames)]<-0

    for(i in (dim(df)[2]-(length(colNames)-1)):dim(df)[2]){
      word<-colnames(df)[i]
      word<-gsub("_nameChar", "", word)
      df[grep(word, df$name, fixed=TRUE), i]<-1
    }

    print("Features are built!")

    df_part<-df[,c(2, 67:dim(df)[2])]

    df_part<-df_part[,-1]

    name_IDX<-grep("_name", colnames(df_part))

    ntrain <- round(dim(df_part)[1]*training_set)
    tindex <- sample(1:dim(df_part)[1],ntrain)

    train<-df_part[tindex,]
    test<-df_part[-tindex,]

    trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
    set.seed(seed)

    mod <- train(gender_new ~., data=train[,c(1, name_IDX)], method = "svmLinear", trControl=trctrl)

    print("Classifier is trained!")

    predictions <- predict(mod, test)
    predictions<-as.character(predictions)
    print("Predictions are made!")

    train$gender_prediction <- NA
    train$test_train <- "train"

    test$gender_prediction <- as.character(predictions)
    test$test_train <- "test"

    results <- rbind(train, test)

    return(results)
  }
}
